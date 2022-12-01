module TopStories exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import RemoteData exposing (WebData)


type alias TopStory =
    { id : Int
    , title : String
    , url : String
    }


type alias Model =
    { topStoryIdsWebData : WebData (List Int)
    , storiesPerPage : Int
    , pagesTopStoriesWebData : List (WebData TopStory)
    }


type Msg
    = GotTopStoryIds (WebData (List Int))
    | GotTopStory (WebData TopStory)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topStoryIdsWebData = RemoteData.Loading
      , storiesPerPage = 30
      , pagesTopStoriesWebData = []
      }
    , getTopStoryIds
    )


getTopStoryIds : Cmd Msg
getTopStoryIds =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json"
        , expect = Http.expectJson (RemoteData.fromResult >> GotTopStoryIds) (list int)
        }


getTopStory : Int -> Cmd Msg
getTopStory storyId =
    Http.get
        { url =
            "https://hacker-news.firebaseio.com/v0/item/"
                ++ String.fromInt storyId
                ++ ".json"
        , expect = Http.expectJson (RemoteData.fromResult >> GotTopStory) topStoryDecoder
        }


topStoryDecoder : Decode.Decoder TopStory
topStoryDecoder =
    Decode.succeed TopStory
        |> required "id" int
        |> required "title" string
        |> optional "url" string ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTopStoryIds topStoryIdsWebData ->
            case topStoryIdsWebData of
                RemoteData.Success topStoryIds ->
                    let
                        updatedModel =
                            { model | topStoryIdsWebData = topStoryIdsWebData }

                        pagesStoryIds =
                            List.take model.storiesPerPage topStoryIds

                        getTopStories =
                            Cmd.batch (List.map getTopStory pagesStoryIds)
                    in
                    ( updatedModel
                    , getTopStories
                    )

                _ ->
                    ( model, Cmd.none )

        GotTopStory topStoryWebData ->
            case topStoryWebData of
                RemoteData.Success _ ->
                    let
                        topStoriesWebData =
                            List.append model.pagesTopStoriesWebData [ topStoryWebData ]
                    in
                    ( { model | pagesTopStoriesWebData = topStoriesWebData }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


type PageLoadState
    = Loading
    | Errored String
    | Loaded


httpErrorMessage : Http.Error -> String
httpErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


successfulHttpRequest : WebData a -> Bool
successfulHttpRequest webData =
    case webData of
        RemoteData.Success _ ->
            True

        _ ->
            False


modelToPageLoadState : Model -> PageLoadState
modelToPageLoadState model =
    case model.topStoryIdsWebData of
        RemoteData.NotAsked ->
            Loading

        RemoteData.Loading ->
            Loading

        RemoteData.Failure message ->
            Errored (httpErrorMessage message)

        RemoteData.Success _ ->
            if
                List.length model.pagesTopStoriesWebData
                    == model.storiesPerPage
                    && List.all successfulHttpRequest model.pagesTopStoriesWebData
            then
                Loaded

            else
                Loading


view : Model -> Html Msg
view model =
    let
        sortedTopStories =
            List.sortWith
                (sortTopStories model.topStoryIdsWebData)
                model.pagesTopStoriesWebData

        sortedTopStoriesWithIndecies =
            List.indexedMap Tuple.pair sortedTopStories
    in
    case modelToPageLoadState model of
        Loading ->
            text "Loading..."

        Loaded ->
            div [] (List.map viewTopStory sortedTopStoriesWithIndecies)

        Errored message ->
            text message


sortTopStories : WebData (List Int) -> WebData TopStory -> WebData TopStory -> Order
sortTopStories topStoryIdsWebData topStory1WebData topStory2WebData =
    case ( topStoryIdsWebData, topStory1WebData, topStory2WebData ) of
        ( RemoteData.Success topStoryIds, RemoteData.Success topStory1, RemoteData.Success topStory2 ) ->
            case ( indexOf topStory1.id topStoryIds, indexOf topStory2.id topStoryIds ) of
                ( Just topStory1IdIndex, Just topStory2IdIndex ) ->
                    if topStory1IdIndex > topStory2IdIndex then
                        GT

                    else
                        LT

                _ ->
                    EQ

        _ ->
            EQ


indexOf : a -> List a -> Maybe Int
indexOf item listOfItems =
    let
        indexOf2 list index =
            case list of
                [] ->
                    Nothing

                listHead :: listTail ->
                    if listHead == item then
                        Just index

                    else
                        indexOf2 listTail (index + 1)
    in
    indexOf2 listOfItems 0


viewTopStory : ( Int, WebData TopStory ) -> Html Msg
viewTopStory ( topStoryIndex, topStoryWebData ) =
    case topStoryWebData of
        RemoteData.Success topStory ->
            let
                indexPrefix =
                    String.fromInt (topStoryIndex + 1) ++ ". "

                linkAddress =
                    if String.length topStory.url > 0 then
                        topStory.url

                    else
                        "https://news.ycombinator.com/item?id=" ++ String.fromInt topStory.id
            in
            div []
                [ text indexPrefix
                , a [ href linkAddress ] [ text <| htmlDecode topStory.title ]
                ]

        _ ->
            text ""


htmlDecode : String -> String
htmlDecode stringWithEscapedCharacters =
    let
        replace ( s1, s2 ) src =
            String.join s2 <| String.split s1 src

        chrmap =
            [ ( "&#x27;", "'" )
            ]
    in
    List.foldl replace stringWithEscapedCharacters chrmap


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
