module TopStories exposing (main)

import Array
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (href)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import RemoteData exposing (WebData)
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query


type alias TopStory =
    { id : Int
    , title : String
    , url : String
    }


type alias Model =
    { topStoryIdsWebData : WebData (List Int)
    , pageNumber : Int
    , storiesPerPage : Int
    , pagesTopStoriesWebData : List (WebData TopStory)
    , navKey : Nav.Key
    }


type Msg
    = GotTopStoryIds (WebData (List Int))
    | GotTopStory (WebData TopStory)
    | LinkClicked UrlRequest
    | UrlChanged Url


storiesPerPage : Int
storiesPerPage =
    25


numberOfTopStories : Int
numberOfTopStories =
    500


firstPageNumber : Int
firstPageNumber =
    1


lastPageNumber : Int
lastPageNumber =
    numberOfTopStories // storiesPerPage


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    ( { topStoryIdsWebData = RemoteData.Loading
      , pageNumber = pageNumberFromUrl url
      , storiesPerPage = storiesPerPage
      , pagesTopStoriesWebData = []
      , navKey = navKey
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
            gotTopStoryIds model topStoryIdsWebData

        GotTopStory topStoryWebData ->
            case topStoryWebData of
                RemoteData.Success _ ->
                    ( { model
                        | pagesTopStoriesWebData =
                            List.append model.pagesTopStoriesWebData [ topStoryWebData ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model
                        | pageNumber = pageNumberFromUrl url
                        , pagesTopStoriesWebData = []
                      }
                    , Cmd.batch
                        [ Nav.pushUrl model.navKey <| Url.toString url
                        , getTopStoryIds
                        ]
                    )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            ( model, Cmd.none )


gotTopStoryIds : Model -> WebData (List Int) -> ( Model, Cmd Msg )
gotTopStoryIds model topStoryIdsWebData =
    case topStoryIdsWebData of
        RemoteData.Success topStoryIds ->
            let
                currentPagesStoryIds =
                    pagesStoryIds model.pageNumber model.storiesPerPage topStoryIds

                getCurrentPageStories =
                    Cmd.batch (List.map getTopStory currentPagesStoryIds)
            in
            ( { model | topStoryIdsWebData = topStoryIdsWebData }
            , getCurrentPageStories
            )

        _ ->
            ( model, Cmd.none )


pagesStoryIds page perPage topStoryIds =
    let
        firstStorysPositionInArray =
            (page - 1) * perPage

        lastStorysPositionInArray =
            firstStorysPositionInArray + perPage
    in
    Array.fromList topStoryIds
        |> Array.slice firstStorysPositionInArray lastStorysPositionInArray
        |> Array.toList


pageNumberFromUrl : Url -> Int
pageNumberFromUrl url =
    let
        maybeMaybePageNumber =
            Url.Parser.parse (Url.Parser.query pageQueryParamParser) url
    in
    case maybeMaybePageNumber of
        Just (Just pageNum) ->
            if pageNum < firstPageNumber then
                firstPageNumber

            else if pageNum > lastPageNumber then
                lastPageNumber

            else
                pageNum

        _ ->
            firstPageNumber


pageQueryParamParser : Url.Parser.Query.Parser (Maybe Int)
pageQueryParamParser =
    Url.Parser.Query.int "p"


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


view : Model -> Document Msg
view model =
    { title = "Hacker News"
    , body = [ currentView model ]
    }


nextPageLinkLocation : Int -> String
nextPageLinkLocation currentPageNumber =
    let
        nextPageNumber =
            min (currentPageNumber + 1) lastPageNumber
    in
    "/?p=" ++ String.fromInt nextPageNumber


previousPageLinkLocation : Int -> String
previousPageLinkLocation currentPageNumber =
    let
        previousPageNumber =
            max (currentPageNumber - 1) firstPageNumber
    in
    "/?p=" ++ String.fromInt previousPageNumber


currentView : Model -> Html Msg
currentView model =
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
            div []
                (List.map (viewTopStory model.pageNumber model.storiesPerPage) sortedTopStoriesWithIndecies
                    ++ [ div [] [ text ("Page " ++ String.fromInt model.pageNumber) ] ]
                    ++ pageNavigationLinks model.pageNumber
                )

        Errored message ->
            text message


pageNavigationLinks pageNumber =
    let
        nextPageLink =
            [ a [ href (nextPageLinkLocation pageNumber) ] [ text "Next Page" ] ]

        previousPageLink =
            [ a [ href (previousPageLinkLocation pageNumber) ] [ text "Previous Page" ] ]
    in
    if pageNumber == firstPageNumber then
        nextPageLink

    else if pageNumber == lastPageNumber then
        previousPageLink

    else
        previousPageLink ++ [ text " " ] ++ nextPageLink


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

                firstListItem :: remainingListItems ->
                    if firstListItem == item then
                        Just index

                    else
                        indexOf2 remainingListItems (index + 1)
    in
    indexOf2 listOfItems 0


viewTopStory : Int -> Int -> ( Int, WebData TopStory ) -> Html Msg
viewTopStory page perPage ( topStoryIndex, topStoryWebData ) =
    case topStoryWebData of
        RemoteData.Success topStory ->
            let
                listPositionPrefix =
                    String.fromInt (((page - 1) * perPage) + topStoryIndex + 1) ++ ". "

                linkAddress =
                    if String.length topStory.url > 0 then
                        topStory.url

                    else
                        "https://news.ycombinator.com/item?id=" ++ String.fromInt topStory.id
            in
            div []
                [ text listPositionPrefix
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
    Browser.application
        { init = init
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
