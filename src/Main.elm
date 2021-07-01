module Main exposing (..)

import Browser
import Css exposing (Style, focus, fontFamilies, property, qt, sansSerif)
import Css.Global exposing (global, selector)
import Html.Styled exposing (Html, div, h3, input, node, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href, id, placeholder, rel, type_)
import Html.Styled.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline as Pip exposing (required)
import List.Extra
import Regex
import RemoteData exposing (RemoteData, WebData)
import Svg.Styled as Svg exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints exposing (lg)
import Tailwind.Utilities as TW


type Filter a
    = Posts a
    | NoActive
    | Empty


type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , body : String
    }


postDecoder : Decoder Post
postDecoder =
    Decode.succeed Post
        |> required "userId" int
        |> required "id" int
        |> required "title" string
        |> required "body" string


type alias User =
    { id : Int
    , name : String
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "id" int
        |> required "name" string


type alias Model =
    { posts : WebData (List Post)
    , users : WebData (List User)
    , filterText : String
    , filterPosts : Filter (List Post)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { posts = RemoteData.Loading
      , users = RemoteData.Loading
      , filterText = ""
      , filterPosts = NoActive
      }
    , Cmd.batch [ getPostsAction, getUsersAction ]
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = SendHttpRequest
    | DataReceivedPosts (WebData (List Post))
    | DataReceivedUsers (WebData (List User))
    | SaveFilter String
    | FilterPost String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( { model | posts = RemoteData.Loading }, getPostsAction )

        DataReceivedPosts response ->
            ( { model | posts = response }, Cmd.none )

        DataReceivedUsers response ->
            ( { model | users = response }, Cmd.none )

        SaveFilter text ->
            ( { model | filterText = text }, Cmd.none )

        FilterPost text ->
            let
                users =
                    searchUser text model.users

                filter =
                    filterPosts model.posts users

                result =
                    if List.isEmpty filter then
                        Empty

                    else
                        Posts filter
            in
            ( { model | filterPosts = result }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ node "link" [ href "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap", rel "stylesheet" ] []
        , globalCss
        , div
            [ css [ containerStyle ] ]
            [ div
                [ css
                    [ lg
                        [ TW.grid
                        , TW.grid_cols_4
                        , TW.gap_4
                        , TW.my_6
                        ]
                    ]
                ]
                [ div [ css [ TW.col_start_2, TW.col_end_4 ] ] [ viewFilter ]
                ]
            ]
        , div
            [ css [ containerStyle ] ]
            [ viewPostsOrError model ]
        ]


containerStyle : Css.Style
containerStyle =
    Css.batch
        [ TW.container
        , TW.mx_auto
        , TW.px_4
        , TW.box_content
        ]


viewFilter : Html Msg
viewFilter =
    div
        [ css
            [ TW.shadow
            , TW.appearance_none
            , TW.border
            , TW.rounded
            , TW.text_gray_700
            , TW.leading_tight
            , TW.flex
            , TW.bg_white
            , TW.divide_x
            , TW.divide_gray_50
            , TW.overflow_hidden
            ]
        ]
        [ div
            [ css
                [ TW.flex
                , TW.p_4
                , TW.text_gray_400
                ]
            ]
            [ iconSearch ]
        , input
            [ css
                [ TW.w_full
                , TW.px_2
                , TW.border_0
                , fontFamilies [ qt "Roboto", .value sansSerif ]
                , TW.text_sm
                , TW.leading_normal
                , focus [ TW.outline_none ]
                ]
            , placeholder "Filter by author"
            , id "filter"
            , type_ "text"
            , onInput FilterPost
            ]
            []
        ]


iconSearch : Html msg
iconSearch =
    svg [ SvgAttr.width "16", SvgAttr.height "16", SvgAttr.fill "currentColor", SvgAttr.class "bi bi-search", SvgAttr.viewBox "0 0 16 16" ] [ Svg.path [ SvgAttr.d "M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001c.03.04.062.078.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1.007 1.007 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z" ] [] ]


globalCss : Html.Styled.Html msg
globalCss =
    global
        [ selector "body" [ TW.bg_blue_50, TW.overflow_x_hidden, fontFamilies [ qt "Roboto", .value sansSerif ] ] ]


searchUser : String -> WebData (List User) -> List User
searchUser text users =
    let
        regex t =
            Maybe.withDefault Regex.never <| Regex.fromString t

        isUser : User -> Bool
        isUser i =
            Regex.contains (regex text) i.name
    in
    case users of
        RemoteData.Success list ->
            List.filter isUser list

        _ ->
            []


filterPosts : WebData (List Post) -> List User -> List Post
filterPosts posts users =
    let
        isUsers : Post -> User -> Post
        isUsers post user =
            if post.userId == user.id then
                post

            else
                Post -1 -1 "" ""
    in
    case posts of
        RemoteData.Success listPost ->
            List.filter (\i -> not <| i.id == -1) <| List.Extra.lift2 isUsers listPost users

        _ ->
            []


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.posts of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Загрузка..." ]

        RemoteData.Success posts ->
            case model.filterPosts of
                Posts a ->
                    viewPosts a model.users

                NoActive ->
                    viewPosts posts model.users

                Empty ->
                    text "No records"

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)


viewUserNameOrError : WebData (List User) -> Int -> Html Msg
viewUserNameOrError listUsers idUser =
    case listUsers of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Загрузка..." ]

        RemoteData.Success users ->
            let
                findUser =
                    List.Extra.find (\i -> i.id == idUser) users
            in
            case findUser of
                Just user ->
                    text user.name

                Nothing ->
                    text "No find user"

        RemoteData.Failure httpError ->
            viewError (buildErrorMessage httpError)


viewPosts : List Post -> WebData (List User) -> Html Msg
viewPosts posts users =
    let
        beforeAndAfter : Style
        beforeAndAfter =
            Css.batch
                [ property "content" "''"
                , property "flex-basis" "100%"
                , TW.w_0
                , TW.order_2
                ]
    in
    div
        [ css
            [ TW.grid
            , TW.gap_4
            , TW.mb_4
            , lg
                [ TW.grid_cols_3
                ]
            ]
        ]
        (List.map (viewPost users) posts)


viewPost : WebData (List User) -> Post -> Html Msg
viewPost users post =
    div
        [ css
            [ TW.shadow
            , TW.border
            , TW.rounded
            , TW.p_4
            , TW.bg_white
            ]
        ]
        [ div [ css [ TW.text_2xl, TW.font_bold, TW.text_blue_400 ] ] [ text post.title ]
        , div [ css [ TW.my_2, TW.text_lg ] ] [ text post.body ]
        , div [ css [ TW.text_sm, TW.text_gray_400 ] ] [ viewUserNameOrError users post.userId ]
        ]


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]


getPostsAction : Cmd Msg
getPostsAction =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "https://jsonplaceholder.typicode.com/posts"
        , body = Http.emptyBody
        , expect =
            list postDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataReceivedPosts)
        , timeout = Nothing
        , tracker = Nothing
        }


getUsersAction : Cmd Msg
getUsersAction =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/json" ]
        , url = "https://jsonplaceholder.typicode.com/users"
        , body = Http.emptyBody
        , expect =
            list userDecoder
                |> Http.expectJson (RemoteData.fromResult >> DataReceivedUsers)
        , timeout = Nothing
        , tracker = Nothing
        }


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
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
