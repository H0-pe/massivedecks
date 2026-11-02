module MassiveDecks.Requests.Api exposing
    ( checkAlive
    , joinLobby
    , lobbySummaries
    , deletePreset
    , newLobby
    , presets
    , savePresets
    , serverConfig
    )

import Http
import Json.Decode as Json
import Json.Encode
import MassiveDecks.Card.Source.Model as Source
import MassiveDecks.Error.Model as Error
import MassiveDecks.Models.Decoders as Decoders
import MassiveDecks.Models.Encoders as Encoders
import MassiveDecks.Models.MdError exposing (MdError)
import MassiveDecks.Pages.Lobby.GameCode as GameCode exposing (GameCode)
import MassiveDecks.Pages.Lobby.Model as Lobby
import MassiveDecks.Pages.Lobby.Token as Token
import MassiveDecks.Pages.Start.LobbyBrowser.Model as LobbyBrowser
import MassiveDecks.Pages.Start.Model as Start
import MassiveDecks.Requests.Request as Request exposing (Request)
import MassiveDecks.ServerConfig exposing (ServerConfig)
import MassiveDecks.Settings.Model exposing (Preset)
import MassiveDecks.User as User
import MassiveDecks.Util.Result as Result
import Url
import Url.Builder


{-| List the public lobbies.
-}
lobbySummaries : (Request.Response Never (List LobbyBrowser.Summary) -> msg) -> Request msg
lobbySummaries msg =
    { method = "GET"
    , headers = []
    , url = url [ "games" ]
    , body = Http.emptyBody
    , expect = Request.expectResponse msg noError (Json.list Decoders.lobbySummary)
    , timeout = Nothing
    , tracker = Nothing
    }


{-| Create a new lobby.
-}
newLobby : (Request.Response Never Lobby.Auth -> msg) -> Start.LobbyCreation -> Request msg
newLobby msg creation =
    { method = "POST"
    , headers = []
    , url = url [ "games" ]
    , body = creation |> Encoders.lobbyCreation |> Http.jsonBody
    , expect = Request.expectResponse (decodeToken >> msg) noError Decoders.lobbyToken
    , timeout = Nothing
    , tracker = Nothing
    }


{-| Join a lobby.
-}
joinLobby : (Request.Response MdError Lobby.Auth -> msg) -> GameCode -> User.Registration -> Request msg
joinLobby msg gameCode registration =
    { method = "POST"
    , headers = []
    , url = url [ "games", gameCode |> GameCode.toString ]
    , body = registration |> Encoders.userRegistration |> Http.jsonBody
    , expect = Request.expectResponse (decodeToken >> msg) Decoders.mdError Decoders.lobbyToken
    , timeout = Nothing
    , tracker = Nothing
    }


{-| Check if previously joined lobbies are still going.
-}
checkAlive : (Request.Response Never (List Lobby.Token) -> msg) -> List Lobby.Token -> Request msg
checkAlive msg tokens =
    { method = "POST"
    , headers = []
    , url = url [ "alive" ]
    , body = tokens |> Encoders.checkAlive |> Http.jsonBody
    , expect = Request.expectResponse msg noError Decoders.tokenValidity
    , timeout = Nothing
    , tracker = Nothing
    }


{-| Get the presets saved on the server. Only someone in a lobby can.
-}
presets : (Request.Response Never (List Preset) -> msg) -> Lobby.Token -> Request msg
presets msg token =
    presetRequest msg token "GET" [] Http.emptyBody


{-| Save presets on the server, replacing any with the same names. Responds with all presets.
-}
savePresets : (Request.Response Never (List Preset) -> msg) -> Lobby.Token -> List Preset -> Request msg
savePresets msg token toSave =
    presetRequest msg token "POST" [] (toSave |> Json.Encode.list Encoders.preset |> Http.jsonBody)


{-| Delete a preset from the server. Responds with the remaining presets.
-}
deletePreset : (Request.Response Never (List Preset) -> msg) -> Lobby.Token -> String -> Request msg
deletePreset msg token name =
    presetRequest msg token "DELETE" [ Url.percentEncode name ] Http.emptyBody


{-| Get configuration from the server.
-}
serverConfig : (Request.Response Never ServerConfig -> msg) -> Request msg
serverConfig msg =
    { method = "GET"
    , headers = []
    , url = url [ "config" ]
    , body = Http.emptyBody
    , expect = Request.expectResponse msg noError Decoders.serverConfig
    , timeout = Nothing
    , tracker = Nothing
    }



{- Private -}


decodeToken : Request.Response error Lobby.Token -> Request.Response error Lobby.Auth
decodeToken =
    Request.map
        Request.GeneralError
        Request.SpecificError
        (Token.decode >> Result.unifiedMap (Error.Token >> Request.GeneralError) Request.Value)


presetRequest : (Request.Response Never (List Preset) -> msg) -> Lobby.Token -> String -> List String -> Http.Body -> Request msg
presetRequest msg token method path body =
    { method = method
    , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
    , url = url ("presets" :: path)
    , body = body
    , expect = Request.expectResponse msg noError Decoders.presets
    , timeout = Nothing
    , tracker = Nothing
    }


url : List String -> String
url path =
    Url.Builder.absolute ("api" :: path) []


noError : Json.Decoder Never
noError =
    Json.fail "No specific errors are expected for this request."
