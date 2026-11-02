module MassiveDecks.Pages.Lobby.Configure.Messages exposing (Msg(..), PresetScope(..))

import MassiveDecks.Pages.Lobby.Configure.Decks.Model as Decks
import MassiveDecks.Pages.Lobby.Configure.Model exposing (..)
import MassiveDecks.Requests.Request as Request
import MassiveDecks.Settings.Model exposing (Preset)


type Msg
    = ApplyChange Id Config
    | SetPresetName String
    | SavePreset
    | ApplyPreset PresetScope Preset
    | DeletePreset String
    | ReceivePresets (Request.Response Never (List Preset))
    | DecksMsg Decks.Msg
    | ChangeTab (Maybe Tab)
    | StartGame
    | ResolveConflict Source Id
    | SetPasswordVisibility Bool
    | SaveChanges
    | RevertChanges
    | NoOp


{-| How much of a preset to apply: everything (replacing the decks), or only add its decks.
-}
type PresetScope
    = WholeGame
    | DecksOnly
