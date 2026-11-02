module MassiveDecks.Pages.Lobby.Configure.Presets exposing
    ( fromConfig
    , patch
    , view
    )

{-| Presets: saved lobby configurations (rules, time limits, privacy and decks) that can be applied to a game.
They are stored in the user's local storage alongside the other settings.
-}

import FontAwesome as Icon
import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Diff as Json
import Json.Encode
import Json.Patch as Json
import MassiveDecks.Card.Source as Source
import MassiveDecks.Icon as Icon
import MassiveDecks.Model exposing (..)
import MassiveDecks.Models.Encoders as Encoders
import MassiveDecks.Pages.Lobby.Configure.Decks.Model as Decks
import MassiveDecks.Pages.Lobby.Configure.Messages exposing (..)
import MassiveDecks.Pages.Lobby.Configure.Model exposing (Config, Model)
import MassiveDecks.Settings.Model exposing (Preset)
import MassiveDecks.Strings as Strings
import MassiveDecks.Strings.Languages as Lang
import MassiveDecks.Util.Maybe as Maybe
import Material.Button as Button
import Material.IconButton as IconButton
import Material.TextField as TextField


{-| Capture the given configuration as a preset with the given name.
-}
fromConfig : String -> Config -> Preset
fromConfig name config =
    { name = name
    , rules = config.rules
    , public = config.privacy.public
    , audienceMode = config.privacy.audienceMode
    , decks = config.decks |> List.map .source
    }


{-| The patch that applies the given preset to the given configuration, if anything would change.
Applying the whole game replaces rules and privacy settings (except the password), and makes the decks match the
preset's decks. Applying decks only adds the preset's decks that aren't already in the game.
-}
patch : PresetScope -> Preset -> Config -> Maybe Json.Patch
patch scope preset config =
    let
        existing =
            config.decks |> List.map .source

        inPreset source =
            preset.decks |> List.any (Source.equals source)

        missing =
            preset.decks |> List.filter (\s -> existing |> List.any (Source.equals s) |> not)

        adds =
            missing
                |> List.map (\s -> Json.Add [ "decks", "-" ] (Decks.DeckOrError s (Ok Nothing) |> Encoders.deckOrError))

        ( settingsPatch, removes ) =
            case scope of
                WholeGame ->
                    let
                        privacy =
                            config.privacy

                        target =
                            { config
                                | rules = preset.rules
                                , privacy = { privacy | public = preset.public, audienceMode = preset.audienceMode }
                            }

                        -- Remove from the end so earlier indices stay valid.
                        toRemove =
                            existing
                                |> List.indexedMap Tuple.pair
                                |> List.filter (Tuple.second >> inPreset >> not)
                                |> List.map Tuple.first
                                |> List.reverse
                    in
                    ( Json.diff (config |> Encoders.config) (target |> Encoders.config)
                    , toRemove |> List.map (\i -> Json.Remove [ "decks", String.fromInt i ])
                    )

                DecksOnly ->
                    ( [], [] )

        changes =
            settingsPatch ++ removes ++ adds
    in
    if List.isEmpty changes then
        Nothing

    else
        Json.Test [ "version" ] (config.version |> Json.Encode.string) :: changes |> Just


{-| View the presets tab.
-}
view : (Msg -> msg) -> Shared -> Model -> Config -> Bool -> Html msg
view wrap shared model remote canEdit =
    let
        presets =
            shared.settings.settings.presets

        trimmedName =
            String.trim model.presetName

        save =
            SavePreset |> wrap |> Maybe.justIf (trimmedName /= "")

        hasUnsavedChanges =
            model.localConfig /= remote

        canApply =
            canEdit && not hasUnsavedChanges

        unsavedWarning =
            if canEdit && hasUnsavedChanges then
                Html.p [ HtmlA.class "preset-warning" ]
                    [ Icon.view Icon.warning, Html.text " ", Strings.PresetNeedsSavedChanges |> Lang.html shared ]

            else
                Html.text ""

        list =
            if List.isEmpty presets then
                Html.p [ HtmlA.class "empty-info" ] [ Strings.NoPresets |> Lang.html shared ]

            else
                Html.ul [ HtmlA.class "preset-list" ] (presets |> List.map (viewPreset wrap shared canApply))
    in
    Html.div [ HtmlA.id "presets-tab" ]
        [ Html.h3 [] [ Strings.ConfigurePresets |> Lang.html shared ]
        , Html.p [ HtmlA.class "description" ] [ Strings.PresetsDescription |> Lang.html shared ]
        , Html.div [ HtmlA.class "save-preset" ]
            [ TextField.viewWithReturn
                (Strings.PresetNameLabel |> Lang.string shared)
                TextField.Text
                model.presetName
                (SetPresetName >> wrap |> Just)
                (save |> Maybe.withDefault (wrap NoOp))
                (wrap NoOp)
            , Button.view
                Button.Raised
                Button.Padded
                (Strings.SavePreset |> Lang.string shared)
                (Icon.save |> Icon.view |> Just)
                save
            ]
        , unsavedWarning
        , list
        ]


viewPreset : (Msg -> msg) -> Shared -> Bool -> Preset -> Html msg
viewPreset wrap shared canApply preset =
    let
        applyButton scope label icon =
            Button.view
                Button.Outlined
                Button.Dense
                (label |> Lang.string shared)
                (icon |> Icon.view |> Just)
                (ApplyPreset scope preset |> wrap |> Maybe.justIf canApply)
    in
    Html.li [ HtmlA.class "preset" ]
        [ Html.div [ HtmlA.class "preset-info" ]
            [ Html.span [ HtmlA.class "preset-name" ] [ Html.text preset.name ]
            , Html.span [ HtmlA.class "preset-details" ]
                [ Strings.PresetDeckCount { decks = List.length preset.decks } |> Lang.html shared ]
            ]
        , Html.div [ HtmlA.class "preset-actions" ]
            [ applyButton WholeGame Strings.ApplyPresetWholeGame Icon.accept
            , applyButton DecksOnly Strings.ApplyPresetDecksOnly Icon.add
            , IconButton.view
                (Icon.remove |> Icon.view)
                (Strings.DeletePreset |> Lang.string shared)
                (DeletePreset preset.name |> wrap |> Just)
            ]
        ]
