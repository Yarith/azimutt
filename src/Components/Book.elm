module Components.Book exposing (DocState, main)

import Components.Atoms.Badge as Badge
import Components.Atoms.Button as Button
import Components.Atoms.Dots as Dots
import Components.Atoms.Icon as Icon
import Components.Atoms.Link as Link
import Components.Atoms.Markdown as Markdown
import Components.Molecules.Dropdown as Dropdown
import Components.Molecules.Feature as Feature
import Components.Molecules.Modal as Modal
import Components.Organisms.Footer as Footer
import Components.Organisms.Header as Header
import Components.Slices.Blog as Blog
import Components.Slices.Content as Content
import Components.Slices.Cta as Cta
import Components.Slices.FeatureGrid as FeatureGrid
import Components.Slices.FeatureSideBySide as FeatureSideBySide
import Components.Slices.Hero as Hero
import Components.Slices.Newsletter as Newsletter
import Css.Global as Global
import ElmBook
import ElmBook.Chapter as Chapter
import ElmBook.ComponentOptions
import ElmBook.ElmCSS as ElmCSS
import ElmBook.StatefulOptions
import ElmBook.ThemeOptions
import Html.Styled exposing (Html, div, img, table, td, text, th, tr)
import Html.Styled.Attributes exposing (alt, css, src, style)
import Libs.Models.TwColor as TwColor exposing (TwColor(..), TwColorLevel(..))
import Tailwind.Utilities exposing (globalStyles, h_12, p_3)


type alias DocState =
    { dropdownDocState : Dropdown.DocState
    , modalDocState : Modal.DocState
    , headerDocState : Header.DocState
    }


init : DocState
init =
    { dropdownDocState = Dropdown.initDocState
    , modalDocState = Modal.initDocState
    , headerDocState = Header.initDocState
    }


main : ElmCSS.Book DocState
main =
    ElmCSS.book "Azimutt Design System"
        |> ElmBook.withThemeOptions
            [ ElmBook.ThemeOptions.subtitle "v0.1.0"
            , ElmBook.ThemeOptions.globals [ Global.global globalStyles ]
            , ElmBook.ThemeOptions.logo (img [ src "/logo.svg", alt "Azimutt logo", css [ h_12 ] ] [])
            ]
        |> ElmBook.withComponentOptions [ ElmBook.ComponentOptions.fullWidth True ]
        |> ElmBook.withStatefulOptions [ ElmBook.StatefulOptions.initialState init ]
        |> ElmBook.withChapterGroups
            -- sorted alphabetically
            [ ( "", [ docs ] )
            , ( "Atoms", [ Badge.doc, Button.doc, colorsDoc, Dots.doc, Icon.doc, Link.doc, Markdown.doc ] )
            , ( "Molecules", [ Dropdown.doc, Feature.doc, Modal.doc ] )
            , ( "Organisms", [ Footer.doc, Header.doc ] )
            , ( "Slices", [ Blog.doc, Content.doc, Cta.doc, FeatureGrid.doc, FeatureSideBySide.doc, Hero.doc, Newsletter.doc ] )
            ]


docs : ElmCSS.Chapter x
docs =
    Chapter.chapter "Readme" |> Chapter.render """

work in progress
---
"""


colorsDoc : ElmCSS.Chapter x
colorsDoc =
    Chapter.chapter "Colors"
        |> Chapter.renderComponent
            (div []
                [ table []
                    (tr [] (th [] [] :: (TwColor.levels |> List.map (\l -> th [] [ text (TwColor.levelToString l) ])))
                        :: (TwColor.colors |> List.map (\c -> tr [] (th [] [ text (TwColor.colorToString c) ] :: (TwColor.levels |> List.map (viewColorCell c)))))
                    )
                ]
            )


viewColorCell : TwColor -> TwColorLevel -> Html msg
viewColorCell c l =
    td [ style "background-color" (TwColor.toHex l c), css [ p_3 ] ] [ text (TwColor.toHex l c) ]
