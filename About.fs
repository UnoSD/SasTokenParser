module SasTokenParser.About

open Fulma
open SasTokenParser.Helpers
open Fable.React

let private buttons =
    Card.footer []
                [ cardFooterLink "Contact" "https://github.com/UnoSD/SasTokenParser/issues"
                  cardFooterLink "GitHub"  "https://github.com/UnoSD"
                  cardFooterLink "Blog"    "https://dev.to/unosd"                           ]

let aboutCard _ _ =
    let modifiers =
        Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]

    card [ Hero.body []
                     [ Container.container [ Container.IsFluid
                                             modifiers ]
                     [ Heading.h1 [] [ str "SasTokenParser" ]
                       Heading.h4 [ Heading.IsSubtitle ] [ str "Pretty viewer of SAS tokens" ] ] ] ]
         buttons