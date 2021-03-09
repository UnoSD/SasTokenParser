module SasTokenParser.Helpers

open Fable.React.Props
open Fable.React
open Fulma

let card content footer =
    Card.card [ ]
              [ Card.content [ ]
                             [ Content.content [] content
                               footer ] ]
              
let cardFooterLink text url =
    Card.Footer.a [ Props [ Href url
                            Target "_blank" ] ]
                  [ str text ]