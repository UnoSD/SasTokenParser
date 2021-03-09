module SasTokenParser.View

open SasTokenParser.Tabs
open SasTokenParser.Page
open Feliz

let view model dispatch =
    Html.div [ tabs model dispatch
               page model dispatch ]