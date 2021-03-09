module SasTokenParser.Page

open SasTokenParser.Models
open SasTokenParser.ParserCard
open SasTokenParser.About

let page model =
    model |>
    match model.CurrentTab with
    | Parser -> parserCard
    | About  -> aboutCard