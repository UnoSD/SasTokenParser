module SasTokenParser.ParserCard

open System
open Fable.FontAwesome
open Fable.React
open Feliz
open Fulma
open SasTokenParser.Helpers
open SasTokenParser.Models
open SasTokenParser.Parser
open SasTokenParser.Messages

module Icon = Free.Fa.Solid


let private iconField (labelText : string) icon input =
    Field.div [ ]
              [ Label.label [] [ Html.text labelText ]
                Control.div [ Control.HasIconLeft ]
                            [ input
                              Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                                        [ Fa.i [ icon ] [ ] ] ] ]

let private urlField model dispatch label icon =
    iconField label icon (Input.text [
        Input.Value model.Url
        Input.OnChange (fun ev -> ev.Value |> UrlChanged |> dispatch)
    ])

let private multilineStringToHtml (text : string) =
    text.Split('\n') |>
    Array.fold (fun acc line -> Html.text line :: Html.br [] :: acc) List.empty

let private row (parameter : string) (readableValue : Result<string, string>) (fieldName : string) (value : string) =
    [
        Html.tableCell [ Html.strong [ Html.text parameter ] ]
        Html.tableCell [ Html.div [
            match readableValue with
            | Ok readableValue -> yield! multilineStringToHtml readableValue
            | Error error      -> Html.strong [ prop.style [ style.color.red ]; prop.children [ Html.text error ] ]
        ] ]
        Html.tableCell [ Html.text fieldName ]
        Html.tableCell [ Html.text value ]
    ] |>
    Html.tableRow

let private tryParseUrl url =
    try
        Uri(url, UriKind.Absolute) |> Some
    with
    | _ -> None

let private card content =
    card content Html.none

let private th (text : string) =
    Html.tableHeader [ Html.text text ]

let private tableHeader (content : ReactElement list) =
    Html.thead [ Html.tableRow content ]

let private disclaimer (title : string) (text : string) dispatch =
    Message.message [ Message.Color IsWarning
                      Message.Size Size.IsSmall ]
                    [ Message.header [ ]
                                     [ Html.text title
                                       Delete.delete [ Delete.OnClick (fun _ -> CloseDisclaimer |> dispatch) ] [] ]
                      Message.body [ ]
                                   [ Html.text text ] ]


let private urlToRows url =
    createRowInfos url |>
    List.choose (fun x -> match x.Value with
                          | Some y -> Some <| row x.Parameter y.Parsed x.FieldName y.Source
                          | None   -> None)

let private rows url =
    tryParseUrl url |>
    Option.map urlToRows |>
    Option.defaultValue [ row "Type" (Error "Invalid URL") "URL" "" ]

let parserCard model dispatch =
    let urlField =
        urlField model dispatch
    
    let disclaimer title text =
        if   model.DisclaimerVisible
        then disclaimer title text dispatch
        else Html.none
    
    card [
        Html.form [
            disclaimer "Note"
                       "This page does not store or share URLs or any extracted data, all is processed within the browser boundaries"
            
            urlField "Insert URL to parse (or add to page URL as fragment: https://...#<SAS URL>)" Icon.Link
            
            Html.table [
                tableHeader [
                    th "Parameter"
                    th "Readable value"
                    th "Field name"
                    th "Value"
                ]
                Html.tableBody [
                    yield! rows model.Url
                ]
            ]
        ]
    ]