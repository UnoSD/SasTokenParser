module SasTokenParser.ParserCard

open System
open Elmish
open Fable.FontAwesome
open Fable.React
open Feliz
open Fulma
open SasTokenParser.Helpers
open SasTokenParser.Models
open SasTokenParser.Messages

module Icon = Free.Fa.Solid

let private s = UrlParser.s
let private str = UrlParser.str
let private map = UrlParser.map

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

let private row (parameter : string) (readableValue : string option) (fieldName : string) (value : string option) =
    match readableValue with
    | None
        -> Html.none
    | Some readableValue
        -> [
               Html.tableCell [ Html.strong [ Html.text parameter ] ]
               Html.tableCell [ Html.div [ yield! multilineStringToHtml readableValue ] ]
               Html.tableCell [ Html.text fieldName ]
               Html.tableCell [ Html.text (value |> Option.defaultValue "") ]
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

let disclaimer (title : string) (text : string) dispatch =
    Message.message [ Message.Color IsWarning
                      Message.Size Size.IsSmall ]
                    [ Message.header [ ]
                                     [ Html.text title
                                       Delete.delete [ Delete.OnClick (fun _ -> CloseDisclaimer |> dispatch) ] [] ]
                      Message.body [ ]
                                   [ Html.text text ] ]

let private cloudMap =
    Map.empty.Add ("windows", "Azure public cloud")

let private getQueryStringMap (query : string) =
    query.Split('&') |> Array.map (fun kvp -> kvp.Split('=')) |> Array.map (function | [| key; value |] -> (key, Some value) | [| key |] -> (key, None) | _ -> failwith "Malformed query string") |> Map.ofArray

let private getReadableDateTime (text : string) =
    match text.Replace("%3A", ":") |> DateTime.TryParse with
    | true, value -> Some <| Date.Format.localFormat Date.Local.englishUK "dd MMM yyyy a\\t hh:mm:ss" value
    | _           -> None

let private (|UrlSegments|_|) (url : Uri) =
    match url.AbsolutePath.Length > 1, lazy(url.AbsolutePath.[1..].Split('/')) with
    | true , Lazy([||])     -> None
    | true , Lazy(segments) -> Some <| Array.toList segments
    | false, _              -> None

let private getBlobName (url : Uri) =
    match url with
    | UrlSegments (_ :: _ :: _) & UrlSegments (_ :: xs) -> Some <| String.Join("/", xs)
    | _                                                 -> None
    
let private getContainerName (url : Uri) =
    match url with
    | UrlSegments (x :: _) -> Some x
    | _                    -> None
    
let private getCloud domain =
    cloudMap |> Map.tryFind domain |> Option.defaultValue domain
    
let private getPermissionsExplanation permissions =
    permissions |>
    Seq.map (function
             | 'r' -> Some "r - Read"
             | 'w' -> Some "w - Write"
             | _   -> None) |>
    fun x -> if Seq.forall Option.isSome x then String.Join("\n", x) |> Some else None
    
let private getResourcesExplanation resource =
    match resource with
    | "b"  -> Some "Blob"
    | "bv" -> Some "Blob version"
    | "bs" -> Some "Blob snapshot"
    | "c"  -> Some "Container"
    | "d"  -> Some "Directory"
    | _    -> None

let private getIpExplanation (ip : string) =
    match ip.Split('-') with
    | [| ip |]           -> Some <| sprintf "Single IP: %s" ip
    | [| fromIp; toIp |] -> Some <| sprintf "IP range from: %s to: %s" fromIp toIp
    | _                  -> None

let private parseHost (host : string) =
    let errorMessage = "Unsupported URL (custom domain or bad URL, add a nice error message)"
    
    match host.Split('.') with
    | [| account; service; "core"; cloud; "net" |] -> {| Account = Some account; Service = Some service; Cloud = Some cloud |}
    | _ ->                                            {| Account = Some errorMessage; Service = None; Cloud = None |}

let private parse url =
    tryParseUrl url |>
    Option.map (fun url -> (url, parseHost url.Host, getQueryStringMap url.Query.[1..])) |>
    Option.map (fun (url, hostInfo, query) ->
        let tryGetQueryStringValue key =
            query |> Map.tryFind key |> Option.flatten
        
        let tryGetQueryStringValueAndMap key func =
            match tryGetQueryStringValue key with
            | Some value -> {| Value = Some value; Explanation = Some <| func value |}
            | None       -> {| Value = None;       Explanation = None               |}
        
        let tryGetQueryStringValueAndBind key func =
            match tryGetQueryStringValue key with
            | Some value -> {| Value = Some value; Explanation = func value |}
            | None       -> {| Value = None;       Explanation = None               |}
        
        {|
            Account       = hostInfo.Account
            Service       = hostInfo.Service
            Domain        = hostInfo.Cloud
            Cloud         = hostInfo.Cloud |> Option.map getCloud
            Container     = getContainerName url
            Blob          = getBlobName url
            Version       = tryGetQueryStringValueAndMap  "sv"  (sprintf "API version: %s")
            Start         = tryGetQueryStringValueAndBind "st"  getReadableDateTime
            Expiry        = tryGetQueryStringValueAndBind "se"  getReadableDateTime
            Resource      = tryGetQueryStringValueAndBind "sr"  getResourcesExplanation
            Permissions   = tryGetQueryStringValueAndBind "sp"  getPermissionsExplanation
            IP            = tryGetQueryStringValueAndBind "sip" getIpExplanation
            Protocol      = tryGetQueryStringValue        "spr"
            Signature     = tryGetQueryStringValue        "sig"
        |})

let private serviceSas =
    Some "Service SAS"

let private hmacSignature =
    Some "HMAC signature"

let private yourUrl =
    Some "Your URL"

let parserCard model dispatch =
    let urlField =
        urlField model dispatch
        
    let createSasRowsOrDefault fromSas defaultRows =
        parse model.Url |>
        Option.map (fromSas >> (List.filter ((<>)Html.none))) |>
        Option.defaultValue defaultRows
    
    let disclaimer title text =
        if model.DisclaimerVisible then
            disclaimer title text dispatch
        else
            Html.none
    
    card [
        Html.form [
            disclaimer "Note"
                       "This page does not store or share URLs or any extracted data, all is processed within the browser boundaries"
            
            urlField "Insert URL/token to parse" Icon.Link
            
            Html.table [
                tableHeader [
                    th "Parameter"
                    th "Readable value"
                    th "Field name"
                    th "Value"
                ]
                Html.tableBody [
                    yield! createSasRowsOrDefault (fun sas -> [
                        row "Type"              serviceSas                  "URL"                       yourUrl
                        row "Account"           sas.Account                 "https://{account}.[...]"   sas.Account      
                        row "Service"           sas.Service                 "{account}.{service}.[...]" sas.Service      
                        row "Cloud"             sas.Cloud                   "core.{cloud}.net"          sas.Domain         
                        row "Container"         sas.Container               "/{container}/[...]"        sas.Container  
                        row "Blob"              sas.Blob                    "{container}/{blob}"        sas.Blob            
                        row "Version"           sas.Version.Explanation     "sv"                        sas.Version.Value
                        row "Start time"        sas.Start.Explanation       "st"                        sas.Start.Value
                        row "Expiry time"       sas.Expiry.Explanation      "se"                        sas.Expiry.Value
                        row "Resource"          sas.Resource.Explanation    "sr"                        sas.Resource.Value
                        row "Permissions"       sas.Permissions.Explanation "sp"                        sas.Permissions.Value
                        row "Allowed IP"        sas.IP.Explanation          "ip"                        sas.IP.Value
                        row "Protocol"          sas.Protocol                "spr"                       sas.Protocol   
                        row "Signature"         hmacSignature               "sig"                       sas.Signature  
                    ])
                      [ row "Invalid SAS token" None                        ""                          None ]
                ]
            ]
        ]
    ]