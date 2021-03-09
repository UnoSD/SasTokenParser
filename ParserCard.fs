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

let private row (parameter : string) (readableValue : Result<string, string>) (fieldName : string) (value : Result<string, string>) =
    [
        Html.tableCell [ Html.strong [ Html.text parameter ] ]
        Html.tableCell [ Html.div [
            match readableValue with
            | Ok readableValue -> yield! multilineStringToHtml readableValue
            | Error error      -> Html.strong [ prop.style [ style.color.red ]; prop.children [ Html.text error ] ]
        ] ]
        Html.tableCell [ Html.text fieldName ]
        Html.tableCell [
            match value with
            | Ok value    -> Html.text value
            | Error error -> Html.strong [ prop.style [ style.color.red ]; prop.children [ Html.text error ] ]
        ]
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
    | true, value -> Ok <| Date.Format.localFormat Date.Local.englishUK "dd MMM yyyy a\\t hh:mm:ss" value
    | _           -> Error <| sprintf "Unable to parse date %s" text

let private (|UrlSegments|_|) (url : Uri) =
    match url.AbsolutePath.Length > 1, lazy(url.AbsolutePath.[1..].Split('/')) with
    | true , Lazy([||])     -> None
    | true , Lazy(segments) -> Some <| Array.toList segments
    | false, _              -> None

let private getBlobName (url : Uri) =
    match url with
    | UrlSegments (_ :: _ :: _) & UrlSegments (_ :: xs) -> Ok <| String.Join("/", xs)
    | _                                                 -> Error "Unable to parse blob name"
    
let private getContainerName (url : Uri) =
    match url with
    | UrlSegments (x :: _) -> Ok x
    | _                    -> Error "Unable to parse container name"
    
let private getCloud domain =
    cloudMap |> Map.tryFind domain |> Option.defaultValue domain
    
let private getPermissionsExplanation permissions =
    permissions |>
    Seq.map (function
             | 'r' -> Some "r - Read"
             | 'w' -> Some "w - Write"
             | _   -> None) |>
    fun x -> if Seq.forall Option.isSome x then String.Join("\n", x) |> Ok else Error <| sprintf "Unable to parse %s" permissions
    
let private getServicesExplanation services =
    services |>
    Seq.map (function
             | 'b' -> Some "b - Blob"
             | 'q' -> Some "q - Queue"
             | 't' -> Some "t - Table"
             | 'f' -> Some "f - File"
             | _   -> None) |>
    fun x -> if Seq.forall Option.isSome x then String.Join("\n", x) |> Ok else Error <| sprintf "Unable to parse %s" services
    
let private getResourcesExplanation resource =
    match resource with
    | "b"  -> Ok "Blob"
    | "bv" -> Ok "Blob version"
    | "bs" -> Ok "Blob snapshot"
    | "c"  -> Ok "Container"
    | "d"  -> Ok "Directory"
    | rt   -> Error <| sprintf "Unrecognised resource type %s" rt

let private getIpExplanation (ip : string) =
    match ip.Split('-') with
    | [| ip |]           -> Ok <| sprintf "Single IP: %s" ip
    | [| fromIp; toIp |] -> Ok <| sprintf "IP range from: %s to: %s" fromIp toIp
    | _                  -> Error "Unable to parse IP or IP range"

let private getProtocol =
    function
    | "http"       -> Ok "HTTP"
    | "https"      -> Ok "HTTPS"
    | "https,http" -> Ok "HTTP and HTTPS"
    | x            -> Error <| sprintf "Unable to parse protocol %s" x

let private parseHost (host : string) =
    let errorMessage = "Unsupported URL (custom domain or bad URL, add a nice error message)"
    
    match host.Split('.') with
    | [| account; service; "core"; cloud; "net" |] -> {| Account = Ok account; Service = Ok service; Cloud = Ok cloud |}
    | _ ->                                            {| Account = Error errorMessage; Service = Error ""; Cloud = Error "" |}

type private SasType<'a> = | Account of 'a | Service of 'a

let private parse url =
    tryParseUrl url |>
    Option.map (fun url -> (url, parseHost url.Host, getQueryStringMap url.Query.[1..])) |>
    Option.map (fun (url, hostInfo, query) ->
        let tryGetQueryStringValue key =
            query |> Map.tryFind key |> Option.flatten |> Option.map Ok |> Option.defaultValue (Error "Missing")
        
        let tryGetQueryStringValueAndMap key func =
            match tryGetQueryStringValue key with
            | Ok value -> {| Value = Ok value;     Explanation = Ok <| func value |}
            | Error _  -> {| Value = Ok "Missing"; Explanation = Ok "Missing"     |}
        
        let tryGetQueryStringValueAndBind key func =
            match tryGetQueryStringValue key with
            | Ok value -> {| Value = Ok value;     Explanation = func value   |}
            | Error _  -> {| Value = Ok "Missing"; Explanation = Ok "Missing" |}
        
        let blobName = getBlobName url
        let containerName = getContainerName url
        let sasType =
            match containerName, blobName with
            | Error _, Error _ -> Account
            | _                -> Service
        
        {|
            Account       = hostInfo.Account
            Service       = hostInfo.Service
            Domain        = hostInfo.Cloud
            Cloud         = hostInfo.Cloud |> Result.map getCloud
            Container     = getContainerName url
            Blob          = getBlobName url
            Version       = tryGetQueryStringValueAndMap  "sv"  (sprintf "API version: %s")
            Services      = tryGetQueryStringValueAndBind "ss"  getServicesExplanation
            Start         = tryGetQueryStringValueAndBind "st"  getReadableDateTime
            Expiry        = tryGetQueryStringValueAndBind "se"  getReadableDateTime
            Resource      = tryGetQueryStringValueAndBind "sr"  getResourcesExplanation
            Permissions   = tryGetQueryStringValueAndBind "sp"  getPermissionsExplanation
            IP            = tryGetQueryStringValueAndBind "sip" getIpExplanation
            Protocol      = tryGetQueryStringValueAndBind "spr" getProtocol
            Signature     = tryGetQueryStringValue        "sig"
        |} |> sasType)

let private serviceSas =
    Ok "Service SAS"
    
let private accountSas =
    Ok "Account SAS"

let private hmacSignature =
    Ok "HMAC signature"

let private yourUrl =
    Ok "Your URL"

let private empty =
    Ok ""

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
                    yield! createSasRowsOrDefault (function
                        | Service sas ->
                            [
                                row "Type"              serviceSas                  "URL"                yourUrl
                                row "Account"           sas.Account                 "//{account}."       sas.Account
                                row "Service"           sas.Service                 ".{service}.core"    sas.Service      
                                row "Cloud"             sas.Cloud                   "core.{cloud}.net"   sas.Domain         
                                row "Container"         sas.Container               ".net/{container}/"  sas.Container  
                                row "Blob"              sas.Blob                    "{container}/{blob}" sas.Blob            
                                row "Version"           sas.Version.Explanation     "sv"                 sas.Version.Value
                                row "Start time"        sas.Start.Explanation       "st"                 sas.Start.Value
                                row "Expiry time"       sas.Expiry.Explanation      "se"                 sas.Expiry.Value
                                row "Resource"          sas.Resource.Explanation    "sr"                 sas.Resource.Value
                                row "Permissions"       sas.Permissions.Explanation "sp"                 sas.Permissions.Value
                                row "Allowed IP"        sas.IP.Explanation          "ip"                 sas.IP.Value
                                row "Protocol"          sas.Protocol.Explanation    "spr"                sas.Protocol.Value  
                                row "Signature"         hmacSignature               "sig"                sas.Signature
                            ]
                        | Account sas -> [
                                row "Type"              accountSas                  "URL"                yourUrl
                                row "Account"           sas.Account                 "//{account}."       sas.Account
                                row "Service"           sas.Service                 ".{service}.core"    sas.Service      
                                row "Cloud"             sas.Cloud                   "core.{cloud}.net"   sas.Domain         
                                row "Version"           sas.Version.Explanation     "sv"                 sas.Version.Value
                                row "Services"          sas.Services.Explanation    "ss"                 sas.Services.Value
                                row "Start time"        sas.Start.Explanation       "st"                 sas.Start.Value
                                row "Expiry time"       sas.Expiry.Explanation      "se"                 sas.Expiry.Value
                                row "Resource"          sas.Resource.Explanation    "sr"                 sas.Resource.Value
                                row "Permissions"       sas.Permissions.Explanation "sp"                 sas.Permissions.Value
                                row "Allowed IP"        sas.IP.Explanation          "ip"                 sas.IP.Value
                                row "Protocol"          sas.Protocol.Explanation    "spr"                sas.Protocol.Value  
                                row "Signature"         hmacSignature               "sig"                sas.Signature
                        ])
                      [ row "Invalid SAS token" empty                       ""                   empty ]
                ]
            ]
        ]
    ]