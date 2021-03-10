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
    | UrlSegments (_ :: _ :: _) & UrlSegments (_ :: xs) -> Some <| String.Join("/", xs)
    | _                                                 -> None
    
let private getContainerName (url : Uri) =
    match url with
    | UrlSegments (x :: _) -> Some x
    | _                    -> None
    
let private getCloudResult domain =
    cloudMap |>
    Map.tryFind domain |>
    Option.map Ok |>
    Option.defaultValue (Error <| sprintf "Unsupported domain %s" domain)
    
let private getExplanationForCharacters value map =
    match value with
    | "" | null -> Error "Empty value"
    | _ ->
        value |>
        Seq.map (fun c -> Map.ofList map |> Map.tryFind c) |>
        fun x -> if   Seq.forall Option.isSome x
                 then Ok    <| String.Join("\n", x)
                 else Error <| sprintf "Unable to parse %s" value
    
let private getPermissionsExplanation permissions =
    [ 'r', "r - Read"
      'w', "w - Write"
      'd', "w - Delete"
      'y', "w - Permanent delete" 
      'l', "w - List"             
      'a', "w - Add"              
      'c', "w - Create"           
      'u', "w - Update"           
      'p', "w - Process"          ] |>
    getExplanationForCharacters permissions
    
let private getServicesExplanation services =
    [ 'b', "b - Blob"
      'q', "q - Queue"
      't', "t - Table"
      'f', "f - File"  ] |>
    getExplanationForCharacters services
    
let private getResourceTypesExplanation services =
    [ 's', "s - Service"
      'c', "c - Container/queue/table/share"
      'o', "o - Blob/message/entity/file" ] |>
    getExplanationForCharacters services
    
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
    match host.Split('.') with
    | [| account; service; "core"; domain; "net" |] -> Some {| Account = account; Service = service; Domain = domain |}
    | _ ->                                            None

type private SasType<'a> = | Account of 'a | Service of 'a | User of 'a | Invalid of 'a

let private parse url =
    tryParseUrl url |>
    Option.map (fun url -> (url, parseHost url.Host, getQueryStringMap url.Query.[1..])) |>
    Option.map (fun (url, hostInfo, query) ->
        let tryGetNonEmptyQueryStringValue key =
            query |>
            Map.tryFind key |>
            Option.flatten
        
        let tryGetQueryStringValueAndBind key func =
            tryGetNonEmptyQueryStringValue key |>
            Option.map (fun value -> {| Source = value; Parsed = func value |})
        
        let (*valuesMap*)_ =
            [ "sv"   , (sprintf "API version: %s" >> Ok)
              "sp"   , getPermissionsExplanation
              "se"   , getReadableDateTime
              "ss"   , getServicesExplanation
              "srt"  , getResourceTypesExplanation
              "sr"   , getResourcesExplanation
              "skoid", Ok
              "sktid", Ok
              "ske"  , Ok
              "sks"  , Ok
              "sdd"  , Ok
              "tn"   , Ok
              "sig"  , Ok ] |>
            List.map (fun (key, parser) -> key, tryGetQueryStringValueAndBind key parser) |>
            Map.ofList
        
        let signedVersion =
            tryGetQueryStringValueAndBind "sv"    (sprintf "API version: %s" >> Ok)
        
        let signedPermissions =
            tryGetQueryStringValueAndBind "sp"    getPermissionsExplanation
        
        let signedExpiry =
            tryGetQueryStringValueAndBind "se"    getReadableDateTime
        
        let signedServices =
            tryGetQueryStringValueAndBind "ss"    getServicesExplanation
                                                  
        let signedResourceTypes =                 
            tryGetQueryStringValueAndBind "srt"   getResourceTypesExplanation
                                                  
        let signedResource =                      
            tryGetQueryStringValueAndBind "sr"    getResourcesExplanation
        
        let signedObjectId =
            tryGetQueryStringValueAndBind "skoid" Ok

        let signedTenantId =
            tryGetQueryStringValueAndBind "sktid" Ok
                
        let signedKeyExpiryTime =
            tryGetQueryStringValueAndBind "ske"   Ok
            
        let signedKeyService =
            tryGetQueryStringValueAndBind "sks"   Ok

        let signedDirectoryDepth =
            tryGetQueryStringValueAndBind "sdd"   Ok
        
        let tableName =
            tryGetQueryStringValueAndBind "tn"    Ok
        
        let signature =
            tryGetNonEmptyQueryStringValue "sig"
        
        let serviceDependantRequiredKeyForServiceSas =
            match hostInfo |> Option.map (fun x -> x.Service) with
            | Some "blob"
            | Some "file"  -> signedResource |> Option.map (fun x -> x.Parsed) |> Option.defaultValue (Error "Missing signed resource")
            | Some "table" -> tableName      |> Option.map (fun x -> x.Parsed) |> Option.defaultValue (Error "Missing table name")
            | _            -> Ok ""
        
        let (|Parsed|_|) (record : {| Parsed : Result<string, string>; Source : string |}) =
            match record.Parsed with
            | Ok x -> Some x
            | _    -> None
        
        let isSignedDirectoryDepthRequired =
            match signedResource with
            | Some (Parsed x) when x = "d" -> true
            | _                            -> false
        
        let isSignedDirectoryDepthPresent =
            match signedDirectoryDepth with
            | Some (Parsed _) -> true
            | _               -> false
        
        let isOk =
            function
            | Ok _    -> true
            | Error _ -> false
        
        let containerName =
            getContainerName url
        
        // account: https://myaccount.blob.core.windows.net/?restype=service&comp=properties&sv=2019-02-02&ss=bf&srt=s&st=2019-08-01T22%3A18%3A26Z&se=2019-08-10T02%3A23%3A26Z&sr=b&sp=rw&sip=168.1.5.60-168.1.5.70&spr=https&sig=F%6GRVAZ5Cdj2Pw4tgU7IlSTkWgn7bUkkAg8P6HESXwmf%4B
        // service: https://myaccount.blob.core.windows.net/sascontainer/sasblob.txt?sv=2019-02-02&st=2019-04-29T22%3A18%3A26Z&se=2019-04-30T02%3A23%3A26Z&sr=b&sp=rw&sip=168.1.5.60-168.1.5.70&spr=https&sig=Z%2FRHIX5Xcg0Mq2rqI3OlWTjEg2tYkboXr1P9ZUXDtkk%3D
        // user   : https://myaccount.blob.core.windows.net/sascontainer/sasblob.txt?se=2021-03-10&sp=racwdl&sv=2018-11-09&sr=c&skoid=00000000-0000-0000-0000-000000000000&sktid=00000000-0000-0000-0000-000000000000&skt=2021-03-09T20%3A18%3A58Z&ske=2021-03-10T00%3A00%3A00Z&sks=b&skv=2018-11-09&sig=FiBaLiCorDnuS18d0000bmSLehDyG0uBT1111bmazoI%3D
        
        // required for account    sas: [sv se sig sp] srt ss
        // required for delegation sas: [sv se sig sp] sr skoid sktid ske sks sdd (when sr=d)
        // required for service    sas: [sv se sig sp] sr (only blob/file) tn (only table) sdd (when sr=d)
        
        let isAccountSas =
            [ signedResourceTypes
              signedServices      ] |>
            List.forall (function | Some (Parsed _) -> true | _ -> false) &&
            not <| Option.isSome containerName
        
        let isUserSas =
            [ signedResource
              signedObjectId
              signedTenantId
              signedKeyExpiryTime
              signedKeyService    ] |>
            List.forall (function | Some (Parsed _) -> true | _ -> false) &&
            (isSignedDirectoryDepthRequired = isSignedDirectoryDepthPresent)
        
        let isServiceSas =
            [ serviceDependantRequiredKeyForServiceSas ] |>
            List.forall (function | Ok _ -> true | _ -> false) &&
            (isSignedDirectoryDepthRequired = isSignedDirectoryDepthPresent) &&
            (not <| isOk (signedResourceTypes |> Option.map (fun x -> x.Parsed) |> Option.defaultValue(Error ""))) &&
            (not <| isOk (signedObjectId |> Option.map (fun x -> x.Parsed) |> Option.defaultValue(Error "")))
        
        let isValidSas =
            [ signedVersion
              signedExpiry
              signedPermissions ] |>
            List.forall (function | Some (Parsed _) -> true | _ -> false) &&
            Option.isSome signature
        
        let (uniqueQsKeys, sasType) =
            match isValidSas, isAccountSas, isUserSas, isServiceSas with
            | true, true , false, false -> "srt ss"                     , Ok "Account SAS"
            | true, false, false, true  -> "sr/tn sdd"                  , Ok "Service SAS"        
            | true, false, true , false -> "sr skoid sktid ske sks sdd" , Ok "User delegation SAS"
            | _                         -> ""                           , Error "Invalid SAS token"                   
                
        let createOptionRowInfo parse opt =
            Option.map (fun x -> {| Parsed = parse x; Source = x |}) opt
        
        let createRowInfoFromHostInfo parse map =
            hostInfo |>
            Option.map map |>
            createOptionRowInfo parse
        
        [
            {|
                Parameter     = "Type"
                Value         = Some {| Parsed = sasType; Source = uniqueQsKeys |}
                FieldName     = "URL and QS"
            |}
            
            {|
                Parameter     = "Account"
                Value         = createRowInfoFromHostInfo Ok (fun x -> x.Account)
                FieldName     = "//{account}."
            |}
            
            {|
                Parameter     = "Service"
                Value         = createRowInfoFromHostInfo Ok (fun x -> x.Service)
                FieldName     = ".{service}.core"
            |}
            
            {|
                Parameter     = "Cloud"
                Value         = createRowInfoFromHostInfo getCloudResult (fun x -> x.Domain)
                FieldName     = "core.{cloud}.net"
            |}
            
            {|
                Parameter     = "Container"
                Value         = createOptionRowInfo Ok containerName
                FieldName     = ".net/{container}/"
            |}
            
            {|
                Parameter     = "Blob"
                Value         = getBlobName url |> createOptionRowInfo Ok
                FieldName     = "{container}/{blob}"
            |}
            
            {|
                Parameter     = "Version"
                Value         = signedVersion
                FieldName     = "sv"
            |}
            
            {|
                Parameter     = "Start time"
                Value         = tryGetQueryStringValueAndBind "st" getReadableDateTime
                FieldName     = "st"
            |}
            
            {|
                Parameter     = "Expiry time"
                Value         = signedExpiry
                FieldName     = "se"
            |}
            
            {|
                Parameter     = "Services"
                Value         = signedServices
                FieldName     = "ss"
            |}
            
            {|
                Parameter     = "Resource"
                Value         = signedResource
                FieldName     = "sr"
            |}
            
            {|
                Parameter     = "Permissions"
                Value         = signedPermissions
                FieldName     = "sp"
            |}
            
            {|
                Parameter     = "Allowed IP"
                Value         = tryGetQueryStringValueAndBind "sip" getIpExplanation
                FieldName     = "sip"
            |}
            
            {|
                Parameter     = "Protocol"
                Value         = tryGetQueryStringValueAndBind "spr" getProtocol
                FieldName     = "spr"
            |}
            
            {|
                Parameter     = "Types"
                Value         = signedResourceTypes
                FieldName     = "srt"
            |}
            
            {|
                Parameter     = "Signature"
                Value         = signature |> Option.map (fun s -> {| Parsed = Ok "HMAC signature"; Source = s |})
                FieldName     = "sig"
            |}
            
//"Table name"           "tn"
//"From partition key"   "spk"
//"From row key"         "srk"
//"To partition key"     "epk"
//"To row key"           "erk"
//"Policy"               "si"
//"Object ID"            "skoid"
//"Tenand ID"            "sktid"
//"Key start time"       "skt"
//"Key expiry time"      "ske"
//"Key service"          "sks"
//"AuthorizedObjectId"   "saoid"
//"UnauthorizedObjectId" "suoid"
//"Correlation ID"       "scid"
//"Directory depth"      "sdd"
//"Cache-Control"        "rscc"
//"Content-Disposition"  "rscd"
//"Content-Encoding"     "rsce"
//"Content-Language"     "rscl"
//"Content-Type"         "rsct"
        ])

let private hmacSignature =
    Ok "HMAC signature"

let private yourUrl =
    Ok "Your URL"

let parserCard model dispatch =
    let urlField =
        urlField model dispatch
        
    let rows =
        parse model.Url |>
        Option.map (fun sas -> sas |>
                               List.choose (fun x -> match x.Value with
                                                     | Some y -> Some {| FieldName = x.FieldName; Parameter = x.Parameter; Value = y |}
                                                     | None   -> None) |>
                               List.map(fun r -> row r.Parameter r.Value.Parsed r.FieldName r.Value.Source)) |>
        Option.map ((List.filter ((<>)Html.none))) |>
        Option.defaultValue [ row "Type" (Error "Invalid URL") "URL" "" ]
    
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
                    yield! rows
                ]
            ]
        ]
    ]