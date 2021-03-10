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

module private Result =
    let isOk =
        function
        | Ok _    -> true
        | Error _ -> false
        
    let isError res =
        not <| isOk res

type private SasType = Account | Service | User | Invalid

let private signedResource             = "sr"
let private signedVersion              = "sv"
let private signedPermissions          = "sp"
let private signedStart                = "st"
let private signedExpiry               = "se"
let private signedServices             = "ss"
let private signedResourceTypes        = "srt"
let private signedObjectId             = "skoid"
let private signedTenantId             = "sktid"
let private signedKeyExpiryTime        = "ske"
let private signedKeyService           = "sks"
let private signedDirectoryDepth       = "sdd"  
let private signedIp                   = "sip"  
let private signedProtocol             = "spr"  
let private tableName                  = "tn"                                       
let private startPk                    = "spk"
let private startRk                    = "srk"
let private endPk                      = "epk"
let private endRk                      = "erk"
let private signedIdentifier           = "si"
let private signedKeyStartTime         = "skt"
let private signedKeyVersion           = "skv"
let private signedAuthorizedObjectId   = "saoid"
let private signedUnauthorizedObjectId = "suoid"
let private signedCorrelationId        = "scid"
let private cacheControlResponse       = "rscc"
let private contentDispositionResponse = "rscd"
let private contentEncodingResponse    = "rsce"
let private contentLanguageResponse    = "rscl"
let private contentTypeResponse        = "rsct"
let private signature                  = "sig"

let private createOptionRowInfo parse opt =
    Option.map (fun x -> {| Parsed = parse x; Source = x |}) opt

// required for account    sas: [sv se sig sp]    srt ss
// required for delegation sas: [sv se sig sp] sr skoid sktid ske sks sdd (when sr=d)
// required for service    sas: [sv se sig sp] sr (only blob/file) tn (only table) sdd (when sr=d)
let private getSasType (qsValueMap : Map<string, {| Parsed : Result<string, string>; Source : string |} option>) service containerName =
    let (|Parsed|_|) (record : {| Parsed : Result<string, string>; Source : string |}) =
        match record.Parsed with
        | Ok x -> Some x
        | _    -> None
    
    let isSignedDirectoryDepthRequired =
        match qsValueMap.[signedResource] with
        | Some (Parsed x) when x = "d" -> true
        | _                            -> false
    
    let isSignedDirectoryDepthPresent =
        match qsValueMap.[signedDirectoryDepth] with
        | Some (Parsed _) -> true
        | _               -> false
    
    let allValidInQueryString keys =
        keys |>
        List.map (fun x -> Map.find x qsValueMap) |>
        List.forall (function | Some (Parsed _) -> true | _ -> false)
    
    let queryStringParsed key =
        qsValueMap.[key] |> Option.map (fun x -> x.Parsed) |> Option.defaultValue(Error "")
    
    let isMissingOrInvalid key =
        Result.isError <| queryStringParsed key
    
    let isValid key =
        not <| isMissingOrInvalid key
    
    let hadServiceSasRequiredKeys =
        match service with
        | Some "blob"
        | Some "file"  -> isValid signedResource
        | Some "table" -> isValid tableName
        | _            -> false
    
    let isAccountSas =
        [ signedResourceTypes
          signedServices      ] |>
        allValidInQueryString   &&
        Option.isNone containerName
    
    let isUserSas =
        [ signedResource
          signedObjectId
          signedTenantId
          signedKeyExpiryTime
          signedKeyVersion
          signedKeyService    ] |>
        allValidInQueryString   &&
        (isSignedDirectoryDepthRequired = isSignedDirectoryDepthPresent)
    
    let isServiceSas =
        hadServiceSasRequiredKeys &&
        (isSignedDirectoryDepthRequired = isSignedDirectoryDepthPresent) &&
        isMissingOrInvalid signedResourceTypes &&
        isMissingOrInvalid signedObjectId
    
    let isValidSas =
        [ signedVersion
          signedExpiry
          signedPermissions ] |>
        List.forall isValid &&
        Option.isSome qsValueMap.[signature]
        
    match isValidSas, isAccountSas, isUserSas, isServiceSas with
    | true, true , false, false -> Account
    | true, false, false, true  -> Service        
    | true, false, true , false -> User
    | _                         -> Invalid      

let private getGuid text =
    match Guid.TryParse(text) with
    | true, _ -> Ok text
    | _       -> Error "Invalid GUID"

let private getInteger text =
    match Int32.TryParse(text) with
    | true, _ -> Ok text
    | _       -> Error "Invalid number"

let private queryStringValidators = [
    signedVersion             , (sprintf "API version: %s" >> Ok)
    signedServices            , getServicesExplanation
    signedKeyService          , getServicesExplanation
    signedStart               , getReadableDateTime
    signedExpiry              , getReadableDateTime
    signedKeyExpiryTime       , getReadableDateTime
    signedKeyStartTime        , getReadableDateTime
    signedObjectId            , getGuid
    signedTenantId            , getGuid
    signedAuthorizedObjectId  , getGuid
    signedUnauthorizedObjectId, getGuid
    signedResourceTypes       , getResourceTypesExplanation
    signedPermissions         , getPermissionsExplanation
    signedResource            , getResourcesExplanation
    signedIp                  , getIpExplanation
    signedProtocol            , getProtocol
    signedDirectoryDepth      , getInteger
    signedKeyVersion          , Ok
    tableName                 , Ok
    startPk                   , Ok
    startRk                   , Ok
    endPk                     , Ok
    endRk                     , Ok
    signedIdentifier          , Ok
    signedCorrelationId       , Ok
    cacheControlResponse      , Ok
    contentDispositionResponse, Ok
    contentEncodingResponse   , Ok
    contentLanguageResponse   , Ok
    contentTypeResponse       , Ok
    signature                 , (fun _ -> Ok "HMAC signature")
]

let private tryGetNonEmptyQueryStringValue query key =
    query |>
    Map.tryFind key |>
    Option.flatten

let private tryGetQueryStringValueAndBind query key func =
    tryGetNonEmptyQueryStringValue query key |>
    Option.map (fun value -> {| Source = value; Parsed = func value |})

let private unsupportedRowInfo query =
    query |>
    Map.filter (fun key _ -> queryStringValidators |> List.exists (fun (k, _) -> k = key) |> not) |>
    Seq.map (fun kvp -> kvp.Key) |>
    List.ofSeq |>
    function
    | [ ] -> None
    |  x  -> Some {| Parsed = Error <| String.Join(", ", x); Source = "" |}

// account: https://myaccount.blob.core.windows.net/?restype=service&comp=properties&sv=2019-02-02&ss=bf&srt=s&st=2019-08-01T22%3A18%3A26Z&se=2019-08-10T02%3A23%3A26Z&sr=b&sp=rw&sip=168.1.5.60-168.1.5.70&spr=https&sig=F%6GRVAZ5Cdj2Pw4tgU7IlSTkWgn7bUkkAg8P6HESXwmf%4B
// service: https://myaccount.blob.core.windows.net/sascontainer/sasblob.txt?sv=2019-02-02&st=2019-04-29T22%3A18%3A26Z&se=2019-04-30T02%3A23%3A26Z&sr=b&sp=rw&sip=168.1.5.60-168.1.5.70&spr=https&sig=Z%2FRHIX5Xcg0Mq2rqI3OlWTjEg2tYkboXr1P9ZUXDtkk%3D
// user   : https://myaccount.blob.core.windows.net/sascontainer/sasblob.txt?se=2021-03-10&sp=racwdl&sv=2018-11-09&sr=c&skoid=00000000-0000-0000-0000-000000000000&sktid=00000000-0000-0000-0000-000000000000&skt=2021-03-09T20%3A18%3A58Z&ske=2021-03-10T00%3A00%3A00Z&sks=b&skv=2018-11-09&sig=FiBaLiCorDnuS18d0000bmSLehDyG0uBT1111bmazoI%3D
let createRowInfos (url : Uri) =
    let hostInfo =
        parseHost url.Host
        
    let query =
        getQueryStringMap url.Query.[1..]
            
    let qsValueMap =
        queryStringValidators |>
        List.map (fun (key, parser) -> key, tryGetQueryStringValueAndBind query key parser) |>
        Map.ofList
    
    let containerName =
        getContainerName url
    
    let service =
        hostInfo |> Option.map (fun x -> x.Service)
    
    let (uniqueQsKeys, sasType) =
        match getSasType qsValueMap service containerName with
        | Account -> "srt ss"                     , Ok    "Account SAS"
        | Service -> "sr/tn sdd"                  , Ok    "Service SAS"        
        | User    -> "sr skoid sktid ske sks sdd" , Ok    "User delegation SAS"
        | _       -> ""                           , Error "Invalid SAS token"                   

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
            Value         = qsValueMap.[signedVersion]
            FieldName     = signedVersion
        |}
        
        {|
            Parameter     = "Start time"
            Value         = qsValueMap.[signedStart]
            FieldName     = signedStart
        |}
        
        {|
            Parameter     = "Expiry time"
            Value         = qsValueMap.[signedExpiry]
            FieldName     = signedExpiry
        |}
        
        {|
            Parameter     = "Services"
            Value         = qsValueMap.[signedServices]
            FieldName     = signedServices
        |}
        
        {|
            Parameter     = "Resource"
            Value         = qsValueMap.[signedResource]
            FieldName     = signedResource
        |}
        
        {|
            Parameter     = "Permissions"
            Value         = qsValueMap.[signedPermissions]
            FieldName     = signedPermissions
        |}
        
        {|
            Parameter     = "Allowed IP"
            Value         = qsValueMap.[signedIp]
            FieldName     = signedIp
        |}
        
        {|
            Parameter     = "Protocol"
            Value         = qsValueMap.[signedProtocol]
            FieldName     = signedProtocol
        |}
        
        {|
            Parameter     = "Types"
            Value         = qsValueMap.[signedResourceTypes]
            FieldName     = signedResourceTypes
        |}
        
        {|
            Parameter     = "Object ID"
            Value         = qsValueMap.[signedObjectId]
            FieldName     = signedObjectId
        |}
        
        {|
            Parameter     = "Tenant ID"
            Value         = qsValueMap.[signedTenantId]
            FieldName     = signedTenantId
        |}
        
        {|
            Parameter     = "Table name"
            Value         = qsValueMap.[tableName]
            FieldName     = tableName
        |}
        
        {|
            Parameter     = "Key expiry time"
            Value         = qsValueMap.[signedKeyExpiryTime]
            FieldName     = signedKeyExpiryTime
        |}
        
        {|
            Parameter     = "Key service"
            Value         = qsValueMap.[signedKeyService]
            FieldName     = signedKeyService
        |}
        
        {|
            Parameter     = "Directory depth"
            Value         = qsValueMap.[signedDirectoryDepth]
            FieldName     = signedDirectoryDepth
        |}

        {|
            Parameter     = "From partition key"
            Value         = qsValueMap.[startPk]
            FieldName     = startPk
        |}

        {|
            Parameter     = "From row key"
            Value         = qsValueMap.[startRk]
            FieldName     = startRk
        |}
             
        {|
            Parameter     = "To partition key"
            Value         = qsValueMap.[endPk]
            FieldName     = endPk
        |}
              
        {|
            Parameter     = "To row key"
            Value         = qsValueMap.[endRk]
            FieldName     = endRk
        |}
        
        {|
            Parameter     = "Policy"
            Value         = qsValueMap.[signedIdentifier]
            FieldName     = signedIdentifier
        |}
        
        {|
            Parameter     = "Key start time"
            Value         = qsValueMap.[signedKeyStartTime]
            FieldName     = signedKeyStartTime
        |}
        
        {|
            Parameter     = "Authorized object ID"
            Value         = qsValueMap.[signedAuthorizedObjectId]
            FieldName     = signedAuthorizedObjectId
        |}

        {|
            Parameter     = "Unauthorized object ID"
            Value         = qsValueMap.[signedUnauthorizedObjectId]
            FieldName     = signedUnauthorizedObjectId
        |}

        {|
            Parameter     = "Correlation ID"
            Value         = qsValueMap.[signedCorrelationId]
            FieldName     = signedCorrelationId
        |}

        {|
            Parameter     = "Cache-Control response header"
            Value         = qsValueMap.[cacheControlResponse]
            FieldName     = cacheControlResponse
        |}
        
        {|
            Parameter     = "Content-Disposition response header"
            Value         = qsValueMap.[contentDispositionResponse]
            FieldName     = contentDispositionResponse
        |}
        
        {|
            Parameter     = "Content-Encoding response header"
            Value         = qsValueMap.[contentEncodingResponse]
            FieldName     = contentEncodingResponse
        |}
        
        {|
            Parameter     = "Content-Language response header"
            Value         = qsValueMap.[contentLanguageResponse]
            FieldName     = contentLanguageResponse
        |}
        
        {|
            Parameter     = "Content-Type response header"
            Value         = qsValueMap.[contentTypeResponse]
            FieldName     = contentTypeResponse
        |}
         
        {|
            Parameter     = "Key version"
            Value         = qsValueMap.[signedKeyVersion]
            FieldName     = signedKeyVersion
        |}
         
        {|
            Parameter     = "Signature"
            Value         = qsValueMap.[signature]
            FieldName     = signature
        |}
        
        {|
            Parameter     = "Unsupported query keys"
            Value         = unsupportedRowInfo query
            FieldName     = ""
        |}
    ]

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