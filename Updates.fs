module SasTokenParser.Updates

open Elmish
open Browser
open SasTokenParser.Models
open SasTokenParser.Messages

let init _ =
    let windowUrl =
        URL.Create window.document.URL
        
    let url =
        match windowUrl.hash with
        | "" | null           -> emptyModel.Url
        | x when x.Length = 1 -> emptyModel.Url
        | x                   -> x.[1..]
        
    { emptyModel with Url = url }, Cmd.none

let update message model =
    match message with    
    | ChangeToTab tab -> { model with CurrentTab = tab }          , Cmd.none
                                                                  
    | UrlChanged url  -> { model with Url = url }                 , Cmd.none
    
    | CloseDisclaimer -> { model with DisclaimerVisible = false } , Cmd.none