module SasTokenParser.Updates

open Elmish
open SasTokenParser.Models
open SasTokenParser.Messages

let init _ =
    emptyModel, Cmd.none

let update message model =
    match message with    
    | ChangeToTab tab -> { model with CurrentTab = tab }          , Cmd.none
                                                                  
    | UrlChanged url  -> { model with Url = url }                 , Cmd.none
    
    | CloseDisclaimer -> { model with DisclaimerVisible = false } , Cmd.none