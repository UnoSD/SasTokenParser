module SasTokenParser.Messages

open SasTokenParser.Models

type Message =
    | ChangeToTab of Tab
    
    | UrlChanged of string
    
    | CloseDisclaimer