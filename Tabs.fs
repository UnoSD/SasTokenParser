module SasTokenParser.Tabs

open Fulma
open Fable.React
open Fable.React.Props
open SasTokenParser.Models
open SasTokenParser.Messages

let tabs model dispatch =
    let tab tabType title =
        Tabs.tab [ Tabs.Tab.IsActive (model.CurrentTab = tabType) ]
                 [ a [ OnClick (fun _ -> ChangeToTab tabType |> dispatch) ] [ str title ] ]
    
    Tabs.tabs [ Tabs.IsCentered ]
              [ tab Parser "Parser"
                tab About  "About"  ]