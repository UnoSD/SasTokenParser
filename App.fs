module SasTokenParser.App

open Elmish
open Elmish.HMR
open Elmish.Debug
open SasTokenParser.Updates
open SasTokenParser.View

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run