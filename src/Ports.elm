port module Ports exposing (..)

import Json.Encode as Json


port startDrag : () -> Cmd msg


port endDrag : () -> Cmd msg


port onEditStart : String -> Cmd msg


port onWindowKeyUp : (Json.Value -> msg) -> Sub msg


port gotNewId : (String -> msg) -> Sub msg


port generateNewId : () -> Cmd msg


requestNewId =
    generateNewId ()


sendStartDrag =
    startDrag ()


sendEndDrag =
    endDrag ()


port scrollToTop : () -> Cmd msg
