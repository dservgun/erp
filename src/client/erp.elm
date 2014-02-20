{-- Starting point for an erp client in elm. --}
import Char
import Maybe
import Http
import Graphics.Input as Input
import Graphics.Collage as Collage
import WebSocket
import Mouse
import Keyboard
import Json
import Dict

defaultUrl = "ws://localhost:8082"
entered = keepIf id True Keyboard.enter

type UIState = {requests : [String], 
                responses : [String]}
initState : UIState
initState = {requests = [],
             responses = []}

sendJsonString aString emailId = 
    if | aString /= "" && emailId /= "" ->
           Json.toString "\n" (Json.Object . Dict.fromList <| 
                               [("name", Json.String aString),
                                ("email", Json.String emailId)])
       |otherwise -> ""

parseJsonValue aValue = aValue
                   
incoming : Signal String
incoming = WebSocket.connect defaultUrl (sendJsonString <~ name ~ emailId)
{-- Maintaining users --}
(name, emailId) = (sampleOn entered content, sampleOn entered content2)

(field1, content) = Input.field "Name"
(field2, content2) = Input.field "email address"
drawString aString = flow down [(plainText <| "" ++ aString)]
updateResponse aString aState = {aState | responses <- aString :: aState.responses}
updateRequest aString aState = {aState | requests <- aString :: aState.requests}


handle aString  = updateResponse aString . updateRequest aString

inputSignal =  incoming
mainSignal = foldp handle initState inputSignal
     
render aState = asText aState

main = above <~ field2 ~ (lift2 above field1  (render <~ mainSignal))

