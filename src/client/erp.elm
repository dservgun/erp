{-- Starting point for an erp client in elm. --}
import Char
import Maybe
import Http
import Graphics.Input as Input
import Graphics.Input.Field as Field
import Graphics.Collage as Collage
import WebSocket
import Mouse
import Keyboard
import Json
import Dict

defaultUrl = "ws://localhost:8082"
entered = keepIf identity True Keyboard.enter

type UIState = {requests : [String], 
                responses : [String]}
initState : UIState
initState = {requests = [],
             responses = []}

sendJsonString aString emailId = 
    if | aString /= "" && emailId /= "" ->
           Json.toString "\n" (Json.Object << Dict.fromList <| 
                               [("name", Json.String aString),
                                ("email", Json.String emailId)])
       |otherwise -> ""

parseJsonValue aValue = aValue
                   
incoming : Signal String
incoming = WebSocket.connect defaultUrl (sendJsonString <~ name ~ emailId)
{-- Maintaining users --}
(name, emailId) = (sampleOn entered (lift (.string) content.signal), sampleOn entered (lift (.string) content2.signal))

content = Input.input Field.noContent
content2 = Input.input Field.noContent
field1 = Field.field Field.defaultStyle content.handle identity "Name" <~ content.signal
field2 = Field.email Field.defaultStyle content2.handle identity "email address" <~ content2.signal
  
drawString aString = flow down [(plainText <| "" ++ aString)]
updateResponse aString aState = {aState | responses <- aString :: aState.responses}
updateRequest aString aState = {aState | requests <- aString :: aState.requests}


handle aString  = updateResponse aString << updateRequest aString

inputSignal =  incoming
mainSignal = foldp handle initState inputSignal
     
render aState = asText aState

main = above <~ field2 ~ (lift2 above field1 (render <~ mainSignal))

