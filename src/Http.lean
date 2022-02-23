import Http.Types
import Http.URI
import Http.Request
import Http.Response
import Http.Headers

namespace Http

namespace Client

def request (method : Method)  (url : URI) (body : Option String) : IO Response := do
  try
    let headers := Headers.fromList [("Host", url.host)]
    let request := Request.init url method headers body
    let text ← String.fromUTF8Unchecked <$> liftM request.send
    match Response.parse text with
    | Except.ok response => return response
    | Except.error e => throw <| IO.Error.userError e
  catch e =>
    throw <| IO.Error.userError s!"Request failed: {e}"

def get (url : URI) : IO Response :=
  request Method.GET url none

def post (url : URI) (body : String) : IO Response :=
  request Method.POST url none

end Client

end Http
