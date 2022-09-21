import Http.Types
import Http.Headers
import Http.URI
import Socket

namespace Http.Request

open Socket

def init (url : URI) (method : Method) (headers : Headers) (body : Option String) : Request :=
  {
    url,
    method,
    headers,
    body,
    protocol := url.scheme.asProtocol
  }

def toRequestString (r : Request) : String :=
  s!"{r.method} {r.url.path} {r.protocol.toString}" ++ CRLF ++
  r.headers.toRequestFormat ++
  CRLF ++ CRLF ++
  if let some body := r.body then body else ""
  
open Protocol in
def send (request : Request) : IO ByteArray := do
  let defaultPort :=
    match request.protocol with
    | http _ => 80
    | https _ => 443
    | _ => 80
  let remoteAddr ← SockAddr.mk {
    host := request.url.host
    port := request.url.port.getD defaultPort |> ToString.toString
    family := inet
    type := stream
  }
  let socket ← Socket.mk inet stream
  socket.connect remoteAddr
  let strSend := request.toRequestString
  let bytesSend ← socket.send strSend.toUTF8
  let bytesRecv ← socket.recv 5000
  return bytesRecv

end Http.Request
