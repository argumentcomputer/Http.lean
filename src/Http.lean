import Http.Types
import Http.URI
import Socket

namespace Http
open Socket

def getRaw (url : URI) : IO ByteArray := do
  let remoteAddr ← url.mkSockAddr
  let socket ← Socket.mk inet stream
  socket.connect remoteAddr
  let strSend :=
    "GET / HTTP/1.1\r\n" ++
    s!"Host: {url.host}\r\n" ++
    "\r\n\r\n"
  let bytesSend ← socket.send strSend.toUTF8
  let bytesRecv ← socket.recv 5000
  return bytesRecv
end Http
