import Http.Types
import Http.Headers
import Http.Parsec

namespace Http.Response

namespace Parser
open Http.Headers Parsec

def protocol : Parsec Protocol := do
  let name ← many1Chars asciiLetter
  skipChar '/'
  let version ← many1Chars (digit <|> pchar '.')
  match name with
  | "HTTP" => return Protocol.http version
  | "HTTPS" => return Protocol.https version
  | s => return Protocol.other s version

def digitsToNat (ds : Array Nat) : Nat :=
  ds.toList.enum.foldl (λ acc (d, i) => acc + d * 10 ^ i) 0

def response : Parsec Response := do
  let protocol ← protocol
  ws
  let statusCode ← digitsToNat <$> many1 digitNat
  ws
  let message ← manyChars <| satisfy (λ c => c != '\n')
  ws
  let headers ← Parser.headers
  let body ← rest
  return {
    protocol,
    headers,
    message,
    body := some body,
    statusCode,
  : Response }

end Parser

def parse (s : String) : Except String Response := Parser.response.parse s

end Http.Response
