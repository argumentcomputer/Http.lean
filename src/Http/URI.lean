import Std
import Http.Parsec
import Socket

open Std Parsec Socket

namespace Http
namespace URI

def Hostname := String

deriving instance ToString for Hostname

def Scheme := String
def Scheme.mk (s: String) := s

deriving instance ToString for Scheme

def Path := List String

instance : ToString Path where
  toString p := p.foldl (λ acc s => acc ++ s) "/"

structure UserInfo where
  username : String
  password : Option String

instance : ToString UserInfo where
  toString ui := ""

def Fragment := List (String × String)

instance : ToString Fragment where
  toString (q : Fragment) := "#" ++ ("&".intercalate <| q.map (λ (k, v) => s!"{k}={v}"))

def Query := List (String × String)

instance : ToString Query where
  toString (q : Query) := "?" ++ ("&".intercalate <| q.map (λ (k, v) => s!"{k}={v}"))
end URI

open URI
structure URI where
  userInfo : Option UserInfo
  host: Hostname
  port: Option UInt16
  scheme: Scheme
  path: Path
  query: Option Query
  fragment: Option Fragment

namespace URI

private def toString (uri : URI) : String :=
  s!"{uri.scheme}://"
  ++ if let some user := uri.userInfo then s!"{user}@"
  else ""
  ++ s!"{uri.host}"
  ++ if let some port := uri.port then s!":{port}"
  else ""
  ++ s!"{uri.path}"
  ++ if let some query := uri.query then s!"?{query}"
  else ""
  ++ if let some fragment := uri.fragment then s!"#{fragment}"
  else ""

instance : ToString URI := ⟨toString⟩

namespace Parser

def schemeParser : Parsec Scheme :=
  Scheme.mk <$> manyChars (asciiLetter <|> oneOf ['+', '-', '.'])

def hostName : Parsec Hostname := do
  let name := many1Chars (asciiLetter <|> digit <|> pchar '-')
  let start := name ++ pstring "."
  many1Strings start ++ name

def parseDigit! (c : Char) : Nat :=
  match c with
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | _ => panic! "Not a digit"

def parseUInt16 : Parsec UInt16 := do
  let as ← many1 digit
  let mut n := 0
  for (i, c) in as.toList.reverse.enum do
    let d := parseDigit! c
    n := n + d * 10 ^ i
  return n.toUInt16

def maybePort : Parsec (Option UInt16) := do
  option $ parseUInt16

def psegment : Parsec String :=
  many1Chars <| oneOf ['-', '%', '_', '+', '$', '.', ':', '*', '@' ] <|> asciiLetter <|> digit

partial def pathParser : Parsec Path := do
  let rec comp : Parsec Path := do
    if ← test <| pstring "/" then
      let part ← psegment
      let rest ← comp
      pure <| part :: rest
    else
      pure []
  comp

def userInfoParser : Parsec UserInfo := do
  let username ← many1Chars <| asciiLetter <|> digit
  let password ← option do skipChar ':'; many1Chars <| asciiLetter <|> digit
  skipChar '@'
  return { username, password : UserInfo }

partial def queryParser : Parsec Query := do
  skipChar '?'
  let rec entries := do
    let k ← psegment
    skipChar '='
    let v ← psegment
    if ← test $ skipChar '&' then
      pure <| (k, v) :: (← entries)
    else
      pure [(k, v)]
  entries

partial def fragmentParser : Parsec Fragment := do
  skipChar '#'
  let rec entries := do
    let k ← psegment
    skipChar '='
    let v ← psegment
    if ← test $ skipChar '&' then
      pure <| (k, v) :: (← entries)
    else
      pure [(k, v)]
  entries

def url : Parsec URI := do  
  let scheme ← schemeParser
  skipString "://"
  let userInfo ← option userInfoParser
  let host ← hostName
  let optPort ← maybePort
  let path ← pathParser
  let query ← option queryParser
  let fragment ← option fragmentParser
  return { scheme, host, port := optPort, path, query, fragment, userInfo : URI }

end Parser

def parse (s : String) : Except String URI := Parser.url.parse s


def mkSockAddr (url : URI) : IO SockAddr :=
  SockAddr.mk {
    host := url.host
    port := url.port.getD 80 |> ToString.toString
    family := inet
    type := stream
  }

end URI
