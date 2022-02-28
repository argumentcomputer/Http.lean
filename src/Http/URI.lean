import Std
import Http.Parsec
import Http.Types
import Socket

open Std Parsec Socket

namespace Http

namespace URI

namespace Query

def set (self : Query) (key value : String) : Query :=
  self.insert key value
  
def empty : Query := HashMap.empty

end Query

namespace Fragment

def set (self : Fragment) (key value : String) : Fragment :=
  self.insert key value
  
def empty : Fragment := HashMap.empty

end Fragment

private def toString (uri : URI) : String :=
  s!"{uri.scheme}://"
  ++ if let some user := uri.userInfo then s!"{user}@"
  else ""
  ++ s!"{uri.host}"
  ++ if let some port := uri.port then s!":{port}"
  else ""
  ++ s!"{uri.path}"
  ++ if !uri.query.isEmpty then s!"{uri.query}"
  else ""
  ++ if uri.fragment.isEmpty then s!"{uri.fragment}"
  else ""

instance : ToString URI := ⟨toString⟩

instance : Inhabited URI where
  default := {
    host := "localhost",
    scheme := "http",
    path := []
  : URI }

def setPath (uri : URI) (p : Path) : URI := { uri with path := p }

def setQueryArg (uri : URI) (key value : String) : URI :=
  { uri with query := uri.query.set key value }

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
  HashMap.ofList <$> entries

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
  HashMap.ofList <$> entries

def url : Parsec URI := do
  let scheme ← schemeParser
  skipString "://"
  let userInfo ← option userInfoParser
  let host ← hostName
  let optPort ← maybePort
  let path ← pathParser
  let query ← (Option.getD · Query.empty) <$> option queryParser
  let fragment ← (Option.getD · Fragment.empty) <$> option fragmentParser
  return { scheme, host, port := optPort, path, query, fragment, userInfo : URI }

end Parser

def parse (s : String) : Except String URI := Parser.url.parse s

end URI
end Http
