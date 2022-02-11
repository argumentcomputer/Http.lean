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

def Path := String

deriving instance ToString for Path 

def Userinfo := String
deriving instance ToString for Userinfo

def Fragment := Std.HashMap String String

instance : ToString Fragment where
  toString (q : Fragment) := ""

def Query := Std.HashMap String String

instance : ToString Query where
  toString (q : Query) := ""

end URI

open URI
structure URI where
  userinfo : Option Userinfo
  host: Hostname
  port: Option UInt16
  scheme: Scheme
  path: Path
  query: Option Query
  fragment: Option Fragment

namespace URI

private def toString (uri : URI) : String :=
  s!"{uri.scheme}://"
  ++ if let some user := uri.userinfo then s!"{user}@"
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

def schemeParser : Parsec Scheme := do
  skipString "http"
  return Scheme.mk "http"

def hostName : Parsec Hostname := do
  let name := many1Chars (asciiLetter <|> digit)
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

def pathParser : Parsec Path := do
  let psegment := digit <|> asciiLetter
  let comp := pstring "/" ++ manyChars psegment
  manyStrings comp

partial def queryParser : Parsec Query := do
  skipChar '?'
  let psegment := digit <|> asciiLetter
  let rec entries := λ map : HashMap String String => do
    let k ← many1Chars psegment
    skipChar '='
    let v ← many1Chars psegment
    let map := HashMap.insert map k v
    if ← test $ skipChar '&' then
      entries map
    else
      pure map
  entries mkHashMap

partial def fragmentParser : Parsec Fragment := do
  skipChar '#'
  let psegment := digit <|> asciiLetter
  let rec entries := λ map : HashMap String String => do
    let k ← many1Chars psegment
    skipChar '='
    let v ← many1Chars psegment
    let map := HashMap.insert map k v
    if ← test $ skipChar '&' then
      entries map
    else
      pure map
  entries mkHashMap

def url : Parsec URI := do
  let scheme ← schemeParser
  skipString "://"
  let userinfo := none
  let host ← hostName
  let optPort ← maybePort
  let path ← pathParser
  let query ← queryParser
  let fragment ← fragmentParser
  return { scheme, host, port := optPort, path, query, fragment, userinfo : URI }

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
