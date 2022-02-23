import Std
namespace Http

namespace URI

def Hostname := String

deriving instance ToString for Hostname

def Scheme := String
 deriving BEq

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

def CRLF : String := "\r\n"

/--
A Case insensitive String with case insensitive BEq and Hashable instances.
-/
def CaseInsString := String
  deriving ToString

instance caseInsensitiveStringBEq : BEq CaseInsString where
  beq s1 s2 := s1.capitalize == s2.capitalize

instance caseInsensitiveStringHashable : Hashable CaseInsString where
  hash s := s.capitalize.hash

inductive Method
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH

def Method.toString: Method → String
  | GET => "GET"
  | HEAD => "HEAD"
  | POST => "POST"
  | PUT  => "PUT"
  | DELETE => "DELETE"
  | CONNECT => "CONNECT"
  | OPTIONS => "OPTIONS"
  | TRACE => "TRACE"
  | PATCH => "PATCH"

instance : ToString Method where
  toString := Method.toString

inductive Protocol
  | http (version : String)
  | https (version : String)
  | other (name : String) (version : String)

def Protocol.toString : Protocol → String
  | http v => s!"HTTP/{v}"
  | https v => s!"HTTPS/{v}"
  | other name v => s!"{name.capitalize}/{v}"

open Protocol in
def URI.Scheme.asProtocol (s : Scheme) : Protocol :=
  match ToString.toString s with
  | "http" => http "1.1"
  | "https" => https "1.2"
  | s => other s ""

instance : ToString Protocol where
  toString := Protocol.toString

/-
Meta information for Requests and Responses.
-/
def Headers := Std.HashMap CaseInsString String

structure Request where
  url : URI
  protocol : Protocol
  method : Method
  headers : Headers
  body : Option String

/-
A HTTP response from a request.
-/
structure Response where
  message : String
  protocol : Protocol
  statusCode : Nat
  headers : Headers
  body : Option String

end Http
