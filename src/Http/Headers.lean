import Http.Types
import Http.Parsec
import Std

namespace Http.Headers
open Std

protected def toString (h : Headers) : String :=
  h.fold (λ acc a b => acc ++ s!"{a}: {b}, ") ""

instance : ToString Headers where
  toString := Headers.toString
  
def toRequestFormat (h : Headers) : String :=
  h.fold (λ acc a b => acc ++ s!"{a}: {b}" ++ CRLF) ""

def set (self : Headers) (name : CaseInsString) (value : String) : Headers :=
  self.insert name value
  
def merge (a b : Headers) : Headers :=
  b.fold (λ a k v => a.set k v) a
  
def fromList (l : List (CaseInsString × String)) : Headers :=
  l.foldl (λ h (n, v) => h.set n v) HashMap.empty

namespace Parser

open Parsec

def header : Parsec (CaseInsString × String) := do
  let key ← many1Chars (asciiLetter <|> pchar '-')
  ws
  skipChar ':'
  ws  
  let value ← manyChars <| satisfy (λ c => c != '\n')
  ws
  return (key, value)

def headers : Parsec Headers := do
  let headers : HashMap CaseInsString String
    ← Array.foldl (λ map (k ,v) => map.insert k v) HashMap.empty <$> (many header)
  ws
  return headers

end Parser
end Http.Headers
