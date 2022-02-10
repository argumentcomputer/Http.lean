
namespace Parsec

structure Pos where
  it : String.Iterator
  line : Nat := 1
  lineOffset : Nat := 0
  deriving Repr, BEq

/-
Result which keeps track of the parsing state.
-/
inductive ParseResult (E : Type) (α : Type) where
  | success (pos : Pos) (res : α)
  | error (pos : Pos) (err : E)
  deriving Repr

end Parsec

/-
A function which converts an iterator to a ParseResult
-/
def Parsec (α : Type) : Type := Parsec.Pos → Parsec.ParseResult String α

namespace Parsec

open ParseResult

instance (α : Type) : Inhabited (Parsec α) :=
  ⟨λ pos => error pos ""⟩

@[inline]
protected def pure (a : α) : Parsec α := λ pos =>
 success pos a

@[inline]
def bind {α β : Type} (f : Parsec α) (g : α → Parsec β) : Parsec β := λ pos =>
  match f pos with
  | success rem a => g a rem
  | error pos msg => error pos msg

instance : Monad Parsec :=
  { pure := Parsec.pure, bind }

@[inline]
def map {α β : Type} (f : α → β) (p : Parsec α) : Parsec β := p >>= pure ∘ f

@[inline]
def andAppend {α : Type} [Append α] (f : Parsec α) (g : Parsec α) : Parsec α := do 
  let a ← f
  let b ← g
  return a ++ b

@[inline]
def andHAppend {A B C : Type} [HAppend A B C] (f : Parsec A) (g : Parsec B) : Parsec C := do 
  let a ← f
  let b ← g
  return a ++ b

instance {α : Type} [Append α] : Append $ Parsec α := ⟨andAppend⟩
instance {A B C : Type} [HAppend A B C] : HAppend (Parsec A) (Parsec B) (Parsec C) := ⟨andHAppend⟩

@[inline]
def fail (msg : String) : Parsec α := fun pos =>
  error pos msg

@[inline]
def never : Parsec Unit := fun pos => error pos ""

/-
Combine two parsers into one where the first takes presedence
and the second is tried if the first one fails.
-/
@[inline]
def orElse (p : Parsec α) (q : Unit → Parsec α) : Parsec α := fun pos =>
  match p pos with
  | success rem a => success rem a
  | error rem err => 
    match q () pos with
    | error rem2 err2 =>
      -- Forward the error of the longest match
      if rem.it.i = rem2.it.i then
        error rem s!"{err} or {err2}"
      else if rem.it.i > rem2.it.i then
        error rem err
      else
        error rem2 err2
    | success rem a => success rem a


def isNewline (c : Char) : Bool :=
  c = '\n'

def nextPosIt (pos : Pos) : Pos :=
  if isNewline pos.it.curr then
    {pos with it := pos.it.next, lineOffset := 0, line := pos.line + 1 }
  else
    {pos with it := pos.it.next, lineOffset := pos.lineOffset + 1 }

def getPos : Parsec (Nat × Nat) := λ pos => success pos (pos.line, pos.lineOffset)

/-
Convert errors to none
-/
def option (p : Parsec α) : Parsec $ Option α := fun pos =>
  match p pos with
  | success rem a => success rem (some a)
  | error rem err => success pos (none)

/-
Try to match but rewind iterator if failure and return success bool
-/
def test (p : Parsec α) : Parsec Bool := fun pos =>
  match p pos with
  | success rem a => success rem true
  | error rem err => success pos false

/-
Rewind the iterator on failure
-/
@[inline]
def attempt (p : Parsec α) : Parsec α := λ pos =>
  match p pos with
  | success rem res => success rem res
  | error _ err => error pos err

instance : Alternative Parsec :=
{ failure := fail "", orElse }

def expectedEndOfInput := "expected end of input"

@[inline]
def eof : Parsec Unit := fun pos =>
  if pos.it.hasNext then
    error pos expectedEndOfInput
  else
    success pos ()

@[inline]
partial def manyCore (p : Parsec α) (acc : Array α) : Parsec $ Array α := do
  if let some res ← option p then
    manyCore p (acc.push $ res)
  else
    pure acc

@[inline]
def many (p : Parsec α) : Parsec $ Array α := manyCore p #[]

@[inline]
def many1 (p : Parsec α) : Parsec $ Array α := do manyCore p #[←p]

@[inline]
partial def manyCharsCore (p : Parsec Char) (acc : String) : Parsec String := do
  if let some res ← option p then
    manyCharsCore p (acc.push $ res)
  else
    pure acc

/-
Zero or more matching chars
-/
@[inline]
def manyChars (p : Parsec Char) : Parsec String := manyCharsCore p ""

/-
One or more matching chars
-/
@[inline]
def many1Chars (p : Parsec Char) : Parsec String := do manyCharsCore p (←p).toString

@[inline]
partial def manyStringsCore (p : Parsec String) (acc : String) : Parsec String :=
  (do manyStringsCore p (acc.append $ ←p))
  <|> pure acc

/-
One or more matching chars
-/
@[inline]
def many1Strings (p : Parsec String) : Parsec String := do
  manyStringsCore p (←p)

/-
Zero or more matching Strings
-/
@[inline]
def manyStrings (p : Parsec String) : Parsec String := manyStringsCore p ""

def pstring (s : String) : Parsec String := λ pos =>
  let substr := pos.it.extract (pos.it.forward s.length)
  if substr = s then
    let it := pos.it.forward s.length
    success ({pos with it}) substr
  else
    error pos s!"expected: {s}"

@[inline]
def skipString (s : String) : Parsec Unit := pstring s *> pure ()

def unexpectedEndOfInput := "unexpected end of input"

@[inline]
def anyChar : Parsec Char := λ pos =>
  if pos.it.hasNext then
    success (nextPosIt pos) pos.it.curr
  else
    error pos unexpectedEndOfInput

@[inline]
def pchar (c : Char) : Parsec Char := attempt do
  if (←anyChar) = c then pure c else fail s!"expected: '{c}'"

@[inline]
def skipChar (c : Char) : Parsec Unit := pchar c *> pure ()

@[inline]
def digit : Parsec Char := attempt do
  let c ← anyChar
  if '0' ≤ c ∧ c ≤ '9' then pure c else fail s!"digit expected"

@[inline]
def hexDigit : Parsec Char := attempt do
  let c ← anyChar
  if ('0' ≤ c ∧ c ≤ '9')
   ∨ ('a' ≤ c ∧ c ≤ 'a')
   ∨ ('A' ≤ c ∧ c ≤ 'A') then pure c else fail s!"hex digit expected"

@[inline]
def asciiLetter : Parsec Char := attempt do
  let c ← anyChar
  if ('A' ≤ c ∧ c ≤ 'Z') ∨ ('a' ≤ c ∧ c ≤ 'z') then pure c else fail s!"ASCII letter expected"

@[inline]
def symbol : Parsec String := attempt do
  let c ← asciiLetter
  let rest ← manyChars (asciiLetter <|> digit)
  return s!"{c}{rest}"


@[inline]
def satisfy (p : Char → Bool) (msg : String := "condition not satisfied") : Parsec Char := attempt do
  let c ← anyChar
  if p c then pure c else fail msg

@[inline]
def notFollowedBy (p : Parsec α) : Parsec Unit := λ pos =>
  match p pos with
  | success _ _ => error pos "unexpected symbol"
  | error _ _ => success pos ()

def isWhitespace (c : Char) : Bool :=
  c = '\u0009' ∨ c = '\u000a' ∨ c = '\u000d' ∨ c = '\u0020'

/-
Non strict whitespace
-/
partial def skipWs : Parsec Unit := λ pos =>
  let c := pos.it.curr
  if pos.it.hasNext && isWhitespace c then
    skipWs <| nextPosIt pos
  else
    success pos ()

@[inline]
def peek? : Parsec (Option Char) := fun pos =>
  if pos.it.hasNext then
    success (nextPosIt pos) pos.it.curr
  else
    success pos none

@[inline]
def peek! : Parsec Char := do
  let some c ← peek? | fail unexpectedEndOfInput
  pure c

@[inline]
def skip : Parsec Unit := fun pos =>
  success pos ()

/-
Zero or more whitespaces
-/
@[inline]
def ws : Parsec Unit := skipWs

/-
One or more whitespaces
-/
def wsStrict : Parsec Unit := do
  _ ← satisfy isWhitespace
  ws

def parse {A: Type} (p: Parsec A) (s : String) : Except String A :=
  match p { it := s.mkIterator : Parsec.Pos } with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error pos err  => Except.error s!"{err} ({pos.line}:{pos.lineOffset})"

end Parsec
