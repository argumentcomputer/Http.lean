import Http
import OpenSSL

open OpenSSL
open Http

def main (args : List String) : IO UInt32 := do
  try
    let ctx ← Context.init ()
    let ssl ← ctx.initSSL
    
    match args with
    | [ "--get", surl ] => do
      let url ← IO.ofExcept <| URI.parse surl
      let data ← getRaw url
      let text := String.fromUTF8Unchecked data
      IO.println text
    | unknown => println! "Unknown arguments {unknown}"
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e
    pure 1
