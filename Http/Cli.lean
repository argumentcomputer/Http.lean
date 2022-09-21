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
      let response ← Client.get url
      println! "headers : {response.headers}"
      println! "body: {response.body}"
    | [ "--post", surl, body ] => do
      let url ← IO.ofExcept <| URI.parse surl
      let response ← Client.post url body
      println! "headers : {response.headers}"
      println! "body: {response.body}"
    | unknown => println! "Unknown arguments {unknown}"
    pure 0
  catch e =>
    IO.eprintln <| "error: " ++ toString e
    pure 1
