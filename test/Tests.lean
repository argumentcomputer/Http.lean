import Http
import Tests.IPFS

open Http Http.URI

#eval Parser.hostName.parse "yatima.io"
#eval Parser.pathParser.parse "/yatima.io/index.html"
#eval URI.parse "http://yatima.io/"

def main (args : List String) : IO UInt32 := do
  try
    let test := args.getD 0 "ipfs"
    match test with
    | "ipfs" => IPFS.test
    | u => IO.eprintln s!"unknown test {u}"
    pure 0
  catch e =>
    IO.eprintln s!"error: {e}"
    pure 1

