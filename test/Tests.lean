import Http

open Http Http.URI

#eval Parser.hostName.parse "yatima.io"
#eval Parser.pathParser.parse "/yatima.io/index.html"
#eval URI.parse "http://yatima.io/"

def main (args : List String) : IO UInt32 := do
  try
    let url ← IO.ofExcept <| URI.parse "http://yatima.io/test?1=1#a"
    println! "{url}"
    pure 0
  catch e =>
    IO.eprintln s!"error: {e}"
    pure 1

