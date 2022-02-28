import Http
import Lean.Data.Json

open Http Lean

namespace IPFS

def localNode : URI := 
  match URI.parse "http://localhost:48084/" with
  | Except.ok url => url
  | Except.error err => panic! err

def CID := String
  deriving ToString
  
def 
namespace DAG

def put (data : String) : IO CID := do
  let res ← Client.post (localNode.setPath ["api", "v0", "dag", "put"]) data
  let json ← IO.ofExcept <| Json.parse <| res.body.getD ""
  return json.getObjValD "Cid" |> Json.getObjValD "/" |> Json.getStr? |> IO.ofExcept
  

def get (cid : CID) : IO Json := do
  let url := localNode.setPath ["api", "v0", "dag", "get"] |> (·.setQueryArg "arg" cid.toString)
  let res ← Client.get url
  let json ← IO.ofExcept <| Json.parse <| res.body.getD ""
  return json

end DAG
end IPFS
