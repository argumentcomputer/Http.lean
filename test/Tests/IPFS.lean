import Http
import Lean.Data.Json

open Http Lean

namespace IPFS

def localNode : URI := 
  match URI.parse "http://localhost:5001/" with
  | Except.ok url => url
  | Except.error err => panic! err

#eval URI.parse "http://localhost:5001/"
#eval localNode

def CID := String
  deriving ToString

namespace DAG

def put (data : String) : IO CID := do
  let res ← Client.post (localNode.setPath ["api", "v0", "dag", "put"]) data
  let json ← IO.ofExcept <| Json.parse <| res.body.getD ""
  json.getObjValD "Cid" |> (·.getObjValD "/") |> Json.getStr? |> IO.ofExcept
  

def get (cid : CID) : IO Json := do
  let url := localNode.setPath ["api", "v0", "dag", "get"] |> (·.setQueryArg "arg" <| toString cid)
  println! url
  let res ← Client.get url
  println! res
  if res.statusCode = 200 then
    let json ← IO.ofExcept <| Json.parse <| res.body.getD ""
    return json
  else
    throw <| IO.Error.userError <| toString res

end DAG

def test : IO Unit := do
  let cid : CID := "bafyreib3h3z3a5jwjcthjojoqjpzrlzly53ycpctnmfsijdk3qb5m3qcdq"
  let dag ← DAG.get cid
  println! "get CID : {cid}\n{dag}"

end IPFS
