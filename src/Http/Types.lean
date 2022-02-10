namespace Http

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
  | PUT => "PUT"
  | DELETE => "DELETE"
  | CONNECT => "CONNECT"
  | OPTIONS => "OPTIONS"
  | TRACE => "TRACE"
  | PATCH => "PATCH"

instance : ToString Method where
  toString := Method.toString

structure Request where
  url : URI
  method : Method
  payload : Option String

end Http
