
namespace Http
namespace Client

inductive ClientError where
  | custom (s : String)

def ClientError.toString : ClientError → String
  | ClientError.custom s => s

instance : ToString ClientError where
  toString := ClientError.toString

inductive Client (A : Type) where
  | ok (a : A)
  | io (iom : IO A)
  | error (error : ClientError)

open Client

instance : Monad Client where
  bind a fb :=
  match a with
  | ok data => fb data
  | io iom => bind (io iom) fb
  | error e => error e
  pure a := ok a

instance : MonadLiftT Client IO where
  monadLift client :=
  match client with
  | ok data => pure data
  | error e => throw <| IO.Error.userError e.toString

instance : MonadLiftT IO Client where
  monadLift io :=
  match io with
  | Result.ok d => pure data
  | error e => throw <| IO.Error.userError e.toString

instance : MonadExcept ClientError Client where
  throw e := error (ClientError.custom e.toString)
  tryCatch ma handle :=
  match ma with
  | ok data => ma
  | error e => handle e

end Client
end Http
