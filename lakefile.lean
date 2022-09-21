import Lake
open Lake DSL

package Http

@[defaultTarget]
lean_exe http {
  root := `Main
}

@[defaultTarget]
lean_lib Http

require Socket from git
  "https://github.com/yatima-inc/Socket.lean" @ "c47fe71cde6b79a40903563cc56c0889bd5ca220"

-- require OpenSSL from git
--   "https://github.com/yatima-inc/OpenSSL.lean" @ "7187dab2f60097194167dbfa5afd862c276f4cd7"
