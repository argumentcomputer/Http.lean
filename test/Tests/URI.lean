import Http

open Http Http.URI
namespace Test
#eval Parser.hostName.parse "yatima.io"
#eval Parser.pathParser.parse "/yatima.io/index.html"
#eval Parser.pathParser.parse "/"
#eval Parser.pathParser.parse "///"
#eval URI.parse "http://yatima.io/"

#eval URI.parse "http://127.0.0.1"

def port := URI.parse "http://127.0.0.1:5001/api"
#eval port
#eval port.map (·.port == some (5001 : UInt16))

#eval URI.parse "https://9876@example.com"

def userinfo := URI.parse "https://user:pass123@example.com"

#eval userinfo
#eval userinfo.map (·.userInfo == some { username := "user", password := "pass123" : UserInfo })

def frag := URI.parse "http://test.com#test=1"

#eval frag
#eval frag.map (let f := ·.fragment; f.get "test" = "1" ∧ g.get)

def complexStr := "https://my:1234@www3.complex.com:8888/dir/file.html?test=2&t2#port=1&div"
#eval toString (URI.parse complexStr) == complexStr
end Test
