# Small Web server in Erlang
Not a complete web server. Can't reply to quesries, yet.

## Project structure
**http.erl**: HTTP/1.1 request parser. Can parse GET request only. Can reply with **http:ok** and make get request with **http:get**
**rudy.erl**: basic rudy server that waits for incoming request, delivers a reply and terminates.

## HTTP parser
Not a complete parser. Done to understand how HTTP is defined.
Parser for HTTP GET request only.

## Request

### request line
Structure : **Method SP Request-URI SP HTTP-version CRLF**

## Socket API
### Basics
1. Listen to Port on a socket
2. Accept incoming request - open communication channel with the Client
3. Receive input from the open connection
4. Send back reply if needed
5. Close receiving connection
6. Close listening socket
