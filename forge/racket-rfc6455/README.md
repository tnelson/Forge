# RFC 6455 WebSockets for Racket

This package, `rfc6455`, provides
[RFC 6455](http://tools.ietf.org/html/rfc6455) compatible WebSockets
server and client interfaces for Racket, building on Racket's
`web-server` collection.

Besides support for RFC 6455, the final WebSockets standard, the
package also incorporates code supporting the earlier, draft
[hybi-00](http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-00)
proposal, because several common older browsers still use this
protocol variant.

Wikipedia has a
[good section on browser support for WebSocket protocol variations](http://en.wikipedia.org/wiki/WebSocket#Browser_support),
which states "All the latest browsers except Android browser support
the latest specification (RFC 6455) of the WebSocket protocol."

This package has been developed against

 - Firefox 24.0 (which is an RFC 6455 peer)
 - Chrome 30.0.1599.101 (which is an RFC 6455 peer)
 - Safari 5.1.10 (which is a hybi-00 peer)

## Examples

See the `net/rfc6455/examples` directory.

 - `test-server.rkt` is a server using the API compatible with
   Racket's existing WebSockets support.

 - `test-service-mapper.rkt` is (roughly) the same service,
   implemented using the extended API provided by this package.

 - `client.html` and `client.js` form a simple browser-based
   WebSockets program that exercises the servers.

 - `client.rkt` is a simple Racket-based WebSockets client program
   that exercises the servers.

To run the examples,

    $ racket -l net/rfc6455/examples/test-server

or

    $ racket -l net/rfc6455/examples/test-service-mapper

and then open `client.html` in your favourite browser. It should work
served directly off the file system. Make sure to open your browser's
web console to see the effect of the WebSockets communication.

The example Racket-based WebSockets client program can be run using

    $ racket -l net/rfc6455/examples/client

## Usage Synopsis

Using the `net/websocket`-compatible interface:

```racket
(require net/rfc6455)
(ws-serve #:port 8081 (lambda (c s) (ws-send! c "Hello world!")))
```

Using an interface that supports URL path matching and WebSocket
subprotocol selection:

```racket
(require net/rfc6455)
(ws-serve* #:port 8081
           (ws-service-mapper
		    ["/test" ;; the URL path (regular expression)
			 [(subprotocol) ;; if client requests subprotocol "subprotocol"
			  (lambda (c) (ws-send! c "You requested a subprotocol"))]
		     [(#f)          ;; if client did not request any subprotocol
			  (lambda (c) (ws-send! c "You didn't explicitly request a subprotocol"))]]))
```

Creating a client connection:

```racket
(require net/rfc6455)
(require net/url)
(define c (ws-connect (string->url "ws://localhost:8081/")))
(ws-send! c "Hello world!")
```

## Installation

From within DrRacket, install the package `rfc6455`. From the
command-line,

    $ raco pkg install rfc6455

(If you are developing the package from a git checkout, see instead
the `link` and `setup` targets of the Makefile.)

## Documentation

Documentation is provided within Racket's own help system once the
package is installed.

## License

The following text applies to all the code in this package except
files marked "public domain" in the `net/rfc6455/examples` directory:

> Copyright (c) 2013 Tony Garnock-Jones <tonygarnockjones@gmail.com>.
>
> This package is distributed under the GNU Lesser General Public
> License (LGPL). This means that you can link it into proprietary
> applications, provided you follow the rules stated in the LGPL. You
> can also modify this package; if you distribute a modified version,
> you must distribute it under the terms of the LGPL, which in
> particular means that you must release the source code for the
> modified software. See <http://www.gnu.org/licenses/lgpl-3.0.txt>
> for more information.
