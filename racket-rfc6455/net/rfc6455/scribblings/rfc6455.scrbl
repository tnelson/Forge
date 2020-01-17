#lang scribble/manual

@(require scribble/manual
	  (for-label racket
		     net/url
		     web-server/http/request-structs
		     net/rfc6455))

@title[#:version "2.1.0"]{RFC 6455 WebSockets for Racket}
@author[(author+email "Tony Garnock-Jones" "tonygarnockjones@gmail.com")]

@;@local-table-of-contents[]

@section{Introduction}

This package, @tt{rfc6455}, provides
@link["http://tools.ietf.org/html/rfc6455"]{RFC 6455} compatible
WebSockets server and client interfaces for Racket, building on Racket's
@racket[web-server] collection.

Besides support for RFC 6455, the final WebSockets standard, the
package also incorporates code supporting the earlier, draft
@link["http://tools.ietf.org/html/draft-ietf-hybi-thewebsocketprotocol-00"]{hybi-00}
proposal, because several common older browsers still use this
protocol variant.

Wikipedia has a
@link["http://en.wikipedia.org/wiki/WebSocket#Browser_implementation"]{section
on browser support for WebSocket protocol variations}. In 2013, at the
time this manual was written, it stated that "all the latest browsers
except Android browser support the latest specification (RFC 6455) of
the WebSocket protocol." For up-to-date, detailed information on
browser support for WebSockets, see
@hyperlink["https://caniuse.com/#search=WebSockets"]{caniuse.com}.

This package has been developed against

@itemize[
  @item{Firefox 24.0 (which is an RFC 6455 peer)}
  @item{Chrome 30.0.1599.101 (which is an RFC 6455 peer)}
  @item{Safari 5.1.10 (which is a hybi-00 peer)}
  ]

@(define (tech:event)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") "synchronizable event"))
@(define (tech:result)
   (tech #:doc '(lib "scribblings/reference/reference.scrbl") "synchronization result"))

@section{Changes}

Version 2.1.0 of this library introduces @racket[ws-conn-close-status]
and @racket[ws-conn-close-reason] for retrieving information from
close frames sent by remote peers.

Version 2.0.0 of this library introduces a new interface to streaming
message reception, @racket[ws-recv-stream], and makes a breaking
change to the way @racket[ws-conn?] values work as @(tech:event)s. The
@(tech:result) of such an event is now a received message, where
previously it was an uninteresting value. See the (new) procedure
@racket[ws-recv-evt].

In addition, prior to version 2.0.0, the default
@racket[#:payload-type] for @racket[ws-recv] was actually
@racket['text], even though the documentation claimed that it was
@racket['auto]. From version 2.0.0, the default is @racket['auto] in
both the documentation and the implementation.

Version 1.0.1 of this library is the last version with the old
@racket[#:stream?] interface to streaming receives. It also has an
interface bug: when (1) using a @racket[ws-conn?] as an
@(tech:event), (2) with an event-handler that calls
@racket[ws-recv], (3) connected to a client that occasionally sends
websocket "ping" frames, the call to @racket[ws-recv] may get stuck
waiting for the next non-"ping" frame, leading to a kind of temporary
stall on the thread invoking @racket[sync]. This bug was the chief
motivation for the changes of version 2.0.0.

@section{Synopsis}

Using the legacy @code{net/websocket}-compatible interface:

@racketblock[
  (require net/rfc6455)
  (ws-serve #:port 8081 (lambda (c s) (ws-send! c "Hello world!")))]

Using an interface that supports URL path matching and WebSocket
subprotocol selection:

@racketblock[
  (require net/rfc6455)
  (ws-serve* #:port 8081
	     (ws-service-mapper
	      ["/test" (code:comment "the URL path (regular expression)")
	       [(subprotocol) (code:comment "if client requests subprotocol \"subprotocol\"")
		(lambda (c) (ws-send! c "You requested a subprotocol"))]
	       [(#f) (code:comment "if client did not request any subprotocol")
		(lambda (c) (ws-send! c "You didn't explicitly request a subprotocol"))]]))]

Creating a client connection:

@racketblock[
  (require net/rfc6455)
  (require net/url)
  (define c (ws-connect (string->url "ws://localhost:8081/")))
  (ws-send! c "Hello world!")]

@section{License}

All the code in this package is licensed under the LGPL, version 3.0
or any later version. Each source file has a brief copyright and
licensing notice attached, but see
@link["http://www.gnu.org/licenses/lgpl-3.0.txt"]{the licence text
itself} for full details.

The only exceptions to the above are the files marked "public domain"
in the @tt{net/rfc6455/examples} directory. They are intended to be
examples of usage of the package for people to build on without
concern for licensing minutiae.

@section{API}

@defmodule[net/rfc6455]

The interface is based on the @code{net/websocket} API from older
versions of Racket, with some extensions and differences.

@defproc[(ws-url? [x any/c]) boolean?]{

Returns true if and only if @racket[x] is a @racket[url?] and has a
@racket[url-scheme] equal to @racket["ws"] or @racket["wss"].

}

@defproc[(wss-url? [x any/c]) boolean?]{

Returns true if and only if @racket[x] is a @racket[url?] and has a
@racket[url-scheme] equal to @racket["wss"].

}

@defproc[(ws-conn? [x any/c]) boolean?]{

Returns @racket[#t] if and only if @racket[x] is a WebSocket
connection as defined by this package.

}

@defproc[(ws-conn-supports-fragmentation? [c ws-conn?]) boolean?]{

@racket[#t] iff the connected peer supports sending and receiving
fragmented/streamed messages. Currently, RFC 6455-compliant peers
support fragmentation, while hybi-00 peers do not.

}

@defproc[(ws-conn-supports-payload-type? [c ws-conn?] [payload-type symbol?]) boolean?]{

@racket[#t] iff the connected peer supports the requested payload
type. RFC 6455-compliant peers support types @racket['text] and
@racket['binary], while hybi-00 peers support only @racket['text].

}

@defproc[(ws-conn-signals-status-on-close? [c ws-conn?]) boolean?]{

@racket[#t] iff the @racket[status] and/or @racket[reason] values are
communicated to the remote peer on connection
close (@racket[ws-close!]). RFC 6455 includes space in the wire
protocol for these values, while hybi-00 does not.

}

@defproc[(ws-conn-closed? [c ws-conn?]) boolean?]{

Returns @racket[#t] if the given connection has been closed, and
@racket[#f] otherwise.

}

@deftogether[(@defproc[(ws-conn-close-status [c ws-conn?]) (or/c #f number?)]
              @defproc[(ws-conn-close-reason [c ws-conn?]) (or/c #f string?)])]{

When @racket[ws-conn-closed?] returns @racket[#t], these procedures
will respectively retrieve the "status code" and "reason" text from
the close frame sent by the remote peer that caused the connection
shutdown. If no such information is available, they will return
@racket[#f]. Only RFC 6455 peers send information in their close
frames; hybi-00 connections will always yield @racket[#f] from these
procedures.

}

@defproc[(ws-connect [u (or/c ws-url? wss-url?)]
		     [#:headers headers (listof header?) '()]
		     [#:protocol protocol (or/c 'rfc6455 'hybi00) 'rfc6455])
	 ws-conn?]{

Connect to the given WebSockets server, via TLS if @racket[(wss-url?
u)]. Supplies @racket[headers] as additional headers in the WebSocket
handshake request. A protocol variant other than the RFC 6455 standard
can be chosen by supplying a value for the @racket[#:protocol]
parameter.

}

@defproc[(ws-serve [conn-dispatch (-> ws-conn? request? void)]
		   [#:conn-headers conn-headers
				   (or/c (-> bytes? (listof header?) request?
					     (values (listof header?) any/c))
					 (-> bytes? (listof header?)
					     (values (listof header?) any/c)))]
		   ...)
	 (-> void)]{

This is a convenience entry point, largely directly compatible with
@code{ws-serve} from the built-in websocket support in older versions
of Racket, including all its arguments besides those
shown. If @racket[#:conn-headers] is supplied, then it is inspected to
see whether it takes two or three arguments, and is called
appropriately.}

@defproc[(ws-serve* [service-mapper (-> url? (-> (or/c symbol? #f) (or/c #f
									 (-> ws-conn? void))))]
		    ...)
	 (-> void)]

Like @racket[ws-serve], except uses the given @racket[service-mapper]
to decide how to handle an incoming request. See
@racket[ws-service-mapper].

@defform[(ws-service-mapper [uri-regexp [(protocol ...) function-expr] ...] ...)
	 #:grammar ([protocol symbol #f])]{

Macro that expands to an expression suitable for use as a
service-mapper with @racket[ws-serve*].

Each @racket[uri-regexp] is matched against an incoming request's URL
in turn until one matches. Then,

@itemize[

  @item{if the client supplied a @tt{Sec-WebSocket-Protocol} header,
each token from that header is checked against the @racket[protocol]s
in turn. If one matches, the corresponding @racket[function-expr] is
used as the connection handler; or,
}

  @item{if no such header was supplied, the first
@racket[function-expr] with a literal @racket[#f] among its
@racket[protocol]s is used.}

]

The @racket[function-expr]s must evaluate to connection handler
procedures, each taking a @racket[ws-conn?] as their only argument.

}

@defproc[(ws-send! [c ws-conn?]
		   [payload (or/c string? bytes? input-port?)]
		   [#:final-fragment? final-fragment? boolean? #t]
		   [#:payload-type payload-type (or/c 'continuation 'text 'binary) 'text]
		   [#:flush? flush? boolean? #t])
	 void?]{

Sends a message to the remote peer.

(Note: Only RFC 6455 peers support fragmentation and non-text
payloads. Attempts to use these features with hybi-00 peers will
signal an error. See @racket[ws-conn-supports-fragmentation?] and
@racket[ws-conn-supports-payload-type?].)

If @racket[payload] is a string, it is converted to bytes using
@racket[string->bytes/utf-8] before transmission. If it is an
input-port, it is read from and streamed using multiple WebSockets
message fragments to the peer until it yields @racket[eof] (see also
@racket[rfc6455-stream-buffer-size]).

If @racket[flush?] is false, the buffers of the underlying connection
socket output-ports are not flushed after sending the message.
Otherwise (i.e. by default), they are flushed.

If @racket[payload-type] is @racket['text] or @racket['binary], the
appropriate WebSockets content type bit is set upon transmission.

Fragmented messages can be sent using this procedure.

@itemize[

  @item{The first fragment in a sequence must have
@racket[payload-type] set to @racket['text] or @racket['binary]. Every
subsequent fragment in the same sequence must have
@racket[payload-type] set to @racket['continuation].}

  @item{The final fragment in a sequence must have
@racket[final-fragment?] set to a non-false value. Every other
fragment in a sequence must have @racket[final-fragment?] set to
@racket[#f].}

]

For single-fragment (unfragmented) messages, the defaults are fine: a
plain @racket[(ws-send! c payload)] is enough. Here is an example of a
multi-fragment message:

@racketblock[(ws-send! c #"first" #:final-fragment? #f)
	     (ws-send! c #"second" #:final-fragment? #f #:payload-type 'continuation)
	     (ws-send! c #"third" #:final-fragment? #t #:payload-type 'continuation)]

}

@defproc[(ws-recv [c ws-conn?]
		  [#:payload-type payload-type (or/c 'auto 'text 'binary) 'auto])
	 (or/c eof-object? string? bytes?)]{

Receives a message from the remote peer.

(Note: Only RFC 6455 peers support binary payloads. Attempts to use
@racket['binary] payload-type with hybi-00 peers will signal an error.
See @racket[ws-conn-supports-payload-type?].)

Returns either a string or a bytes, depending on
@racket[payload-type]. If a specific @racket['text] or
@racket['binary] payload type is requested, the corresponding result
type is returned, or if @racket['auto] is requested, the message's own
text/binary indicator bit is used to decide which to return. If
@racket[eof] occurs mid-message, fragments so far received are
discarded and @racket[eof] is returned.

Multi-fragment messages are transparently reassembled into a single
string or bytes.

}

@defproc[(ws-recv-evt [c ws-conn?]
		      [#:payload-type payload-type (or/c 'auto 'text 'binary) 'auto])
	 evt?]{

Produces a @(tech:event) that effectively gives an asynchronous
version of @racket[ws-recv]. The @(tech:result) of the event is the
received message (either a string or a bytes, just the same as the way
@racket[ws-recv] works) or @racket[eof].

The same caveats regarding @racket[payload-type] apply here as for
@racket[ws-recv].

A @racket[ws-conn?] value may be used as if it were an event produced
by @racket[ws-recv-evt]: using @racket[c] alone as an event is
equivalent to using @racket[(ws-recv-evt c)].

}

@defproc[(ws-recv-stream [c ws-conn?]) input-port?]{

Receives a message from the remote peer, making the contents of the
message available through an input port.

(Note: Only RFC 6455 peers support streaming. Attempts to use this
procedure with hybi-00 peers will signal an error. See
@racket[ws-conn-supports-fragmentation?].)

Returns an input port from which the bytes or characters making up the
message can be read. An end-of-file from the resulting input port is
ambiguous: it does not separate the end of the message being read from
the end of the connection itself. Use @racket[ws-conn-closed?] to
disambiguate, though beware of
@hyperlink["https://en.wikipedia.org/wiki/Time_of_check_to_time_of_use"]{time-of-check-to-time-of-use}
issues.

Multi-fragment messages are transparently reassembled. Fragment
boundaries are not preserved when reading from the returned input
port.

}

@defproc[(ws-close! [c ws-conn?]
		    [#:status status integer? 1000]
		    [#:reason reason string? ""])
	 void?]{

Closes a connection. Has no effect if the connection is already
closed. The status code and reason are supplied to the remote peer in
the close frame.

(Note: hybi-00 peers do not have room in their wire protocol for the
status and reason codes. See
@racket[ws-conn-signals-status-on-close?].)

}

@defparam[rfc6455-stream-buffer-size size integer?]{

Used when streaming the contents of an input-port given to
@racket[ws-send!]. Streamed message fragments will be no larger than
@racket[(rfc6455-stream-buffer-size)] bytes each.

}

@defparam[hybi00-framing-mode mode (or/c 'new 'old)]{

Used with @emph{pre-}hybi-00 peers. Selects either the "new" or "old"
framing modes. You only have to worry about this if you're trying to
communicate with truly ancient, pre-hybi-00 peers, and then you no
doubt have bigger problems.

}

@defparam[ws-idle-timeout seconds number?]{

Idle timeout in seconds. If the interval between successive received
frames (of any type) exceeds this number of seconds, the connection
will be closed.

This parameter defaults to 300 seconds, i.e. five minutes.

}
