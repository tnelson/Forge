#lang racket/base
;; Main exports.

;; Copyright (c) 2013 Tony Garnock-Jones
;;
;; This module is distributed under the GNU Lesser General Public
;; License (LGPL). This means that you can link it into proprietary
;; applications, provided you follow the rules stated in the LGPL. You
;; can also modify this module; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software. See http://www.gnu.org/licenses/lgpl-3.0.txt for
;; more information.

(require "rfc6455/service-mapper.rkt")
(require "rfc6455/server.rkt")
(require "rfc6455/client.rkt")
(require "rfc6455/url.rkt")
(require (only-in "rfc6455/timeout.rkt" ws-idle-timeout))
(require (only-in "rfc6455/rfc6455/conn.rkt" rfc6455-stream-buffer-size))
(require (only-in "rfc6455/hybi00/conn.rkt" hybi00-framing-mode))

(provide ws-service-mapper
	 (all-from-out "rfc6455/server.rkt")
	 (all-from-out "rfc6455/client.rkt")
	 (all-from-out "rfc6455/url.rkt")
	 rfc6455-stream-buffer-size
	 hybi00-framing-mode
	 ws-idle-timeout)
