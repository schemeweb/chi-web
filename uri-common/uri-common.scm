;;
;; URI-common provides URI handling procedures for common URI schemes
;; that are based on the generic syntax such as http, https, file, ftp.
;; It also provides automatic form-urlencoded query argument
;; encoding/decoding
;;
; Copyright (c) 2008-2018, Peter Bex
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. Neither the name of the author nor the names of its
;    contributors may be used to endorse or promote products derived
;    from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.
;
; Please report bugs, suggestions and ideas to the Chicken Trac
; ticket tracking system (assign tickets to user 'sjamaan'):
; http://trac.callcc.org

(define (replace-char-with-string string old-char new-substring)
  (call-with-port
   (open-output-string)
   (lambda (out)
     (let loop ((i 0))
       (if (= i (string-length string))
           (get-output-string out)
           (let ((char (string-ref string i)))
             (if (char=? old-char char)
                 (write-string new-substring out)
                 (write-char char out))))))))

(define (string-contains-char? chars char)
  (let loop ((n (string-length chars)))
    (and (> n 0) (or (char=? char (string-ref chars (- n 1)))
                     (loop (- n 1))))))

(define (split-string-at-chars string chars)
  (let loop ((a 0) (b 0) (fields '()))
    (if (= a (string-length string))
        (reverse fields)
        (if (= b (string-length string))
            (loop b b (cons (substring string a b) fields))
            (let ((char (string-ref string b)))
              (if (string-contains-char? chars char)
                  (loop (+ b 1) (+ b 1) (cons (substring string a b) fields))
                  (loop a (+ b 1) fields)))))))

;; We could use the hostinfo egg for this, but that would be yet another
;; dependency. Besides, not all service names have a matching URI scheme
;; nor do all URI schemes have a matching service name.
(define default-ports
  '((http . 80)              ; RFC 2616
    (https . 443)            ; RFC 2818
    (shttp . 80)             ; RFC 2660
    (ftp . 21)               ; RFC 959; no official URI scheme defined
    ;; nonstandard, but could be useful
    (svn+ssh . 22)
    (svn . 3690)
    ))

;; A common URI is a generic URI plus stored decoded versions of most components
(define-record-type URI-common
  (make-URI-common generic username password host port path query fragment)
  URI-common?
  (generic   URI-common-generic   URI-common-generic-set!)
  (username  URI-common-username  URI-common-username-set!)
  (password  URI-common-password  URI-common-password-set!)
  (host      URI-common-host      URI-common-host-set!)
  (port      URI-common-port      URI-common-port-set!)
  (path      URI-common-path      URI-common-path-set!)
  (query     URI-common-query     URI-common-query-set!)
  (fragment  URI-common-fragment  URI-common-fragment-set!))

;;; Conversion procedures
(define (uri->uri-generic uri)
  (URI-common-generic uri))

(define (uri-reference u)
  (let ((u1 (generic:uri-reference u)))
    (and u1 (uri-generic->uri u1))))

(define (absolute-uri u)
  (let ((u1 (generic:absolute-uri u)))
    (and u1 (uri-generic->uri u1))))

(define (uri-generic->uri uri)
  (make-URI-common generic: uri
                   username: (decode-string* (generic:uri-username uri))
                   password: (decode-string* (generic:uri-password uri))
                   host:     (decode-string* (generic:uri-host uri))
                   port:     (generic:uri-port uri)
                   path:     (decode-path (generic:uri-path uri))
                   query:    (form-urldecode (generic:uri-query uri))
                   fragment: (decode-string* (generic:uri-fragment uri))))

(define (decode-string* s)
  (and s (uri-decode-string s)))

(define (uri->list uri . rest)
  (let-optionals rest ((userinfomap (lambda (u pw) (string-append u ":******" ))))
    (list (uri-scheme uri)
          (list (uri-auth->list uri userinfomap) (uri-path uri) (uri-query uri))
          (uri-fragment uri))))

(define (uri-auth->list uri userinfomap)
  (let ((username (uri-username uri))
        (password (uri-password uri)))
    (list (if (and username password) (userinfomap username password) #f)
          (uri-host uri)
          (uri-port uri))))

;;; Accessors and predicates
(define uri-reference? URI-common?)
(define (uri? u)
  (and (URI-common? u) (generic:uri? (URI-common-generic u))))
(define (absolute-uri? u)
  (and (URI-common? u) (generic:absolute-uri? (URI-common-generic u))))
(define (relative-ref? u)
  (and (URI-common? u) (generic:relative-ref? (URI-common-generic u))))
(define (uri-scheme u)
  (generic:uri-scheme (URI-common-generic u)))
(define uri-username URI-common-username)
(define uri-password URI-common-password)
(define uri-host     URI-common-host)
(define uri-query    URI-common-query)
(define uri-fragment URI-common-fragment)
(define (uri-default-port? uri)
  (default-port? (uri-port uri) (uri-scheme uri)))

;; Uri-path variant which performs Scheme-Based Normalization for well-known
;; "common" schemes.  See per RFC 3986, section 6.2.3
(define (uri-path uc)
  (let ((path (URI-common-path uc)))
    (if (and (not (relative-ref? uc)) ; For real URIs (not relative-refs),
             (or (null? path)         ; an empty path equals a path of "/"
                 (eq? path #f)))
        '(/ "")
        path)))

(define (uri-port uc)
  (let ((u (URI-common-generic uc)))
    (or (generic:uri-port u)
        (alist-ref (generic:uri-scheme u) default-ports))))

;;; Constructor
(define (make-uri . key/values)
  (apply update-uri (make-URI-common generic: (generic:make-uri)) key/values))

;;; Updaters
(define update-uri
  (let ((unset (list 'unset)))
    (lambda (uc #!key
                (scheme unset) (username unset) (password unset)
                (host unset) (port unset)
                (path unset) (query unset) (fragment unset))
      (let* ((uc (update-URI-common uc)) ; new copy
             (actual-scheme (if (eq? scheme unset)
                                (generic:uri-scheme (URI-common-generic uc))
                                scheme))
             (path (if (and actual-scheme (or (eq? path #f) (eq? path '())))
                       '(/ "") ; normalize path
                       path))
             (actual-port (if (eq? port unset)
                              (URI-common-port uc)
                              port)))
        (unless (and (eq? port unset) (eq? scheme unset))
          ;; Clear port if it's the default for this scheme
          (URI-common-generic-set!
           uc (generic:update-uri
               (URI-common-generic uc)
               port: (if (default-port? actual-port actual-scheme)
                         #f
                         actual-port))))
        ;; This code is ugly!
        (unless (eq? scheme unset)
          (URI-common-generic-set!
           uc (generic:update-uri (URI-common-generic uc) scheme: scheme)))
        (unless (eq? username unset)
          (URI-common-generic-set!
           uc (generic:update-uri (URI-common-generic uc)
                                  username: (encode-string* username)))
          (URI-common-username-set! uc username))
        (unless (eq? password unset)
          (URI-common-generic-set!
           uc (generic:update-uri (URI-common-generic uc)
                                  password: (encode-string* password)))
          (URI-common-password-set! uc password))
        (unless (eq? host unset)
          (URI-common-generic-set!
           uc (generic:update-uri (URI-common-generic uc)
                                  host: (encode-string* host)))
          (URI-common-host-set! uc host))
        (unless (eq? port unset)
          ;; Generic port set above - it depends on the scheme too
          (URI-common-port-set! uc port))
        (unless (eq? path unset)
          (URI-common-generic-set!
           uc (generic:update-uri (URI-common-generic uc)
                                  path: (encode-path path)))
          (URI-common-path-set! uc path))
        (unless (eq? query unset)
          (URI-common-generic-set!
           uc (generic:update-uri (URI-common-generic uc)
                                  query: (form-urlencode query)))
          (URI-common-query-set! uc query))
        (unless (eq? fragment unset)
          (URI-common-generic-set!
           uc (generic:update-uri (URI-common-generic uc)
                                  fragment: (encode-string*
                                             fragment)))
          (URI-common-fragment-set! uc fragment))
        uc))))

(define uri-encode-string generic:uri-encode-string)
(define uri-decode-string generic:uri-decode-string)
(define char-set:gen-delims generic:char-set:gen-delims)
(define char-set:sub-delims generic:char-set:sub-delims)
(define char-set:uri-reserved generic:char-set:uri-reserved)
(define char-set:uri-unreserved generic:char-set:uri-unreserved)

(define (encode-string* s . rest)
  (and s (apply uri-encode-string s rest)))

(define (default-port? port scheme)
  (eqv? port (alist-ref scheme default-ports)))

(define (path-transcoder transcode-component)
  (lambda (p)
    (and p (if (and (pair? p) (eq? '/ (car p)))
               (cons '/ (map transcode-component (cdr p)))
               (map transcode-component p)))))

(define encode-path (path-transcoder uri-encode-string))

;;; Handling of application/x-www-form-urlencoded data
;;
;; This implements both HTML 4's specification
;; (http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4.1)
;; and XHTML XForms' specification
;; (http://www.w3.org/TR/xforms/#serialize-urlencode)
;;
;; The latter is a more generalised form of the former, as it allows
;; the user to specify a custom separator character.  The HTML 4
;; spec also contains a recommendation
;; (http://www.w3.org/TR/html401/appendix/notes.html#h-B.2.2)
;; that semicolons should be used instead of ampersands as a separator.
;; However, it provides no mechanism to select the separator to use
;; when submitting a form, which makes it a pretty useless recommendation.
;; This recommendation also complicates matters on the server because one
;; would need to handle both form-generated GET query parameters and
;; hardcoded GET query parameters as specified in anchors.
;;
;; There's also a 2006 Internet-Draft by Bjoern Hoehrmann that was
;; intended to standardize this, but it was allowed to expire in 2007:
;; http://ietfreport.isoc.org/idref/draft-hoehrmann-urlencoded
;; It was different in a few ways from the x-www-form-urlencoded type.
;; For example, www-form-urlencoded only pct-encoded the chars that
;; are not allowed in a query string, whereas x-www-form-urlencoded
;; pct-encodes *all* reserved chars, regardless of whether it is
;; necessary. There are servers which do not accept input that isn't
;; fully pct-encoded so we use strictly x-www-form-urlencoded.

(define form-urlencoded-separator (make-parameter ";&"))

(define (form-urlencode alist #!key (separator (form-urlencoded-separator)))
  (and alist (not (null? alist))
      (let* ((separator-chars (->char-set separator))
             (separator-string (string-take
                                ;; Can't use separator-chars here because
                                ;; charsets have no ordering and conversion
                                ;; to string and back reorders the chars
                                 (if (string? separator)
                                     separator
                                     (char-set->string separator)) 1))
              (enc (lambda (s)
                     (replace-char-with-string
                      (uri-encode-string
                       s
                       (char-set-union
                        separator-chars
                        (char-set #\= #\+)
                        (char-set-delete
                         (char-set-complement char-set:uri-unreserved)
                         #\space)))
                      #\space
                      "+")))
              (encoded-components
               (reverse
                (fold
                 (lambda (arg query)
                   (if (not (pair? arg))
                       (error "Not a pair:" arg)
                       (case (cdr arg)
                         ((#f) query)
                         ((#t) (cons (enc (symbol->string (car arg)))
                                     query))
                         (else (cons (string-append
                                      (enc (symbol->string (car arg)))
                                      "="
                                      (enc (symbol->string (cdr arg))))
                                     query)))))
                 '() alist))))
         (and (not (null? encoded-components))
              (string-join encoded-components separator-string)))))

(define (form-urldecode query #!key (separator (form-urlencoded-separator)))
  (if query
      (map (lambda (part)
             (let ((idx (string-index part #\=))
                   (decode (lambda (s)
                             (uri-decode-string
                              (replace-char-with-string s #\+ "%20")))))
               (if idx
                   (cons (string->symbol (decode (string-take part idx)))
                         (decode (string-drop part (add1 idx))))
                   (cons (string->symbol (decode part))
                         #t))))
           (split-string-at-chars
            query
            (char-set->string (->char-set separator))))
      '())) ; _always_ provide a list interface for the query, even if not there

(define decode-path (path-transcoder uri-decode-string))

;;; Miscellaneous procedures

;; Simple convenience procedures
(define (uri->string uri . args)
  (apply generic:uri->string (URI-common-generic uri) args))

(define (wrap proc)
  (lambda args
    (uri-generic->uri (apply proc (map URI-common-generic args)))))

;; TODO: What about normalization issues here? Right now uri-relative-from
;; gives a nonempty reference when uri1 has path=() and uri2 has path=(/ "")
;; This could be considered a bug.  Same for uri->string and with port-nrs
;; However, URIs with paths updated by this egg do not have that problem.
(define uri-relative-to             (wrap generic:uri-relative-to))
(define uri-relative-from           (wrap generic:uri-relative-from))
(define uri-normalize-case          (wrap generic:uri-normalize-case))
(define uri-normalize-path-segments (wrap generic:uri-normalize-path-segments))

;; Copied from uri-generic, because we need to use our modified uri-path
;; procedure.  Alternatively, we could update the path with our own
;; path, but that's even sillier.
(define (uri-path-absolute? uri)
  (let ((path (uri-path uri)))
    (and (pair? path) (eq? '/ (car path)))))

(define (uri-path-relative? uri)
  (not (uri-path-absolute? uri)))
)
