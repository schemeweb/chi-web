(define-library (chi-web uri-generic)
  (export
   uri-reference make-uri update-uri update-authority
   uri-reference? uri-auth uri-authority uri-scheme uri-path uri-query
   uri-fragment uri-host uri-ipv6-host? uri-port
   uri-username uri-password
   authority? authority-host authority-ipv6-host? authority-port
   authority-username authority-password

   uri? absolute-uri absolute-uri? uri->string uri->list
   relative-ref? uri-relative-to uri-relative-from
   uri-decode-string uri-encode-string
   uri-normalize-case uri-normalize-path-segments
   uri-path-absolute? uri-path-relative?

   char-set:gen-delims char-set:sub-delims
   char-set:uri-reserved char-set:uri-unreserved)
  (import (scheme base) (scheme char) (scheme write))
  (import (srfi 1) (srfi 14))
  (cond-expand ((library (srfi 130))
                (import (srfi 130)))
               ((library (srfi 13))
                (import (srfi 13))))
  (cond-expand ((library (matchable))
                (import (matchable)))
               ((library (chibi match))
                (import (chibi match))))
  (begin (define (with-output-to-string proc)
           (call-with-port (open-output-string)
                           (lambda (out)
                             (parameterize ((current-output-port out))
                               (proc)
                               (get-output-string out))))))
  (begin (define uri-error error))
  (include "uri-generic/uri-generic.matchable.scm")
  (cond-expand
   (chicken
    (include "uri-generic/uri-generic.chicken.scm"))
   (else
    (begin (define update-uri update-uri*)
           (define make-uri make-uri*)))))
