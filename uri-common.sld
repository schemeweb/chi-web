(define-library (chi-web uri-common)
  (export
   uri-reference uri-reference? absolute-uri absolute-uri? relative-ref?
   uri->uri-generic uri-generic->uri uri->list
   make-uri update-uri uri? uri-scheme uri-username uri-password
   uri-host uri-port uri-path uri-query uri-fragment
   uri->string form-urlencode form-urldecode form-urlencoded-separator
   uri-relative-to uri-relative-from
   uri-normalize-path-segments uri-normalize-case
   uri-path-relative? uri-path-absolute?
   uri-default-port? uri-encode-string uri-decode-string

   char-set:gen-delims char-set:sub-delims
   char-set:uri-reserved char-set:uri-unreserved)
  (import (scheme base))
  (import (srfi 1) (srfi 14))
  (cond-expand ((library (srfi 130))
                (import (srfi 130)))
               ((library (srfi 13))
                (import (srfi 13))))
  (cond-expand (chicken (import (chicken format)))
               (else))
  (import (prefix (chi-web uri-generic) generic:))
  (include "uri-common/uri-common.scm")
  (cond-expand (chicken (include "uri-common/uri-common.chicken.scm"))
               (else)))
