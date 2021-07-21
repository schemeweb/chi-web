(define-record-printer (<URI> x out)
  (fprintf out
           "#(URI scheme=~s authority=~a path=~s query=~s fragment=~s)"
           (URI-scheme x)
           (URI-authority x)
           (URI-path x)
           (URI-query x)
           (URI-fragment x)))

(define-record-printer (<URIAuth> x out)
  (fprintf out
           "#(URIAuth host=~s~a port=~a)"
           (URIAuth-host x)
           (if (URIAuth-ipv6-host? x) "(ipv6)" "")
           (URIAuth-port x)))

(define update-uri
  (let ((unset (list 'unset)))
    (lambda (uri . key/values)
      (apply
       (lambda (#!key
                (scheme (URI-scheme uri)) (path (URI-path uri))
                (query (URI-query uri)) (fragment (URI-fragment uri))
                (auth unset) (authority unset)
                (username unset) (password unset)
                (host unset) (port unset))
         (let* ((args (list 'scheme scheme
                            'path path
                            'query query
                            'fragment fragment))
                (args (if (not (eq? auth unset))
                          (append args (list 'auth auth)) args))
                (args (if (not (eq? authority unset))
                          (append args (list 'authority authority)) args))
                (args (if (not (eq? username unset))
                          (append args (list 'username username)) args))
                (args (if (not (eq? password unset))
                          (append args (list 'password password)) args))
                (args (if (not (eq? host unset))
                          (append args (list 'host host)) args))
                (args (if (not (eq? port unset))
                          (append args (list 'port port)) args))
                )
           (apply update-uri* uri args)))
       key/values))))

(define (make-uri . key/values)
  (apply update-uri (make-URI #f #f '() #f #f) key/values))
