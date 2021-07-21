(define-record-printer (URI-common x out)
  (fprintf
   out
   "#<URI-common: scheme=~S port=~S host=~S path=~S query=~S fragment=~S>"
   (uri-scheme x) (uri-port x) (uri-host x)
   (uri-path x) (uri-query x) (uri-fragment x)))
