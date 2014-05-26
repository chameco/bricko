(module bricko (http-get http-post fetch-contents get-attr get-by-id)
 (import chicken scheme (srfi 1))
 (use http-client
      html-parser)

 (define (debug x)
   (display x)
   (newline)
   x)

 (define (identity x) x)

 (define-syntax if-call
   (syntax-rules
     (=>)
     [(if-call pred test => result)
      (let [(temp test)]
        (if (pred temp) (result temp) #f))]))

 (define (http-get url)
   (with-input-from-request url #f html->sxml))

 (define (http-post url data)
   (with-input-from-request url data html->sxml))

 (define (call-on-match f node-spec l)
   (if (null? l)
     l
     (if (list? node-spec)
       (call-on-match f
                      (if (and (list? (car l)) (eq? (car node-spec) (caar l)))
                        (cond [(> (cadr node-spec) 1) (list (car node-spec) (- (cadr node-spec) 1))]
                              [(= (cadr node-spec) 1) (car node-spec)]
                              [else (error "Invalid node index in specification" node-spec)])
                        node-spec)
                      (cdr l))
       (if (and (list? (car l)) (eq? node-spec (caar l)))
         (f (car l))
         (call-on-match f node-spec (cdr l))))))

 (define (fetch-contents node-list tree)
   (if (null? node-list)
     tree
     (call-on-match (lambda (leaf) (fetch-contents (cdr node-list) leaf))
                    (car node-list)
                    (cdr tree))))

 (define (get-attr attr tree)
   (call-on-match (lambda (attrs)
                    (let ([r (assv attr (cdr attrs))])
                      (if r (cadr r) #f)))
                  '@
                  (cdr tree)))

 (define (leaf-with-attr attr value tree)
   (let loop ([l (cdr tree)])
     (if (null? l)
       #f
       (if (equal? (get-attr attr (car l)) value)
         (car l)
         (loop (cdr l))))))

 (define (get-by-id id-value tree)
   (if (null? tree)
     #f
     (let ([root-id (get-attr 'id tree)])
       (if (equal? root-id id-value)
         tree
         (if-call (lambda (x) (not (null? x)))
                  (filter identity (map (lambda (leaf) (get-by-id id-value leaf))
                                        (filter (lambda (x) (and (list? x) (not (eq? (car x) '@)))) (cdr tree))))
                  => car))))))
