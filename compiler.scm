(define void?
    (lambda (a)
        (equal? a void-object)))


(define void-object
  (if #f 'ori))

(define string-downcase
  (letrec ((list-downcase
            (lambda (ls)
              (if (null? ls)
                  '()
                  (cons (char-downcase (car ls)) (list-downcase (cdr ls)))))))
  (lambda (s)
    (list->string (list-downcase (string->list s))))))



;;; compiler.scm
;;;
;;; Programmer: ???

;;; general support routines

(define with (lambda (s f) (apply f s)))

(define member?
  (lambda (a s)
    (ormap
     (lambda (b) (eq? a b))
     s)))

(define file->list
  (lambda (filename)
    (let ((port (open-input-file filename)))
      (letrec ((loop
        (lambda ()
          (let ((ch (read-char port)))
            (if (eof-object? ch) '()
            (cons ch (loop)))))))
    (let ((s (loop)))
      (close-input-port port)
      s)))))

(define make-char-between?
  (lambda (char<=?)
    (lambda (char-from char-to)
      (lambda (char)
    (and (char<=? char-from char)
         (char<=? char char-to))))))

;;; The scanner recognizes parenthesis and single quote.
;;; It knows to ignore comments up to the end of the current input line,
;;; as well as whitespaces.

(define list->tokens
  (letrec ((st-init
        (lambda (s)
          (cond
           ((null? s) '())
           ((char=? (car s) #\;) (st-comment s))
           ((char=? (car s) #\.) `((dot) ,@(st-init (cdr s))))
           ((char=? (car s) #\') `((single-quote) ,@(st-init (cdr s))))
           ((char=? (car s) #\`) `((quasiquote) ,@(st-init (cdr s))))
           ((char=? (car s) #\,) (st-unquote (cdr s)))
           ((char=? (car s) #\() `((lparen) ,@(st-init (cdr s))))
           ((char=? (car s) #\)) `((rparen) ,@(st-init (cdr s))))
           ((char=? (car s) #\#) (st-hash (cdr s)))
           ((char=? (car s) #\") (st-string (cdr s) '()))
           ((char-whitespace? (car s)) (st-init (cdr s)))
           ((char-symbol? (car s))
        (st-symbol/number (cdr s) (list (car s))))
           (else (scanner-error "What's this" s)))))
       (st-unquote
        (lambda (s)
          (cond ((null? s) `((,'unquote) ,@(st-init '())))
            ((char=? (car s) #\@)
             `((,'unquote-splicing) ,@(st-init (cdr s))))
            (else `((,'unquote) ,@(st-init s))))))
       (st-symbol/number
        (lambda (s chars)
          (cond ((null? s)
             `(,(make-symbol/number-token chars) ,@(st-init '())))
            ((char-symbol? (car s))
             (st-symbol/number (cdr s) (cons (car s) chars)))
            ((char-delimiter? (car s))
             `(,(make-symbol/number-token chars) ,@(st-init s)))
            (else (scanner-error "At the end of a symbol: " s)))))
       (st-string
        (lambda (s chars)
          (cond ((null? s)
             (scanner-error "Expecting a \" char to close the string"))
            ((char=? (car s) #\")
             `((string ,(list->string (reverse chars)))
               ,@(st-init (cdr s))))
            ((char=? (car s) #\\) (st-meta-char (cdr s) chars))
            (else (st-string (cdr s) (cons (car s) chars))))))
       (st-meta-char
        (lambda (s chars)
          (cond ((null? s)
             (scanner-error
              "Expecting a string meta-char; reached EOF"))
            ((char=? (car s) #\\) (st-string (cdr s) (cons #\\ chars)))
            ((char=? (car s) #\") (st-string (cdr s) (cons #\" chars)))
            ((char-ci=? (car s) #\n)
             (st-string (cdr s) (cons #\newline chars)))
            ((char-ci=? (car s) #\r)
             (st-string (cdr s) (cons #\return chars)))
            ((char-ci=? (car s) #\t)
             (st-string (cdr s) (cons #\tab chars)))
            ((char-ci=? (car s) #\f)
             (st-string (cdr s) (cons #\page chars)))
            (else (scanner-error "What kind of a meta-char is " s)))))
       (st-hash
        (lambda (s)
          (cond ((null? s)
             (scanner-error
              "Expecting something after #, but reached end"))
            ((char=? (car s) #\() `((vector) ,@(st-init (cdr s))))
            ((char=? (car s) #\\) (st-char-1 (cdr s)))
            ((char-ci=? (car s) #\f)
             `((boolean #f) ,@(st-init (cdr s))))
            ((char-ci=? (car s) #\t)
             `((boolean #t) ,@(st-init (cdr s))))
            ((char=? (car s) #\;) `((comment) ,@(st-init (cdr s))))
            (else (scanner-error
               "Expecting t, f, \\, ( after #, but found" s)))))
       (st-char-1
        (lambda (s)
          (cond ((null? s) (error 'scanner "Must be one char after #\\"))
            (else (st-char (cdr s) (list (car s)))))))
       (st-char
        (lambda (s chars)
          (cond ((null? s) `((char ,(make-char chars)) ,@(st-init '())))
            ((char-delimiter? (car s))
             `((char ,(make-char chars)) ,@(st-init s)))
            (else (st-char (cdr s) (cons (car s) chars))))))
       (st-comment
        (lambda (s)
          (cond ((null? s) (st-init '()))
            ((char=? (car s) #\newline) (st-init (cdr s)))
            (else (st-comment (cdr s)))))))
    (lambda (s)
      (st-init s))))

(define make-symbol/number-token
  (lambda (chars)
    (let* ((string (list->string (reverse chars)))
       (maybe-number (string->number string)))
      (if (number? maybe-number)
      `(number ,maybe-number)
      `(symbol ,(string->symbol (string-downcase string)))))))

(define make-char
  (lambda (chars)
    (cond ((null? chars) (scanner-error "Found #\\ without any char"))
      ((null? (cdr chars)) (car chars))
      (else (let* ((string (list->string (reverse chars)))
               (maybe-number (string->number string 8)))
          (if (number? maybe-number)
              (integer->char maybe-number)
              (cond ((string-ci=? string "return") #\return)
                ((string-ci=? string "newline") #\newline)
                ((string-ci=? string "space") #\space)
                ((string-ci=? string "tab") #\tab)
                ((string-ci=? string "page") #\page)
                (else (scanner-error
                   "Can't recognize the following character: "
                   (format "#\\~s" string))))))))))

(define char-alphabetic? ((make-char-between? char-ci<=?) #\a #\z))
(define char-decimal? ((make-char-between? char<=?) #\0 #\9))

(define char-symbol?
  (let ((punc-chars (string->list "!@$%^*-_=+<>./?:")))
    (lambda (char)
      (or (char-alphabetic? char)
      (char-decimal? char)
      (ormap 
       (lambda (punc-char) (char=? punc-char char))
       punc-chars)))))

(define char-whitespace?
  (lambda (char)
    (char<=? char #\space)))

(define char-delimiter?
  (lambda (char)
    (or (char-whitespace? char)
    (not (char-symbol? char)))))

(define scanner-error
  (lambda (message s)
    (if (null? s)
    (error 'list-tokens message)
    (error 'list-tokens
           (format "~a: [~s]~a"
               message
               (car s)
               (list->string (cdr s)))))))

(define file->tokens
  (lambda (filename)
    (list->tokens
     (file->list filename))))

(define string->tokens
  (lambda (string)
    (list->tokens
     (string->list string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assignment 1 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; part 1 - READER IMPLEMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tokens->sexprs
    (lambda (tokens)
        (get-sexprs tokens (lambda(id empty) id))))
        
(define get-sexpr
  (lambda (token ret-sexpr+toks ret-fail)
    (cond 
        ;BASE CASE NULL - send fail procedure
        ((null? token) (ret-fail))
        
        ;BASE CASES (terminals) - send  ret-sexpr+toks
        ((or    (eq? (caar token) 'boolean)
                (eq? (caar token) 'char)
                (eq? (caar token) 'number)
                (eq? (caar token) 'string)
                (eq? (caar token) 'symbol)
                ) (ret-sexpr+toks (cadar token) (cdr token)))
        
        ;VECTOR CASE  
        ((eq? (caar token) 'vector)
           (get-sexprs (cdr token)
                       (lambda (sexprs toks)
                         (if (or (null? toks) (not (eq? (caar toks) 'rparen))) ;if there is no rparen or end-of-input
                                (reader-error "No rparen ) found after vector.") ; case we should throw an error
                                (ret-sexpr+toks (list->vector sexprs) (cdr toks) ) ; case we should go on                     
                         ))))
                         
        ;BEGIN LIST CASE  
        ((eq? (caar token) 'lparen)
           (get-sexprs (cdr token)
                       (lambda (sexprs toks)
                         (if (or (null? toks) (not (or (eq? (caar toks) 'dot) (eq? (caar toks) 'rparen)))) ;if there is no rparen or dot or end-of-input
                                (reader-error "No rparen ) found after list.") ; case we should throw an error
                                (if (eq? (caar toks) 'rparen)
                                    (ret-sexpr+toks sexprs (cdr toks)) ; case we got list
                                    (get-sexpr (cdr toks) ; case we got improper list
                                                (lambda (sexpr2 toks2) 
                                                    (if (or (null? toks2) (not (eq? (caar toks2) 'rparen))) ;success - check for rparen (improper list)
                                                        (reader-error "No rparen ) found after list.")
                                                        (ret-sexpr+toks (append sexprs sexpr2) (cdr toks2))))
                                                ret-fail)) ;fail procedure - remain the same
                         ))))
                         
        ;RIGHT-PAREN / DOT CASE - IDENTIFY END OF VECTOR / LIST
        ((or (eq? (caar token) 'dot) (eq? (caar token) 'rparen)) (ret-fail))
        
        
        ;SINGLE QUOTE CASE 
        ((eq? (caar token) 'single-quote)
            (get-sexpr (cdr token)
                      (lambda (sexpr toks)
                        (ret-sexpr+toks `',sexpr toks))
                      (lambda ()
                        (reader-error "No s-expression given after quote"))))
        
        ;OTHER QUOTE TYPES CASE
        ((or (eq? (caar token) 'quasiquote) (eq? (caar token) 'unquote) (eq? (caar token) 'unquote-splicing))
            (get-sexpr (cdr token)
                      (lambda (sexpr toks)
                        (ret-sexpr+toks (list (caar token) `,sexpr) toks))
                      (lambda ()
                        (reader-error "No s-expression given after quasiquote"))))
                        
        ;UN-RECOGNIZED TOKEN CASE
        (else (reader-error "Unrecognized characters."))
    )))
    
(define get-sexprs
  (lambda (toks ret-exps)
    (get-sexpr 
                ;tokens input
                toks
               ;success
               (lambda (sexpr toks)
                        (get-sexprs toks
                                    (lambda (sexprs toks) (ret-exps (cons sexpr sexprs) toks))))
               ;fail (i.e. stop recursion when in get-sexpr procedure
               (lambda () (ret-exps '() toks)))))
               

; A generic error procedure based on given message. copy-past from scanner-error               
(define reader-error
  (lambda (msg)
    (error 'read-error msg)))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; part 2 - PARSER IMPLEMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
    
;;; matcher.scm
;;;
;;; Programmer: Mayer Goldberg, 2012
;;; Editors: Tom Peres, Niv Stolarski

(define match
  (letrec ((match
        (lambda (pat e ret-vals ret-fail)
          (cond ((and (pair? pat) (pair? e))
             (match (car pat) (car e)
                (lambda (vals-car)
                  (match (cdr pat) (cdr e)
                     (lambda (vals-cdr)
                       (ret-vals
                    (append vals-car vals-cdr)))
                     ret-fail))
                ret-fail))
            ((and (vector? pat) (vector? e)
              (= (vector-length pat) (vector-length e))
              (match (vector->list pat) (vector->list e)
                 ret-vals ret-fail)))
            ((procedure? pat)
             (let ((v (pat e)))
               (if v (ret-vals v) (ret-fail))))
            ((equal? pat e) (ret-vals '()))
            (else (ret-fail))))))
    (lambda (pat e ret-with ret-fail)
      (match pat e
         (lambda (vals) (apply ret-with vals))
         ret-fail))))

(define ?
  (lambda (name . guards)
    (let ((guard?
       (lambda (e)
         (andmap 
          (lambda (g?) (g? e))
          guards))))
      (lambda (value)
    (if (guard? value)
        (list value)
        #f)))))

;;; composing patterns

(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

(define compose-patterns
  (letrec ((match-nothing (lambda (e failure) (failure)))
           (loop (lambda (s) (if (null? s)
                                match-nothing
                                (let (  (match-rest (loop (cdr s))) ;v1 e1
                                        (match-first (car s))); v2 e2
                                    ;body
                                    (lambda (e failure)
                                        (match-first e
                                        (lambda ()
                                        (match-rest e failure))))))))
            )
    ;letrec body
    (lambda patterns
      (loop patterns))))

;;; Example: A tag-parser for Scheme

(define *void-object* (if #f #f))

(define simple-const?
  (let ((preds (list boolean? char? number? string? vector?)))
    (lambda (e)
      (ormap (lambda (p?) (p? e)) preds))))

(define var?
  (lambda (e)
    (and (symbol? e)
     (not (reserved-word? e)))))

(define reserved-word?
  (lambda (e)
    (ormap
     (lambda (kw) (eq? e kw))
     *reserved-words*)))
     
(define not-reserved-word?
    (lambda (e)
        (not (reserved-word? e))))

(define *reserved-words*
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote
    quote set! unquote unquote-splicing))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assignment 2 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
(define parse
  (let ((run
     (compose-patterns
      (pattern-rule
       (? 'c simple-const?)
       (lambda (c) `(const ,c)))
      (pattern-rule
       `(quote ,(? 'c))
       (lambda (c) `(const ,c)))
      (pattern-rule
       (? 'v var?)
       (lambda (v) `(var ,v)))
      ;; if
      (pattern-rule
       `(if ,(? 'test) ,(? 'dit))
       (lambda (test dit)
         `(if-3 ,(parse test) ,(parse dit) (const ,*void-object*))))
      (pattern-rule
       `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
       (lambda (test dit dif)
         `(if-3 ,(parse test) ,(parse dit) ,(parse dif))))
      ;; let
      (pattern-rule
       `(let ,(? 'bindings list?) . ,(? 'exprs))
       (lambda (bindings exprs)
         (if (duplicate-vars? (map car bindings))
                (error 'parse "There is a duplicate arg in the let expression!")
                (parse `((lambda ,(map car bindings) ,(beginify exprs)) ,@(map cadr bindings))))))
      ;; let*
      (pattern-rule
       `(let* () ,(? 'expr) . ,(? 'exprs list?))
       (lambda (expr exprs)
         (parse (beginify (cons expr exprs)))))
      (pattern-rule
       `(let* ((,(? 'var var?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
       (lambda (var val rest exprs)
         (parse `(let ((,var ,val))
               (let* ,rest . ,exprs)))))
      ;; letrec
          (pattern-rule
           `(letrec () ,(? 'expr) . ,(? 'exprs list?))
           (lambda (expr exprs)
             (parse (beginify (cons expr exprs)))))
          (pattern-rule
           `(letrec ,(? 'ribs) ,(? 'expr) . ,(? 'exprs list?))
           (lambda (ribs expr exprs)
             (parse (expand-letrec `(letrec ,ribs ,expr ,@exprs)))))
      #;(pattern-rule
       `(letrec ,(? 'c))
       (lambda (c) (parse (expand-letrec `(letrec ,c)))))
      ;; quaziquote
      (pattern-rule
       `(quasiquote ,(? 'c))
       (lambda (c) `(const ,(expand-qq c))))
      ;; lambda (without beginify)
      (pattern-rule
       `(lambda ,(? 'args) ,(? 'expr))
       (lambda (args expr)
            (lambda-selector 
                args
                (lambda () `(lambda-simple ,args ,(parse expr)))
                (lambda (s a) `(lambda-opt ,s ,a ,(parse expr)))
                (lambda () `(lambda-variadic ,args ,(parse expr)))
                )))
       ;; lambda (with beginify)
      (pattern-rule
       `(lambda ,(? 'args) ,(? 'expr) . ,(? 'exprs list?))
       (lambda (args expr exprs)
            (lambda-selector 
                args
                (lambda () `(lambda-simple ,args ,(parse (beginify (cons expr exprs)))))
                (lambda (s a) `(lambda-opt ,s ,a ,(parse (beginify (cons expr exprs)))))
                (lambda () `(lambda-variadic ,args ,(parse (beginify (cons expr exprs)))))
                )))
      ;; MIT Define
      (pattern-rule
       `(define (,(? 'v var?) . ,(? 'vars)) ,(? 'exprs))
       (lambda (v vars exprs) `(define ,(parse v) ,(parse (list 'lambda vars exprs)))))
      ;; regular Define
      (pattern-rule
       `(define ,(? 'v var?) ,(? 'expr))
       (lambda (v expr) `(define ,(parse v) ,(parse expr))))
      ;; Begin (Seq)
      (pattern-rule
       `(begin . ,(? 'exprs list?))
       (lambda (exprs) `(seq ,(map parse exprs))))     
      ;; OR (empty)
      (pattern-rule
       `(or)
       (lambda () (parse '#f)))
      ;; OR (1 arg)
      (pattern-rule
       `(or ,(? 'expr))
       (lambda (expr) `,(parse expr)))  
      ;; OR (2+ arg)
      (pattern-rule
       `(or ,(? 'expr) .  ,(? 'exprs))
       (lambda (expr exprs) `(or ,(map parse (cons expr exprs)))))
      ;; AND (empty)
      (pattern-rule
       `(and)
       (lambda () (parse '#t)))
      ;; AND (1 arg)
      (pattern-rule
       `(and ,(? 'expr))
       (lambda (expr) `,(parse expr)))  
      ;; AND (2+ arg)
      (pattern-rule
       `(and ,(? 'expr) .  ,(? 'exprs))
       (lambda (expr exprs) (parse `(if ,expr (and ,@exprs) #f))))
      ;; Cond (else rule)
      (pattern-rule
       `(cond (else . ,(? 'exprs list?)))
       (lambda (exprs) (parse `,(beginify exprs))))    
       ;; Cond (1 rule)
      (pattern-rule
       `(cond ,(? 'expr list?))
       (lambda (expr) (parse `(if ,(car expr) ,(beginify (cdr expr))))))
       ;; Cond (2+ rules)
      (pattern-rule
       `(cond ,(? 'expr list?) .  ,(? 'exprs))
       (lambda (expr exprs) (parse `(if ,(car expr) ,(beginify (cdr expr)) (cond ,@exprs)))))      
      ;; Application (without args)
      (pattern-rule
       `(,(? 'v not-reserved-word?))
       (lambda (v) `(applic ,(parse v) ())))
      ;; Application (with args)
      (pattern-rule
       `(,(? 'v not-reserved-word?) . ,(? 'args))
       (lambda (v args) `(applic ,(parse v) ,(map parse args))))
      )))     
      
    (lambda (e)
      (run e
       (lambda ()
         e
         #;(error 'parse
            (format "I can't recognize this: ~s" e)))))))
            

(define beginify
  (lambda (s)
    (cond   ((null? s) *void-object*)
            ((null? (cdr s)) (car s))
            (else `(begin ,@s)))))

;;;;;;;;;;;;; Quaziquote expander ;;;;;;;;;;;;;
      
(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
      ((unquote-splicing? e) (error 'expand-qq "unquote-splicing here makes no sense!"))
      ((pair? e)
       (let ((a (car e))
         (b (cdr e)))
         (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
           ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
           (else `(cons ,(expand-qq a) ,(expand-qq b))))))
      ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
      ((or (null? e) (symbol? e)) `',e)
      (else e))))

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
       (eq? (car e) tag)
       (pair? (cdr e))
       (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

;;;;;;;;;;;;; Lambda expander ;;;;;;;;;;;;;

(define lambda-selector
    (lambda (e ret-pro ret-imp ret-sym)
        (cond   ((pair? e) 
                    (lambda-selector (cdr e)
                        ret-pro
                        (lambda (s a) (ret-imp (cons (car e)s) a))
                        (lambda () (ret-imp (list (car e))(cdr e)))))
                ((null? e)(ret-pro))
                ((symbol? e)(ret-sym))
                (else (parser-error "un-recognized lambda.")))))

;;;;;;;;;;;;; letrec expander ;;;;;;;;;;;;;

; (define yn
  ; (lambda fs
    ; (let ((ms (map
          ; (lambda (fi)
            ; (lambda ms
              ; (apply fi
                 ; (map (lambda (mi)
                    ; (lambda args
                      ; (apply (apply mi ms) args)))
                   ; ms))))
        ; fs)))
      ; (apply (car ms) ms))))

(define expand-letrec
  (lambda (e)
    (with e
      (lambda (_letrec ribs . exprs)
    (let* ((names `(,(gensym) ,@(map car ribs)))
           (fs `((lambda ,names ,@exprs)
             ,@(map (lambda (rib) `(lambda ,names ,(cadr rib)))
             ribs))))
      `(yn ,@fs))))))
	  


;;;;;;;;;;;; Duplication test in a let arguments

(define duplicate-vars?
    (lambda (args)
        (cond   ((null? args) #f)
                ((member? (car args) (cdr args)) #t)
                (else (duplicate-vars? (cdr args))))))
        
      
;;;;;;;;;;;; A generic error procedure based on given message. copy-past from scanner-error
               
(define parser-error
  (lambda (msg)
    (error 'read-error msg)))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Assignment 3 ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Ass3 - Q1 ;;;;;;;;;;;;;;;;;;;

(define map-run
    (lambda (action params env lst)
        (if (null? lst) 
            '()
            (cons (action (car lst) params env) (map-run action params env (cdr lst))))))

(define pe->lex-pe
    (letrec ((run
        (lambda (pe params env)
            (cond
;               ((null? pe) pe) ;;;;;;;;; CHECK!!!!!!
                ((not (pair? pe)) pe)
                ((eq? (car pe) 'const) pe)
                ((eq? (car pe) 'if-3)
                    `(if-3 ,(run (cadr pe) params env) ,(run (caddr pe) params env) ,(run (cadddr pe) params env)))
                ((eq? (car pe) 'or)
                    `(or ,(map-run run params env (cadr pe))))
                ((eq? (car pe) 'seq)
                    `(seq ,(map-run run params env (cadr pe))))
                ((eq? (car pe) 'define)
                    `(define ,(run (cadr pe) params env) ,(run (caddr pe) params env)))
                ((eq? (car pe) 'applic)
                    `(applic ,(run (cadr pe) params env) ,(map-run run params env (caddr pe))))
                ((eq? (car pe) 'var)
                    (with pe
                        (lambda (_ v)
                            (search-in-rib 
                                v 
                                params
                                (lambda (min) `(pvar ,v ,min))
                                (lambda ()
                                    (search-in-ribs 
                                        v 
                                        env
                                        (lambda (maj min)
                                            `(bvar ,v ,maj ,min))
                                        (lambda ()
                                            `(fvar ,v))))))))
                ((eq? (car pe) 'lambda-simple)
                    (with pe
                        (lambda (_ argl body)
                            `(lambda-simple ,argl ,(run body argl (cons params env))))))
                ((eq? (car pe) 'lambda-opt)
                    (with pe
                        (lambda (_ argl opt body)
                            `(lambda-opt ,argl ,opt
                                ,(run body `(,@argl ,opt) (cons params env))))))
                ((eq? (car pe) 'lambda-variadic)
                    (with pe
                        (lambda (_ argl body)
                            `(lambda-variadic ,argl ,(run body (list argl) (cons params env))))))
                ))))
        (lambda (pe)
            (run pe '() '()))))

(define search-in-rib
    (lambda (a s ret-min ret-nf)
        (cond   ( (null? s) (ret-nf) )
                ( (eq? (car s) a ) (ret-min 0) )
                ( else (search-in-rib a (cdr s)
                        (lambda(min) (ret-min (+ 1 min)))
                        ret-nf)))))

(define search-in-ribs
    (lambda (a env ret-maj+min ret-nf)
            (if (null? env)
                (ret-nf)
                (search-in-rib 
                        a 
                        (car env)
                        (lambda(min) (ret-maj+min 0 min))
                        (lambda() (search-in-ribs 
                            a 
                            (cdr env)
                            (lambda (maj min)
                                (ret-maj+min (+ 1 maj) min))
                            ret-nf))))))
                            

;;;;;;;;;;;;;;;;; Ass3 - Q2 ;;;;;;;;;;;;;;;

#;(define annotate-tc
  (lambda (pe)
    (run pe #f)))

#;(define run
  (lambda (pe tp?)
    (let ((tag (car pe))
          (exp (cdr pe)))
      (cond ((eq? tag 'const) pe)
            ((eq? tag 'fvar) pe)
            ((eq? tag 'pvar) pe)
            ((eq? tag 'bvar) pe)
            ((eq? tag 'if-3)
             (with exp
                   (lambda (test dit dif)
                     `(if-3 ,(run test #f)
                            ,(run dit tp?)
                            ,(run dif tp?)))))
            ((eq? tag 'seq)
             `(seq (,@(map (lambda (e) (run e #f))
                           (reverse (cdr (reverse (car exp)))))
                    ,(run (car (reverse (car exp))) tp?))))
            ((eq? tag 'or)
             `(or (,@(map (lambda (e) (run e #f))
                          (reverse (cdr (reverse (car exp)))))
                   ,(run (car (reverse (car exp))) tp?))))
            ((eq? tag 'define)
             (with exp
                   (lambda (var expr)
                     `(define ,var ,(run expr #f)))))
            ((eq? tag 'applic)
             (with exp
                   (lambda (proc args)
                     (let ((rest
                            (cons (run proc #f)
                                  `(,(map (lambda (e) (run e #f)) args)))))
                       (if tp?
                           (cons 'tc-applic rest)
                           (cons 'applic rest))))))
            ((eq? tag 'lambda-simple)
             (with exp
                   (lambda (args body)
                     `(lambda-simple ,args ,(run body #t)))))
            ((eq? tag 'lambda-opt)
             (with exp
                   (lambda (args rest body)
                     `(lambda-opt ,args ,rest ,(run body #t)))))
            ((eq? tag 'lambda-variadic)
             (with exp
                   (lambda (args body)
                     `(lambda-variadic ,args ,(run body #t)))))
            (else
             (error 'run "error"))))))
			 
#;(define map-run-tc
    (lambda (lst flag)
        (cond 
            ((null? lst) '())
            ((null? (cdr lst)) (list (run-tc (car lst) flag)))
            (else (cons (run-tc (car lst) #f) (map-run-tc (cdr lst) flag ))))))

#;(define run-tc
        (lambda (pe tc-flag)
            (cond
                ((null? pe) pe)
                ((not (pair? pe)) pe)
                ((eq? (car pe) 'const) pe)
                ((eq? (car pe) 'fvar) pe)
                ((eq? (car pe) 'bvar) pe)
                ((eq? (car pe) 'pvar) pe)
                ((eq? (car pe) 'if-3)
                    `(if-3 ,(run-tc (cadr pe) #f) ,(run-tc (caddr pe) tc-flag) ,(run-tc (cadddr pe) tc-flag)))
                ((eq? (car pe) 'seq)
                    `(seq ,(map-run-tc (cadr pe) tc-flag)))
                ((eq? (car pe) 'or)
                    `(or ,(map-run-tc (cadr pe) tc-flag)))
                ((eq? (car pe) 'define)
                    `(define ,(run-tc (cadr pe) #f) ,(run-tc (caddr pe) #f))) ;??? not sure why i changed caddr to #f
                ((eq? (car pe) 'lambda-simple)
                    `(lambda-simple ,(cadr pe) ,(run-tc (caddr pe) #t)))
                ((eq? (car pe) 'lambda-variadic)                
                    `(lambda-variadic ,(cadr pe) ,(run-tc (caddr pe) #t))) 
                ((eq? (car pe) 'lambda-opt)
                    `(lambda-opt ,(cadr pe) ,(caddr pe) ,(run-tc (cadddr pe) #t)))
                ;;;;;;;;;;;;;;;;;
                ((eq? (car pe) 'applic)
                    (if (eq? #t tc-flag)
                        `(tc-applic ,(run-tc (cadr pe) #t) ,(run-tc (caddr pe) #f))
                        `(applic ,(run-tc (cadr pe) #t) ,(map-run-tc (caddr pe) #f))))
                (else (cons (run-tc (car pe) tc-flag) (run-tc (cdr pe) tc-flag) ))
                
                )))

#;(define annotate-tc
  (lambda (pe)
    (run-tc pe #f)))

             
;********************************************^SEMANTICS^*********************************************************************

;*********************************************CODE GEN***********************************************************************

;;; Programmers: Ori Popowski, Reut Sharabani, Chen Zrubavel


(define T_VOID 937610)
(define T_NIL 722689)
(define T_BOOL 741553)
(define T_CHAR 181048)
(define T_INTEGER 945311)
(define T_STRING 799345)
(define T_SYMBOL 368031)
(define T_PAIR 885397)
(define T_VECTOR 335728)
(define T_CLOSURE 276405)

;;; Offsets to various segments
(define *ctab-seg* 10)
(define *symtab-seg* 0)
(define *prim-clos-seg* 0)

(define *initial-constant-table*
  `((10 ,void-object (937610))
    (11 () (722689))
    (12 #f (741553 0))
    (14 #t (741553 1))))

(define *primitive-names*
  '(procedure? vector? symbol? string? char? number? integer? boolean? pair? null? remainder bin/ bin- bin=?
    bin<? bin+ bin* bin>? car cdr string->symbol integer->char symbol->string char->integer
    set-cdr! set-car! string-set! vector-set! make-string make-vector cons vector-length
    string-length string-ref vector-ref apply eq?))
    
(define *primitive-closures*
  (letrec ((make-indices
            (lambda (ls new i)
              (if (null? ls)
                  (reverse new)
                  (make-indices (cdr ls) (cons (cons (+ (caar ls) i) (cdar ls)) new) (+ i 3))))))
    (make-indices (make-list (length *primitive-names*) `(0 ,T_CLOSURE ,T_NIL 0)) '() 0)))
                
    
(define *initial-symbol-table*
  '())
  
(define ^^label
  (lambda (name)
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        (string-append name
                       (number->string n))))))

;;; Unique label generators
(define ^label-prim (^^label             "PRIM"))
(define ^label-prim-exit (^^label        "PRIM_EXIT"))
(define ^or-exit (^^label                "OR_EXIT"))
(define ^label-if3else (^^label          "Lif3else"))
(define ^label-if3exit (^^label          "Lif3exit"))
(define ^lambda-simp-loop1 (^^label      "LAMBDA_SIMP_LOOP1"))
(define ^lambda-simp-loop1-exit (^^label "LAMBDA_SIMP_LOOP1_EXIT"))
(define ^lambda-simp-loop2 (^^label      "LAMBDA_SIMP_LOOP2"))
(define ^lambda-simp-loop2-exit (^^label "LAMBDA_SIMP_LOOP2_EXIT"))
(define ^lambda-simp-exit (^^label       "LAMBDA_SIMP_EXIT"))
(define ^lambda-simp-code (^^label       "LAMBDA_SIMP_CODE"))
(define ^lambda-opt-loop1 (^^label       "LAMBDA_OPT_LOOP1"))
(define ^lambda-opt-loop1-exit (^^label  "LAMBDA_OPT_LOOP1_EXIT"))
(define ^lambda-opt-loop2 (^^label       "LAMBDA_OPT_LOOP2"))
(define ^lambda-opt-loop2-exit (^^label  "LAMBDA_OPT_LOOP2_EXIT"))
(define ^lambda-opt-loop3 (^^label       "LAMBDA_OPT_LOOP3"))
(define ^lambda-opt-loop3-exit (^^label  "LAMBDA_OPT_LOOP3_EXIT"))
(define ^lambda-opt-loop4 (^^label       "LAMBDA_OPT_LOOP4"))
(define ^lambda-opt-loop4-exit (^^label  "LAMBDA_OPT_LOOP4_EXIT"))
(define ^lambda-opt-create-list (^^label "LAMBDA_OPT_CREATE_LIST"))
(define ^lambda-opt-gen-body (^^label    "LAMBDA_OPT_GEN_BODY"))
(define ^lambda-opt-exit (^^label        "LAMBDA_OPT_EXIT"))
(define ^lambda-opt-code (^^label        "LAMBDA_OPT_CODE"))
(define ^lambda-var-loop1 (^^label       "LAMBDA_VAR_LOOP1"))
(define ^lambda-var-loop1-exit (^^label  "LAMBDA_VAR_LOOP1_EXIT"))
(define ^lambda-var-loop2 (^^label       "LAMBDA_VAR_LOOP2"))
(define ^lambda-var-loop2-exit (^^label  "LAMBDA_VAR_LOOP2_EXIT"))
(define ^lambda-var-loop3 (^^label       "LAMBDA_VAR_LOOP3"))
(define ^lambda-var-loop3-exit (^^label  "LAMBDA_VAR_LOOP3_EXIT"))
(define ^lambda-var-create-list (^^label "LAMBDA_VAR_CREATE_LIST"))
(define ^lambda-var-gen-body (^^label    "LAMBDA_VAR_GEN_BODY"))
(define ^lambda-var-exit (^^label        "LAMBDA_VAR_EXIT"))
(define ^lambda-var-code (^^label        "LAMBDA_VAR_CODE"))
(define ^fvar-exit (^^label              "FVAR_EXIT"))
(define ^tc-applic-loop (^^label         "TC_APPLIC_LOOP"))
(define ^tc-applic-loop-exit (^^label    "TC_APPLIC_LOOP_EXIT"))

         
;;; Topological sorting for constants.
(define topological
  (lambda (e)
    (cond ((pair? e)
           `(,@(topological (car e)) ,@(topological (cdr e)), e))
          ((vector? e)
           `(,@(apply append (map topological (vector->list e))) ,e))
          (else
           `(,e)))))


;;; Add a constant into the constant table.
(define add-const
  (letrec ((add-const
            (lambda (ls table)
              (let* ((i (caar table))
                     (s (length (caddar table)))
                     (j (+ i s)))
                
                (cond
                  ((null? ls)
                   table)
                  
                  ((const-member? (car ls) table)
                   (add-const (cdr ls) table))
                  
                  ((symbol? (car ls))
                   (let* ((st (symbol->string (car ls)))
                          (ci (string->cisc st))
                          (new `(,@`((,(+ j 2 (car ci)) ,(car ls) (,T_SYMBOL ,j))
                                     (,j ,st (,T_STRING ,@ci)))
                                 ,@table)))
                     (add-const (cdr ls) new)))
                  
                  ((char? (car ls))
                   (let ((new (cons `(,j ,(car ls) (,T_CHAR ,(char->integer (car ls))))
                                    table)))
                     (add-const (cdr ls) new)))
                  
                  ((number? (car ls))
                   (let ((new (cons `(,j ,(car ls) (,T_INTEGER ,(car ls)))
                                    table)))
                     (add-const (cdr ls) new)))
                  
                  ((string? (car ls))
                   (let ((new (cons `(,j ,(car ls) (,T_STRING ,@(string->cisc (car ls))))
                                    table)))
                     (add-const (cdr ls) new)))
                  
                  ((pair? (car ls))
                   (let* ((^car (get-address (caar ls) table))
                          (^cdr (get-address (cdar ls) table))
                          (new (cons `(,j ,(car ls) (,T_PAIR ,^car ,^cdr))
                                     table)))
                     (add-const (cdr ls) new)))
                  
                  ((vector? (car ls))
                   (let ((new (cons `(,j ,(car ls) (,T_VECTOR ,@(vector->cisc (car ls) table)))
                                    table)))
                     (add-const (cdr ls) new)))
                  
                  (else
                   (cons (car ls) table)))))))
            
    (lambda (c table)
      (set! *initial-constant-table* (reverse (add-const (topological c) (reverse table)))))))


;;; Check if a constant is a member in constant table.
(define const-member?
  (lambda (c table)
    (not (eq? (get-entry c table) '()))))

;;; Return the whole entry that corresponds to the constant.
(define get-entry
  (lambda (c table)
    (if (null? table)
        '()
        (if (equal? c (cadar table))
            (car table)
            (get-entry c (cdr table))))))

;;; Get the memory address of constant c.
(define get-address
  (lambda (c table)
    (car (get-entry c table))))

;;; Converts a Scheme string into its memory representation in CISC Assembly.
(define string->cisc
  (letrec ((string->cisc
            (lambda (l)
              (if (null? l)
                  '()
                  (cons (char->integer (car l)) (string->cisc (cdr l)))))))
  (lambda (s)
    (let ((st (string->cisc (string->list s))))
      (cons (length st) st)))))

;;; Converts a Scheme vector into its memory representation in CISC Assembly.
(define vector->cisc
  (letrec ((vector->cisc
            (lambda (l table)
              (if (null? l)
                  '()
                  (cons (get-address (car l) table) (vector->cisc (cdr l) table))))))
    (lambda (vec table)
      (let ((v (vector->cisc (vector->list vec) table)))
        (cons (length v) v)))))
     
;;; Finds the next free address for *initial-symbol-table*
(define next-free1
  (lambda (table)
    (let ((last (car (last-pair table))))
      (+ (car last) (length (caddr last))))))

;;; Finds the next free address for *primitive-closures*
(define next-free2
  (lambda (table)
    (let ((last (car (last-pair table))))
      (+ (car last) (length (cdr last)) 1))))

;;; Makes a model of the constant table layout: <address, constant-name, memory-layout>.
(define make-symbol-table
  (letrec ((mst
            (lambda (ctab stab i)
              (if (null? ctab)
                  (begin (set-car! (cdddar stab) 0) ;; put 0 as the last bucket's next pointer
                         (reverse stab))
                  (if (equal? T_SYMBOL (caaddr (car ctab)))
                      (begin (set-car! (cdaddr (car ctab)) i)
                             (mst (cdr ctab)
                                  (cons `(,i ,(get-address (symbol->string (cadar ctab)) *initial-constant-table*) 0 ,(+ i 4)) stab)
                                  (+ i 4)))
                      (mst (cdr ctab) stab i))))))
    (lambda (ctab stab)
      (set! *initial-symbol-table* (mst ctab stab *symtab-seg*)))))


;;; Fills primitive buckets with names and pointers.
(define fill-prim-buckets
  (lambda (bucks names closures i new)
    (if (= i 35)
        (reverse new)
        (fill-prim-buckets (cdr bucks)
                           (cdr names) 
                           (cdr closures) 
                           (+ i 1)
                           (cons `(,(+ (caar bucks) *prim-bucks-seg*) ,(caar names) ,(caar closures) ,(+ (cadddr (car bucks)) *prim-bucks-seg*)) new)))))
      

;;; Corrects the offsets of *primitive-names* to be absolute addresses
(define fix-addresses
  (letrec ((fix-addresses
            (lambda (li new n)
              (if (null? li)
                  new
                  (fix-addresses (cdr li) (cons (cons (+ (caar li) n) (cdar li)) new) n)))))
  (lambda (ls n)
    (reverse (fix-addresses ls '() n)))))

;;; Makes a string of consecutive C commands to store code
;;; addresses of primitive procedures in the run-time memory.
(define copy-closures
  (letrec ((cc
            (lambda (prims i)
              (if (null? prims)
                  ""
                  (string-append
                   "\tMOV(M(mem)[" (number->string i) "], (long) &&" (car prims) ");\n"
                   (cc (cdr prims) (+ i 3)))))))
    (let ((prims '("PRIM_EQ" "APPLY" "VECTOR_REF" "STRING_REF" "STRING_LENGTH"
                             "VECTOR_LENGTH" "CONS" "MAKE_VECTOR" "MAKE_STRING"
                             "VECTOR_SET" "STRING_SET" "SET_CAR" "SET_CDR" "CHAR_INTEGER"
                             "SYMBOL_STRING" "INTEGER_CHAR" "STRING_SYMBOL" "CDR" "CAR"
                             "BIN_GT" "BIN_MUL" "BIN_PL" "BIN_LT" "BIN_EQ" "BIN_MIN"
                             "BIN_DIV" "REMAINDER" "PRIM_NULL" "PAIR" "BOOLEAN" "INTEGER" "NUMBER"
                             "CHAR" "STRING" "SYMBOL" "VECTOR" "PROCEDURE")))
      (lambda ()
        (cc prims (+ *prim-clos-seg* 2))))))

;;; Takes the symbol table and makes the relevant
;;; buckets point to the corresponding primitive
;;; closure.
(define point-to-prim-closures
  (lambda (tab i count)
    (if (not (zero? count))
        (begin (set-car! (cddar tab) i)
               (point-to-prim-closures (cdr tab) (+ i 3) (- count 1))))))


;;; Converts a list to a string which represent
;;; an array in C.
(define list->c-array
  (lambda (consts) 
    (string-append "{"
                   (apply string-append
                          (cdr (apply append
                                      (map (lambda (v)
                                             (list ", " (number->string v)))
                                           consts))))
                   "}")))
    
(define code-gen-const
  (lambda (c)
    (let ((address (get-address c *initial-constant-table*)))
    (cond
      ((boolean? c)
       (if c
           "\tMOV(R0, IMM(14));\n"
           "\tMOV(R0, IMM(12));\n"))
      ((char? c)
       (string-append
        "\t /********CHAR*******/\n"
        "\tMOV(R0, IMM(" (number->string address) "));\n"))
      ((number? c)
       (string-append
        "\t /********NUMBER*******/\n"
        "\tMOV(R0, IMM(" (number->string address) "));\n"))
      ((string? c)
       (string-append
        "\t /********STRING*******/\n"
        "\tMOV(R0, IMM(" (number->string address) "));\n"))
      ((symbol? c)
       (string-append
        "\t /********SYMBOL*******/\n"
        "\tMOV(R0, IMM(" (number->string address) "));\n"))
      ((vector? c)
       (string-append
        "\t /********VECTOR*******/\n"
        "\tMOV(R0, IMM(" (number->string address) "));\n"))
      ((list? c)
       (string-append
        "\t /********LIST*******/\n"
        "\tMOV(R0, IMM(" (number->string address) "));\n"))
      ((pair? c)
       (string-append
        "\t /********PAIR*******/\n"
        "\tMOV(R0, IMM(" (number->string address) "));\n"))
        
        ((void? c)
       (string-append
        "\t /********VOID*******/\n"
        "\tMOV(R0, IMM(10));\n"))
      (else
       (error 'code-gen-const ""))))))

(define code-gen-or
  (lambda (argl env)
    (let* ((or-exit (^or-exit))
           (args (map (lambda (e) (code-gen e env)) argl))
           (st (string-append
                "\tMOV(R1, INDD(R0, 1));\n"
                "\tCMP(R1, IMM(0));\n"
                "\tJUMP_NE(" or-exit ");\n"))
           (app (map (lambda (e) (string-append e st)) args)))
      (string-append
       "\t /********OR*******/\n"
       (apply string-append app)
       "\t" or-exit ":\n"
       "\t /*******^OR^******/\n"
       ))))

(define code-gen-if3
  (lambda (e env)
    (with e
          (lambda (test do-if-true do-if-false)
            (let ((code-test (code-gen test env))
                  (code-dit (code-gen do-if-true env))
                  (code-dif (code-gen do-if-false env))
                  (label-else (^label-if3else))
                  (label-exit (^label-if3exit)))
              (string-append
               "\t/********IF-3********/\n"
               code-test "\n" ; when run, the result of the test will be in R0
               "\tMOV(R0, INDD(R0, 1));\n"
               "\tCMP(R0, IMM(0));\n"
               "\tJUMP_EQ(" label-else ");\n"
               code-dit "\n"
               "\tJUMP(" label-exit ");\n"
               "\t" label-else ":\n"
               code-dif "\n"
               "\t" label-exit ":\n"
               "\t/*******^IF-3^*******/\n"))))))

(define code-gen-seq
  (lambda (rest env)
    (apply string-append (map (lambda (e) (code-gen e env)) rest))))

(define code-gen-lambda-simple
  (lambda (rest env)
    (let ((body (cadr rest))
          (loop1 (^lambda-simp-loop1))
          (loop1-exit (^lambda-simp-loop1-exit))
          (loop2 (^lambda-simp-loop2))
          (loop2-exit (^lambda-simp-loop2-exit))
          (exit (^lambda-simp-exit))
          (code (^lambda-simp-code)))
      (string-append
       "\t/**********LAMBDA-SIMPLE**********/\n"
       "\tPUSH(IMM(3));\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"
       "\tMOV(IND(R0), IMM(T_CLOSURE));\n"
       "\tMOV(R1, R0);\n" ;;; R1 points to the newly created closure.
       "\tPUSH(IMM(" (number->string (+ env 1)) "));\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"  ;;; R0 points to the new environment.
       "\tMOV(INDD(R1, 1), R0);\n" ;;; Put new environment in the new closure.
       "\tMOV(R2, FPARG(0))\n" ;;; R2 points to old environment.
       "\tMOV(R3, IMM(" (number->string env) "));\n"
       "\tMOV(R4, R3);\n"
       "\tDECR(R4);\n"
       "\t" loop1 ":\n"
       "\tCMP(R3, IMM(0));\n"
       "\tJUMP_EQ(" loop1-exit ");\n"
       "\tMOV(INDD(R0, R3), INDD(R2, R4));\n"
       "\tDECR(R3);\n"
       "\tDECR(R4);\n"
       "\tJUMP(" loop1 ");\n"
       "\t" loop1-exit ":\n"
       "\tMOV(R3, R0);\n"
       "\tMOV(R4, FPARG(1));\n"
       "\tPUSH(R4);\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"
       "\tMOV(IND(R3), R0);\n"
       "\tMOV(R2, FPARG(1));\n"
       "\tMOV(R5, 0);\n"
       "\tMOV(R6, 2);\n"
       "\t" loop2 ":\n"
       "\tCMP(R2, IMM(0));\n"
       "\tJUMP_EQ(" loop2-exit ");\n"
       "\tMOV(INDD(R0, R5), FPARG(R6));\n"
       "\tDECR(R2);\n"
       "\tINCR(R5);\n"
       "\tINCR(R6);\n"
       "\tJUMP(" loop2 ");\n"
       "\t" loop2-exit ":\n"
       "\tMOV(INDD(R1, IMM(2)), IMM(&&" code "));\n"
       "\tMOV(R0, R1);\n"
       "\tJUMP(" exit ");\n"
       "\t" code ":\n"
       "\tPUSH(FP);\n"
       "\tMOV(FP, SP);\n"
       (code-gen body (+ env 1))
       "\tPOP(FP);\n"
       "\tRETURN;\n"
       "\t" exit ":\n"
       "\t/*********^LAMBDA-SIMPLE^*********/\n"
       ))))

(define code-gen-lambda-opt
  (lambda (rest env)
    (let ((body (caddr rest))
          (oblig-args (length (car rest)))
          (loop1 (^lambda-opt-loop1))
          (loop1-exit (^lambda-opt-loop1-exit))
          (loop2 (^lambda-opt-loop2))
          (loop2-exit (^lambda-opt-loop2-exit))
          (loop3 (^lambda-opt-loop3))
          (loop3-exit (^lambda-opt-loop3-exit))
          (loop4 (^lambda-opt-loop4))
          (loop4-exit (^lambda-opt-loop4-exit))
          (create-list (^lambda-opt-create-list))
          (gen-body (^lambda-opt-gen-body))
          (exit (^lambda-opt-exit))
          (code (^lambda-opt-code)))
      #;(display body)
      (string-append
       "\t/**********LAMBDA-OPT**********/\n"
       "\tPUSH(IMM(3));\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"
       "\tMOV(IND(R0), IMM(T_CLOSURE));\n"
       "\tMOV(R1, R0);\n" ;;; R1 points to the newly created closure.
       "\tPUSH(IMM(" (number->string (+ env 1)) "));\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"  ;;; R0 points to the new environment.
       "\tMOV(INDD(R1, 1), R0);\n" ;;; Put new environment in the new closure.
       "\tMOV(R2, FPARG(0))\n" ;;; R2 points to old environment.
       "\tMOV(R3, IMM(" (number->string env) "));\n"
       "\tMOV(R4, R3);\n"
       "\tDECR(R4);\n"
       "\t" loop1 ":\n"
       "\tCMP(R3, IMM(0));\n"
       "\tJUMP_EQ(" loop1-exit ");\n"
       "\tMOV(INDD(R0, R3), INDD(R2, R4));\n"
       "\tDECR(R3);\n"
       "\tDECR(R4);\n"
       "\tJUMP(" loop1 ");\n"
       "\t" loop1-exit ":\n"
       "\tMOV(R3, R0);\n"
       "\tMOV(R4, FPARG(1));\n"
       "\tPUSH(R4);\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"
       "\tMOV(IND(R3), R0);\n"
       "\tMOV(R2, FPARG(1));\n"
       "\tMOV(R5, 0);\n"
       "\tMOV(R6, 2);\n"
       "\t" loop2 ":\n"
       "\tCMP(R2, IMM(0));\n"
       "\tJUMP_EQ(" loop2-exit ");\n"
       "\tMOV(INDD(R0, R5), FPARG(R6));\n"
       "\tDECR(R2);\n"
       "\tINCR(R5);\n"
       "\tINCR(R6);\n"
       "\tJUMP(" loop2 ");\n"
       "\t" loop2-exit ":\n"
       "\tMOV(INDD(R1, IMM(2)), IMM(&&" code "));\n"
       "\tMOV(R0, R1);\n"
       "\tJUMP(" exit ");\n"
       "\t" code ":\n"
       "\tPUSH(FP);\n"
       "\tMOV(FP, SP);\n"
       
       "\t/******STACK REPAIR*******/\n"
       "\tMOV(R1, FPARG(1));\n"
       "\tCMP(R1, IMM(" (number->string oblig-args) "));\n"
       "\tJUMP_NE(" create-list ");\n"
       
       "\tMOV(R2, R1);\n"
       "\tADD(R2, IMM(4));\n"
       "\tMOV(R3, SP);\n"
       "\tMOV(R4, SP);\n"
       "\tDECR(R4);\n"
       loop3 ":\n"
       "\tCMP(R2, IMM(0));\n"
       "\tJUMP_EQ(" loop3-exit ");\n"
       "\tMOV(STACK(R3), STACK(R4));\n"
       "\tDECR(R3);\n"
       "\tDECR(R2);\n"
       "\tDECR(R4);\n"
       "\tJUMP(" loop3 ");\n"
       loop3-exit ":\n"
       "\tMOV(STACK(R3), IMM(11));\n"
       "\tINCR(FP);\n"
       "\tINCR(SP);\n"
       "\tINCR(FPARG(1));\n"
       "\tJUMP(" gen-body ");\n"
       
       create-list ":\n"
       "\tMOV(R3, R1);\n"
       "\tSUB(R3, IMM(" (number->string oblig-args) "));\n"
       "\tPUSH(R3);\n"
       "\tMOV(R2, SP);\n"
       "\tSUB(R2, IMM(" (number->string (+ oblig-args 6)) "));\n" ;;; absolute address of first of rest in stack.
       "\tPUSH(R2);\n"
       "\tCALL(CREATE_LIST);\n"
       "\tDROP(IMM(2));\n"
       "\tMOV(FPARG(" (number->string (+ oblig-args 2)) "), R0);\n" ;;; move pointer to the list.
       "\tMOV(FPARG(1), IMM(" (number->string (+ oblig-args 1)) "));\n"
       
       "\tMOV(R2, SP);\n"
       "\tSUB(R2, R1);\n"
       "\tSUB(R2, IMM(4));\n"
       "\tMOV(R3, SP);\n"
       "\tSUB(R3, IMM(" (number->string (+ oblig-args 1)) "));\n"
       "\tSUB(R3, IMM(4));\n"
       "\tMOV(R4, IMM(4));\n"
       "\tADD(R4, IMM(" (number->string (+ oblig-args 1)) "));\n"
       loop4 ":\n"
       "\tCMP(R4, IMM(0));\n"
       "\tJUMP_EQ(" loop4-exit ");\n"
       "\tMOV(STACK(R2), STACK(R3));\n"
       "\tINCR(R3);\n"
       "\tINCR(R2);\n"
       "\tDECR(R4);\n"
       "\tJUMP(" loop4 ");\n"
       loop4-exit":\n"
       "\tSUB(FP, R1);\n"
       "\tADD(FP, IMM(" (number->string (+ oblig-args 1)) "));\n"
       "\tSUB(SP, R1);\n"
       "\tADD(SP, IMM(" (number->string (+ oblig-args 1)) "));\n"
       
       
       gen-body ":\n"
       (code-gen body (+ env 1))
       "\tPOP(FP);\n"
       "\tRETURN;\n"
       "\t" exit ":\n"
       "\t/*********^LAMBDA-OPT^*********/\n"
       ))))

(define code-gen-lambda-var
  (lambda (rest env)
    (let ((body (cadr rest))
          (loop1 (^lambda-var-loop1))
          (loop1-exit (^lambda-var-loop1-exit))
          (loop2 (^lambda-var-loop2))
          (loop2-exit (^lambda-var-loop2-exit))
          (loop3 (^lambda-var-loop3))
          (loop3-exit (^lambda-var-loop3-exit))
          (create-list (^lambda-var-create-list))
          (gen-body (^lambda-var-gen-body))
          (exit (^lambda-var-exit))
          (code (^lambda-var-code))
          )
      (string-append
       "\t/**********LAMBDA-VAR**********/\n"
       "\tPUSH(IMM(3));\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"
       "\tMOV(IND(R0), IMM(T_CLOSURE));\n"
       "\tMOV(R1, R0);\n" ;;; R1 points to the newly created closure.
       "\tPUSH(IMM(" (number->string (+ env 1)) "));\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"  ;;; R0 points to the new environment.
       "\tMOV(INDD(R1, 1), R0);\n" ;;; Put new environment in the new closure.
       "\tMOV(R2, FPARG(0))\n" ;;; R2 points to old environment.
       "\tMOV(R3, IMM(" (number->string env) "));\n"
       "\tMOV(R4, R3);\n"
       "\tDECR(R4);\n"
       "\t" loop1 ":\n"
       "\tCMP(R3, IMM(0));\n"
       "\tJUMP_EQ(" loop1-exit ");\n"
       "\tMOV(INDD(R0, R3), INDD(R2, R4));\n"
       "\tDECR(R3);\n"
       "\tDECR(R4);\n"
       "\tJUMP(" loop1 ");\n"
       "\t" loop1-exit ":\n"
       "\tMOV(R3, R0);\n"
       "\tMOV(R4, FPARG(1));\n"
       "\tPUSH(R4);\n"
       "\tCALL(MALLOC);\n"
       "\tDROP(IMM(1));\n"
       "\tMOV(IND(R3), R0);\n"
       "\tMOV(R2, FPARG(1));\n"
       "\tMOV(R5, 0);\n"
       "\tMOV(R6, 2);\n"
       "\t" loop2 ":\n"
       "\tCMP(R2, IMM(0));\n"
       "\tJUMP_EQ(" loop2-exit ");\n"
       "\tMOV(INDD(R0, R5), FPARG(R6));\n"
       "\tDECR(R2);\n"
       "\tINCR(R5);\n"
       "\tINCR(R6);\n"
       "\tJUMP(" loop2 ");\n"
       "\t" loop2-exit ":\n"
       "\tMOV(INDD(R1, IMM(2)), IMM(&&" code "));\n"
       "\tMOV(R0, R1);\n"
       "\tJUMP(" exit ");\n"
       "\t" code ":\n"
       "\tPUSH(FP);\n"
       "\tMOV(FP, SP);\n"
	   
       "\t/******STACK REPAIR*******/\n"
       "\tMOV(R1, FPARG(1));\n"
       "\tCMP(R1, IMM(0));\n"
       "\tJUMP_NE(" create-list ");\n"
       
       "\tMOV(R2, SP);\n"
       "\tMOV(R3, SP);\n"
       "\tDECR(R3);\n"
       "\tMOV(STACK(R2), STACK(R3));\n"
       "\tDECR(R3);\n"
       "\tDECR(R2);\n"
       "\tMOV(STACK(R2), STACK(R3));\n"
       "\tDECR(R3);\n"
       "\tDECR(R2);\n"
       "\tMOV(STACK(R2), STACK(R3));\n"
       "\tDECR(R3);\n"
       "\tDECR(R2);\n"
       "\tMOV(STACK(R2), IMM(1));\n"
       "\tMOV(STACK(R3), IMM(11));\n" ; NIL
       "\tINCR(SP);\n"
       "\tINCR(FP);\n"
       "\tJUMP(" gen-body ");\n"
       
       create-list ":\n"
       "\tPUSH(R1);\n"
       "\tMOV(R2, SP);\n"
       "\tSUB(R2, IMM(6));\n" ;;; absolute address of first of rest in stack.
       "\tPUSH(R2);\n"
       "\tCALL(CREATE_LIST);\n"
       "\tDROP(IMM(2));\n"
       "\tMOV(FPARG(2), R0);\n" ;;; move pointer to the list.
       "\tMOV(FPARG(1), IMM(1));\n"
       
       "\tMOV(R2, SP);\n"
       "\tSUB(R2, R1);\n"
       "\tSUB(R2, IMM(4));\n"
       "\tMOV(R3, SP);\n"
       "\tSUB(R3, IMM(5));\n"
       "\tMOV(R4, IMM(5));\n"
       loop3 ":\n"
       "\tCMP(R4, IMM(0));\n"
       "\tJUMP_EQ(" loop3-exit ");\n"
       "\tMOV(STACK(R2), STACK(R3));\n"
       "\tINCR(R3);\n"
       "\tINCR(R2);\n"
       "\tDECR(R4);\n"
       "\tJUMP(" loop3 ");\n"
       loop3-exit":\n"
       "\tSUB(FP, R1);\n"
       "\tADD(FP, IMM(1));\n"
       "\tSUB(SP, R1);\n"
       "\tADD(SP, IMM(1));\n"  
       
       gen-body ":\n"
       (code-gen body (+ env 1))
	   
       "\tPOP(FP);\n"
       "\tRETURN;\n"
       "\t" exit ":\n"
       "\t/*********^LAMBDA-VAR^*********/\n"
       ))))
    
(define code-gen-applic
  (lambda (rest env)
    (let* ((proc (car rest))
           (exprs (cadr rest))
           (m (length exprs))
           (gen-exprs (map (lambda (e) (code-gen e env)) (reverse exprs)))
           (st (map (lambda (e) (string-append e "\tPUSH(R0);\n")) gen-exprs))
           (pushes (apply string-append st)))
      (string-append
       "\t /********APPLIC*******/\n"
       "\t" pushes "\n"
       "\tPUSH(IMM(" (number->string m) "));\n"
       (code-gen proc env)
       "\tPUSH(INDD(R0, IMM(1)));\n"
       "\tCALLA(INDD(R0, IMM(2)));\n"
       "\tMOV(R1, STARG(0));\n" ;;; number of params in the stack
       "\tADD(R1, IMM(2));\n" ;;; size of frame
       "\tSUB(SP, R1);\n"
       "\t /*******^APPLIC^******/\n"
       ))))

(define code-gen-tc-applic
  (lambda (rest env)
    (let* ((proc (car rest))
           (exprs (cadr rest))
           (m (length exprs))
           (gen-exprs (map (lambda (e) (code-gen e env)) (reverse exprs)))
           (st (map (lambda (e) (string-append e "\tPUSH(R0);\n")) gen-exprs))
           (pushes (apply string-append st))
           (loop (^tc-applic-loop))
           (exit (^tc-applic-loop-exit)))
      (string-append
       "\t /********TC-APPLIC*******/\n"
       "\t" pushes "\n"
       "\tPUSH(IMM(" (number->string m) "));\n"
       (code-gen proc env)
       "\t/*****OVERWRITE-FRAME************/\n"
       "\tPUSH(INDD(R0, IMM(1)));\n" ;; push env
       "\tMOV(R1, STARG(0));\n" ;; args num
       "\tADD(R1, IMM(2));\n" ;; frame size;
       "\tMOV(R2, SP);\n"
       "\tSUB(R2, R1);\n" ;; bottom of frame.
       "\tMOV(R3, R2);\n"
       "\tSUB(R3, IMM(2));\n"
       "\tPUSH(STACK(R3));\n" ;; push overwritten frames ret
       "\tSUB(R3, IMM(2));\n"
       "\tMOV(R3, STACK(R3));\n" ;; num of overwritten frames args
       "\tADD(R3, IMM(4));\n" ;; size of overwritten frame
       "\tMOV(R4, R2);\n"
       "\tSUB(R4, R3);\n" ;; bottom of overwritten frame
       "\tMOV(R5, R2);\n"
       "\tDECR(R5);\n"
       "\tMOV(FP, STACK(R5));\n" ;; overwritten frames old fp
       "\tMOV(R5, R3);\n" ;; size of copied frame
       
       loop ":\n"
       "\tCMP(R3, IMM(0));\n"
       "\tJUMP_EQ(" exit ");\n"
       "\tMOV(STACK(R4), STACK(R2));\n"
       "\tINCR(R2);\n"
       "\tINCR(R4);\n"
       "\tDECR(R3);\n"
       "\tJUMP(" loop ");\n"
       exit ":\n"
       "\tSUB(SP, R5);\n"
       "\tJUMPA(INDD(R0, IMM(2)));\n"
       "\t /*******^TC-APPLIC^******/\n"
       ))))


(define code-gen-pvar
  (lambda (rest)
    (let ((min (cadr rest)))
      #;(display min)
      (string-append
       "\t/*****PVAR******/\n"
       "\tMOV(R0, FPARG(" (number->string (+ 2 min)) "));\n"))))

(define code-gen-bvar
  (lambda (rest)
    (let ((maj (cadr rest))
          (min (caddr rest)))
      #;(display maj)
      #;(display min)
      (string-append
       "\t/*****BVAR******/\n"
       "\tMOV(R0, FPARG(0));\n"
       "\tMOV(R0, INDD(R0, IMM(" (number->string maj) ")));\n"
       "\tMOV(R0, INDD(R0, IMM(" (number->string min) ")));\n"))))

(define code-gen-define
  (lambda (rest env)
    (let* ((var (cadar rest))
           (val (cadr rest))
           (entry (get-entry var *initial-constant-table*))
           (bucket (cadr (caddr entry))))
      #;(display entry)
      (string-append
       (code-gen val env)
       "\t/*****DEFINE******/\n"
       "\tMOV(R1, IMM(" (number->string bucket) "));\n"
       "\tMOV(INDD(R1, 2), R0);\n"
       "\tMOV(R0, IMM(10));\n"
       "\t/****^DEFINE^*****/\n"))))

(define code-gen-fvar
  (lambda (rest)
    (let* ((var (car rest))
           (entry (get-entry var *initial-constant-table*))
           (bucket (cadr (caddr entry)))
           (exit (^fvar-exit)))
      #;(display entry)
      (string-append
       "\t/*****FVAR******/\n"
       "\tMOV(R0, IMM(" (number->string bucket) "));\n"
       "\tMOV(R0, INDD(R0, 2));\n"
       "\tCMP(R0, IMM(0));\n"
       "\tJUMP_NE(" exit ");\n"
       "\tprintf(\"Error\");\n"
       "\tJUMP(END);\n"
       "\t" exit ":\n"
       
       "\t/****^FVAR^*****/\n"))))
      
    
(define cg-const?
  (lambda (pe)
    #;(display pe)
    (equal? (car pe) 'const)))

(define cg-or?
  (lambda (pe)
    (equal? (car pe) 'or)))
    
(define cg-if3?
  (lambda (e)
    (equal? (car e) 'if-3)))
    
(define cg-seq?
  (lambda (e)
    (equal? (car e) 'seq)))

(define cg-lambda-simple?
  (lambda (e)
    (equal? (car e) 'lambda-simple)))

(define cg-lambda-opt?
  (lambda (e)
    (equal? (car e) 'lambda-opt)))

(define cg-lambda-var?
  (lambda (e)
    (equal? (car e) 'lambda-variadic)))

(define cg-applic?
  (lambda (e)
    (equal? (car e) 'applic)))

(define cg-tc-applic?
  (lambda (e)
    (equal? (car e) 'tc-applic)))
    
(define cg-pvar?
 (lambda (e)
   (equal? (car e) 'pvar)))

(define cg-bvar?
 (lambda (e)
   (equal? (car e) 'bvar)))

(define cg-fvar?
 (lambda (e)
   (equal? (car e) 'fvar)))

(define cg-define?
 (lambda (e)
   (equal? (car e) 'define)))
    

(define code-gen
  (lambda (pe env)
    #;(display pe)
    (cond
      ((cg-const? pe) (code-gen-const (cadr pe)))
      ((cg-or? pe) (code-gen-or (cadr pe) env))
      ((cg-if3? pe) (code-gen-if3 (cdr pe) env))
      ((cg-seq? pe) (code-gen-seq (cadr pe) env))
      ((cg-lambda-simple? pe) (code-gen-lambda-simple (cdr pe) env))
      ((cg-lambda-opt? pe) (code-gen-lambda-opt (cdr pe) env))
      ((cg-lambda-var? pe) (code-gen-lambda-var (cdr pe) env))
      ((cg-pvar? pe) (code-gen-pvar (cdr pe)))
      ((cg-bvar? pe) (code-gen-bvar (cdr pe)))
      ((cg-fvar? pe) (code-gen-fvar (cdr pe)))
      ((cg-define? pe) (code-gen-define (cdr pe) env))
      ((cg-applic? pe) (code-gen-applic (cdr pe) env))
      ((cg-tc-applic? pe) (code-gen-tc-applic (cdr pe) env))
      ((list? pe) (map (lambda (e) (code-gen e env)) pe))
      (else
       (error 'code-gen "error")))))

 (define support #f)

(define compile-scheme-file
  (lambda (source dest)
    (let* ((out (open-output-file dest 'replace))
           (file-to-compile (append (file->tokens "support-code.scm") (file->tokens source)))
           (sexprs (tokens->sexprs file-to-compile))
           (code-gen-input (map pe->lex-pe (map parse sexprs)))
           )
      ;;; Add primitives to constant table:
      (map (lambda (e) (add-const e *initial-constant-table*)) *primitive-names*)
      ;;; Add source program into constant table:
      (add-const sexprs *initial-constant-table*)
      ;;; Determine address in memory for initial symbol table:
      (set! *symtab-seg* (next-free1 *initial-constant-table*))
      ;;; Build initial symbol table from constant table:
      (make-symbol-table *initial-constant-table* *initial-symbol-table*)
      ;;; Determine address in memory for primitive procedures closures:
      (set! *prim-clos-seg* (next-free2 *initial-symbol-table*))
      ;;; Build initial closures for primitive procedures. Code pointers will be add later.
      (set! *primitive-closures* (fix-addresses *primitive-closures* *prim-clos-seg*))
      ;;; Make the primitive procedures entries in symbol table point to primitive closures.
      (point-to-prim-closures *initial-symbol-table* *prim-clos-seg* (length *primitive-names*))
      #;(display code-gen-input)
      (display (string-append
              "#include <stdio.h>\n"
              "#include <stdlib.h>\n"
              "#include <string.h>\n"
              "#include \"cisc.h\"\n\n"

              "#define CTAB_START 1\n"
              "#define STAB_START 2\n\n"
     
              "int main()\n"
              "{\n"
              "\tSTART_MACHINE;\n"
              "\tlong arr[] = \n"
              "\t" (list->c-array (append (apply append (map caddr *initial-constant-table*))
                                          (apply append *initial-symbol-table*)
                                          (apply append (map cdr *primitive-closures*))
                                          )) ";\n\n"
              "\tmemcpy((void *) (M(mem)+10), (void *) arr, sizeof arr);\n"
              "\tM(mem)[0] = (sizeof arr)/WORD_SIZE + 10;\n\n"
              "\tJUMP(CONTINUE);\n\n"

              "\t#include \"char.lib\"\n"
              "\t#include \"io.lib\"\n"
              "\t#include \"math.lib\"\n"
              "\t#include \"string.lib\"\n"
              "\t#include \"system.lib\"\n"
              "\t#include \"scheme.lib\"\n"
              "\t#include \"proj.lib\"\n"
              "\t#include \"primitives.lib\"\n\n"

              "\tCONTINUE:\n\n"
              "\tMOV(M(mem)[CTAB_START], IMM (" (number->string *ctab-seg*) "));\n"
              "\tMOV(M(mem)[STAB_START], IMM(" (number->string *symtab-seg*) "));\n\n"
              (copy-closures) "\n\n"
              "\tPUSH(IMM(0));\n"
              "\tPUSH(IMM(T_NIL));\n"
              "\tPUSH(LABEL(END));\n"
              "\tPUSH(FP);\n"
              "\tMOV(FP,SP);\n\n"
              "\t" (apply string-append
                     (map (lambda (e)
                            (if support
                                (code-gen e 0)
                                (if (and (equal? (car e) 'define)
                                         (equal? (cadadr e) 'sexpr->display-string))
                                    (begin (set! support #t)
                                           (code-gen e 0))
                                    (string-append (code-gen e 0)
                                                   "\tPUSH(R0);\n"
                                                   "\tCALL(WRITE_SOB);\n"
                                                   "\tDROP(1);\n"
                                                   "\tPUSH('\\n');\n"
                                                   "\tCALL(PUTCHAR);\n"
                                                   "\tDROP(1);\n"
                                                   ))))
                          code-gen-input)) ";\n"
              "\tEND:\n"
             
              "\tSTOP_MACHINE;\n"
              
              "\treturn 0;\n"
              "}\n"
              ) out)
       #;(display file-to-compile o-port1)
       #;(display (code-gen code-gen-input) o-port1)      
       (close-output-port out)
       )))



#;(compile-scheme-file "d:\\comp2\\b.scm" "d:\\comp2\\arch\\a.c")
