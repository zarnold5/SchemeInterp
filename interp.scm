;Zach Arnold
;My Scheme Interpreter

;--- define the global environment
(define genv 
  (cons (cons 'values values)
  (cons (cons 'write write)
  (cons (cons 'eof-object? eof-object?)
  (cons (cons 'open-input-file open-input-file)
  (cons (cons 'assoc assoc)
  (cons (cons 'read read)
  (cons (cons 'display display)
  (cons (cons 'number? number?)
  (cons (cons 'equal? equal?)
  (cons (cons 'null? null?)
  (cons (cons 'boolean? boolean?)
  (cons (cons 'string? string?)
  (cons (cons 'char? char?)
  (cons (cons 'list? list?)
  (cons (cons 'symbol? symbol?)
  (cons (cons '+ +)
  (cons (cons '* *)
  (cons (cons '- -)
  (cons (cons '/ /)
  (cons (cons 'cdr cdr)
  (cons (cons 'car car)
  (cons (cons 'cons cons)'())
))))))))))))))))))))))

;; (define (caaddr lst)
;;   (car (car (cdr (cdr lst)))))

(define (cddr lst)
  (cdr (cdr lst)))

(define (cadadar lst)
  (car (cdr (car (cdr (car lst))))))

(define (cddadar lst)
    (cdr (cdr (car (cdr (car lst))))))

(define (caddadar lst)
  (car (cdr (cdr (car (cdr (car lst)))))))

(define (cadar lst)
  (car (cdr (car lst)))
)

(define (caadr lst)
  (car (car (cdr lst))))

(define (cdadr lst)
  (cdr (car (cdr lst))))

(define (caar lst)
  (car (car lst))
)

(define (cdar lst)
  (cdr (car lst))
)

(define (cadddr lst)
  (car (cdr (cdr (cdr lst))))
)

(define (caddr lst)
  (car (cdr (cdr lst)))
)

(define (cadr lst)
  (car (cdr lst))
)

; --- usefull for let, making a 2d list of pairs into assoc list.
(define (make-assoc lst)
  (if (null? lst)
      '()
      (cons (cons (caar lst) (cadar lst)) (make-assoc (cdr lst))))
)

(define (update-env lst env)
  (if (null? lst) 
      env
      (cons (car lst) (update-env (cdr lst) env))
)) 

(define (eval-lst lst env)
  (if (null? lst)
      '()
      (cons (evaluate (car lst) env) (eval-lst (cdr lst) env))
))

(define (ret-last lst)
  (if (null? (cdr lst))
      (car lst)
      (ret-last (cdr lst))
))
      
(define (in-env? elmt lst)
  (if (null? lst)
      #f
      (if (equal? (caar lst) elmt)
          #t
          (in-env? elmt (cdr lst)))
))

(define (eval-conds lst env)
  (if (null? lst)
      (values)
      (if (equal? (caar lst) 'else)
          (evaluate (cadar lst) env)
          (if (evaluate (caar lst) env)
              (evaluate (cadar lst) env)
              (eval-conds (cdr lst) env)))
))

(define (put-in-genv! elmt env) 
  (set-cdr! env (cons (car env) (cdr env)))
  (set-car! env elmt)
        
)

(define (bind-parms lstP lstV lstA)
  (if (equal? lstP '())
      lstA
      (cons (cons (car lstP) (car lstV)) (bind-parms (cdr lstP) (cdr lstV)
                                                     lstA))
      ))

(define (load-help port file env)
  (let ((input (read port)))

  (if (eof-object? input)
      (begin
        (display "Loading ")
        (write file)
        (display "..."))
      (begin
        (evaluate input env)
        (load-help port file env)))))
  
    
(define (evaluate expr env)
  (cond 

   ; Self Evaluating expressions:
   ((number? expr) expr)
   ((string? expr) expr)
   ((boolean? expr) expr)
   ((null? expr) expr)
   ((char? expr) expr)


   ; If it is a symbol?
   ((symbol? expr) 
    (if (in-env? expr env)
        (cdr (assoc expr env))
        expr))

  ;list? lets get to work...
  ((list? expr)
   (cond
    ; --- if --- done
    ((equal? (car expr) 'if)
    (let (( bexpr (evaluate (cadr expr) env))
          (then-clause (caddr expr))
          (else-clause (cadddr expr)))
      (if bexpr 
          (evaluate then-clause env)
          (evaluate else-clause env)
          )))

    ; --- let --- done
    ((equal? (car expr) 'let)
     ((lambda (lst proc)
       (evaluate proc (update-env (make-assoc lst) env)))
       (cadr expr) (cons 'begin (cddr expr))))

    ; --- begin --- done
    ((equal? (car expr) 'begin)
       (ret-last (eval-lst (cdr expr) env)))
     

    ; --- define --- done
    ((equal? (car expr) 'define)
     (if (list? (cadr expr))
         (evaluate
          (cons 'define (cons (caadr expr) (cons (cons 'lambda 
                (cons (cdadr expr) 
                      (cons (caddr expr)
                            '()))) '()))) env)                 
     (begin
           (put-in-genv! (cons (cadr expr) (evaluate (caddr expr) env)) env)
           (cadr expr))))
     
    ; --- cond --- done
    ((equal? (car expr) 'cond)
     (eval-conds (cdr expr) env))

    ; --- eval --- done
    ((equal? (car expr) 'eval)
     (evaluate (cadr expr) env))

    ; --- lambda --- done
    ((equal? (car expr) 'lambda)
      (cons 'closure
            (cons expr
             (cons env '()))))

    ; --- load ---
    (( equal? (car expr) 'load)
     (load-help (open-input-file (cadr expr))  (cadr expr) env))
       

    ; --- quote --- done
    ((equal? (car expr) 'quote)
     (cadr expr))
    
    ; --- apply --- done
     ((equal? (car expr) 'apply)
      (if (list? (assoc (cadr expr) env))
          (evaluate (cons (cadr expr) (evaluate (caddr expr) env)) env)
          (apply (evaluate (cadr expr) env) (evaluate (caddr expr) env))))

    ; --- closure --- done
     ((list? (car expr))
      (if (equal? (caar expr) 'closure)
          (evaluate (cons 'begin (cddadar expr)) 
                    (bind-parms (cadadar expr) (cdr expr) env))
          (evaluate (cons (evaluate (car expr) env) (cdr expr)) env)))

     ;--- function application
     ((in-env? (car expr) env)
        (if (list? (cdr (assoc (car expr) env)))
            (evaluate (eval-lst expr env) env)
            (apply (evaluate (car expr) env) (eval-lst (cdr expr) env))))

     (else (display "ruh-roh "))
                         
))))

; Read Evaluate Print Loop 
(define (repl)
  (display "\n")
  (display "]=> ")
  (define reading (read))
    
  (if (equal? reading 'exit)
      '()
      (begin
        
        (display "\n")
        (let ((e (evaluate reading genv)))
          (if (equal? e #!unspecific)
              (display "\n")
              (let ((v ";Value: "))
                (display v)
                (write e)
                (display "\n"))))
        (repl)))

 )
(begin
  (display "\n")
  (repl))

