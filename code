#lang pl 2
#|------------------Q1------------------|#

#|
<char> ::= 0|1|2|3|4|5|6|7|8|9

<charseqe> ::= <char>
               |<char><charseq>

<form> ::= \#<char>

<emptystring> ::= λ

<string> ::= "" || <form> | <<charseq>> | "<charseq>"

<stringseq> ::= <string> 
                |<string><emptystring><stringseq> //with blank in the middle

<SE> :: = <charseq> | <stringseq>
         |{string <SE>}
         |{string-length <SE>}
         |{string-append <SE><SE>}
         |{string-insert <SE>}
         |{number->string <SE>}

Derivation process:
<SE> = ( string-append ( string #\1 #\2 #\4 ) "12" )
(string-append <SE><SE>) => (string-append (string <SE>)<SE>) => (string-append (string <stringseq>)<SE>)
=> (string-append (string <string><emptystring><stringseq>)<SE>)
=>(string-append (string <form><emptystring><stringseq>)<SE>)
=>(string-append (string \#<char><emptystring><stringseq>)<SE>)
=>(string-append (string \#1λ<stringseq>)<SE>)//this process of stringseq is turning 3 times in the same way until the next step:
=>(string-append (string \#1λ#\2λ<string>)<SE>)
=>(string-append (string \#1λ#\2λ\#4)<SE>)
=>(string-append (string \#1λ#\2λ\#4)<stringseq>)
=>(string-append (string \#1λ#\2λ\#4)<string>)
=>(string-append (string \#1λ#\2λ\#4)"<char><charseq>")
=>(string-append (string \#1λ#\2λ\#4)"1<char>")
=>(string-append (string \#1λ#\2λ\#4)"12")
|#

#|------------------Q2------------------|#

(: squares : Number Number -> Number)
(define (squares duble second)
  (+ second(* duble duble)))
  
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lt)
  (foldl squares 0 lt)
 )

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(3 2 1)) => 14)
(test (sum-of-squares '(5 4 8 9 10)) => 286)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(-1)) => 1)
(test (sum-of-squares '(-1 -2 -3)) => 14)


#|------------------Q3-A------------------|#


(: createPolynomial : (Listof Number) -> (Number -> Number)) 
(define (createPolynomial coeffs) 
  (: poly : (Listof Number) Number Integer Number -> 
Number) 
  (define (poly argsL x power accum) 
     (if (null? argsL)  
         accum
         (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power))))))
  (: polyX : Number -> Number) 
  (define (polyX x)
    (poly coeffs x 0 0))
polyX)

 
(define p2345 (createPolynomial '(2 3 4 5)))

(test (p2345 0) =>  
   (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))

(test (p2345 4) =>  
   (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))

(test (p2345 11) =>
      (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
 
 
(define p536 (createPolynomial '(5 3 6)))

(test (p536 11) =>
      (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2)))) 
 
(define p_0 (createPolynomial '())) (test (p_0 4) =>
                                          0) 


#|------------------Q3-B1------------------|#

#| 
  The grammar: 
    <PLANG> ::= {{poly <AES>} {<AEs>}} -1
    
     <AEs>::=  <AE>           -1
             | <AE> <AEs>     -2
    
     <AE>::=    <num>         -1  (<num> is number in Racket)
           | {+ <WAE> <WAE> } -2
           | {- <WAE> <WAE> } -3
           | {* <WAE> <WAE> } -4
           | {/ <WAE> <WAE> } -5
|#

#|------------------Q3-B2------------------|#

  (define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE])

  (: parse-sexpr : Sexpr -> AE) 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs)(parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs)(parse-sexpr rhs))] 
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

  (: parse-sexprB : (Listof Sexpr) -> (Listof AE))
  (define (parse-sexprB sexpr_list)
(if (null? sexpr_list)
        null
        (cons (parse-sexpr (first sexpr_list))(parse-sexprB (rest sexpr_list))))
    )

   
(: parse : String -> PLANG) 
;; parses a string containing a PLANG expression to a PLANG AST
 (define (parse str) 
  (let ([code (string->sexpr str)])
      (match code
      [(list (list 'poly c ...) (list )) (error 'parse-sexpr "parse: at least one point is required in ~s" code)]
      [(list (list 'poly) (list p ...)) (error 'parse-sexpr "parse: at least one coefficient is required in ~s" code)]
      [(list (list 'poly c ...) (list p ...)) (Poly (parse-sexprB c) (parse-sexprB p))] 
      [else (error 'parse-sexpr "bad syntax in ~s" code)])))


(test (parse "{{poly 1 2 3} {1 2 3}}")  
     => (Poly (list (Num 1) (Num 2) (Num 3))  
              (list (Num 1) (Num 2) (Num 3))))

(test (parse "{{poly } {1 2} }")  
     =error> "parse: at least one coefficient is required in ((poly) (1 2))")

(test (parse "{{poly 1 2} {} }") =error>
      "parse: at least one point is required in ((poly 1 2) ())")
