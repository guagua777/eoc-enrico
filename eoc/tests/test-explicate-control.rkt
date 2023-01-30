#lang racket

(require "test-util.rkt")
(require "uniquify.rkt")
(require "remove-complex-oper.rkt")
(require "explicate-control.rkt")
(require "rvar.rkt")
(require "cvar.rkt")

(define programs
  (list
   (Program '() (Prim '+ (list (Int 2) (Int 3))))
   (Program '() (Prim '+ (list (Prim '- (list (Int 2))) (Int 3))))
   (Program '()
            (Let 'y (Let 'x (Int 20)
                         (Prim '+ (list (Var 'x) (Let 'x (Int 22) (Var 'x)))))
                    (Var 'y)))
   (Program '()
    (Let 'a (Int 42)
         (Let 'b (Var 'a)
              (Var 'b))))

   (Program '()
    (Prim '+ (list (Let 'x (Prim '+ (list (Prim '- (list (Int 1))) (Int 2)))
                       (Prim '+ (list (Var 'x) (Int 2))))
                   (Prim '+ (list (Int 4) (Int 5))))))))

(define (pass program) (explicate-control (remove-complex-opera* (uniquify program))))
(define (pass-2 program) (explicate-control (remove-complex-opera*-2 (uniquify program))))

(test-eq
 (pass (list-ref programs 0))
 (CProgram '()
           (list `(start .
                   ,(Return (Prim '+ (list (Int 2) (Int 3))))))))

(test-eq
 (pass-2 (list-ref programs 0))
 (CProgram '()
           (list `(start .
                   ,(Seq (Assign (Var 'tmp.0) (Int 2))
                     (Seq (Assign (Var 'tmp.1) (Int 3))
                      (Seq (Assign (Var 'tmp.2) (Prim '+ (list (Var 'tmp.0) (Var 'tmp.1))))
                       (Return (Var 'tmp.2)))))))))

(test-eq
 (pass (list-ref programs 1))
 (CProgram '()
           (list `(start .
                   ,(Seq (Assign (Var 'tmp.0) (Prim '- (list (Int 2))))
                     (Return (Prim '+ (list (Var 'tmp.0) (Int 3)))))))))

(test-eq
  (pass (list-ref programs 2))
  (CProgram '()
           `((start .
              ,(Seq (Assign (Var 'x.1) (Int 20))
                (Seq (Assign (Var 'x.2) (Int 22))
                 (Seq (Assign (Var 'y.1) (Prim '+ (list (Var 'x.1) (Var 'x.2))))
                  (Return (Var 'y.1)))))))))

(test-eq
 (pass (list-ref programs 3))
 (CProgram '()
          `((start .
             ,(Seq (Assign (Var 'a.1) (Int 42))
               (Seq (Assign (Var 'b.1) (Var 'a.1))
                (Return (Var 'b.1))))))))

(test-eq
 (pass (list-ref programs 4))
 (CProgram '()
           `((start .
              ,(Seq (Assign (Var 'tmp.0) (Prim '- (list (Int 1))))
                (Seq (Assign (Var 'x.1) (Prim '+ (list (Var 'tmp.0) (Int 2))))
                 (Seq (Assign (Var 'tmp.1) (Prim '+ (list (Var 'x.1) (Int 2))))
                  (Seq (Assign (Var 'tmp.2) (Prim '+ (list (Int 4) (Int 5))))
                   (Return (Prim '+ (list (Var 'tmp.1) (Var 'tmp.2))))))))))))

(for ([program programs])
  (test-eq (interp-RVar program)
           (interp-CVar (pass program))))
