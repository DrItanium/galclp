; galclp
; Copyright (c) 2023, Joshua Scoggins
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
; A simple expert system to make it easier to write gal equations with more complex features

(defmessage-handler OBJECT to-string primary () (str-cat ?self))
(defmessage-handler MULTIFIELD to-string primary () (implode$ ?self))
(deffunction recompute-parent
             ($?thing)
             (assert (recompute parents for ?thing)))
(defclass MAIN::expression
  (is-a USER)
  (slot parent
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (storage local)
        (visibility public)
        (default-dynamic FALSE))
  (slot kind
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE)))


(defclass MAIN::binary-expression
  (is-a expression)
  (slot left-child
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot right-child
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot operator
        (storage shared)
        (visibility public)
        (default NEED_TO_OVERRIDE!))
  (message-handler to-string primary))

(defmessage-handler binary-expression to-string primary
                    ()
                    (str-cat (send ?self:left-child 
                                   to-string)
                             " "
                             (dynamic-get operator)
                             " "
                             (send ?self:right-child
                                   to-string)))
(defclass MAIN::unary-expression
  (is-a expression)
  (slot child 
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot operator
        (storage shared)
        (visibility public)
        (default NEED_TO_OVERRIDE!))
  (message-handler to-string primary))

(defmessage-handler unary-expression to-string primary
                    ()
                    (str-cat (dynamic-get operator) 
                             (send ?self:child
                                   to-string)))


(defclass MAIN::not-expression
  (is-a unary-expression)
  (slot operator
        (source composite)
        (default "/"))
  (slot kind
        (source composite)
        (default-dynamic not)))


(defclass MAIN::identity-expression
  (is-a unary-expression)
  (slot operator
        (source composite)
        (default ""))
  (slot kind
        (source composite)
        (default-dynamic identity)))

(defclass MAIN::and-expression
  (is-a binary-expression)
  (slot operator
        (source composite)
        (default *))
  (slot kind
        (source composite)
        (default-dynamic and)))

(defclass MAIN::or-expression
  (is-a binary-expression)
  (slot operator
        (source composite)
        (default +))
  (slot kind
        (source composite)
        (default-dynamic or)))

(defclass MAIN::assignment-expression
  (is-a binary-expression)
  (slot operator
        (source composite)
        (default =))
  (slot kind
        (source composite)
        (default-dynamic assignment)))

(defclass MAIN::pld
  (is-a USER)
  (slot chip
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (slot title 
        (type SYMBOL)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot pinout
             (type STRING)
             (storage local)
             (visibility public)
             (default ?NONE))
  (multislot expressions
             (type INSTANCE)
             (storage local)
             (visibility public)
             (default ?NONE))
  (multislot description
             (type STRING)
             (storage local)
             (visibility public)
             (default ?NONE))
  (message-handler to-string primary))

(defmessage-handler pld to-string primary
                    (?router)
                    (printout ?router 
                              ?self:chip crlf
                              ?self:title crlf 
                              crlf)
                    (progn$ (?l ?self:pinout)
                            (printout ?router ?l crlf))
                    (progn$ (?x ?self:expressions)
                            (printout ?router 
                                      (send ?x to-string)
                                      crlf))
                    (printout ?router crlf crlf DESCRIPTION crlf)
                    (progn$ (?d ?self:description)
                            (printout ?router ?d crlf)))


(defgeneric *and)
(defmethod *and
  (?left ?right)
  (make-instance of and-expression
                 (left-child ?left)
                 (right-child ?right)))
(defmethod *and
  (?a ?b (?rest MULTIFIELD))
  (*and ?a ?b 
        (expand$ ?rest)))

(defmethod *and
  (?a ?b $?rest)
  (*and (*and ?a ?b)
        (expand$ ?rest)))

(defgeneric *or)
(defmethod *or
  (?a ?b)
  (make-instance of or-expression
                 (left-child ?a)
                 (right-child ?b)))
(defmethod *or
  (?a ?b $?rest)
  (*or (*or ?a ?b)
       (expand$ ?rest)))
(defmethod *or
  (?a ?b (?rest MULTIFIELD))
  (*or ?a ?b 
        (expand$ ?rest)))


(deffunction *not
             (?a)
             (make-instance of not-expression
                            (child ?a)))

(deffunction *assign
             (?dest ?expression)
             (make-instance of assignment-expression
                            (left-child ?dest)
                            (right-child ?expression)))
(deffunction *identity
             (?a)
             (make-instance of identity-expression
                            (child ?a)))

(deftemplate MAIN::parent-claim
             (slot parent
                   (type INSTANCE)
                   (default ?NONE))
             (slot target
                   (type INSTANCE)
                   (default ?NONE)))
;; parent recompute operations
(defrule MAIN::fix-parents:unary
         (declare (salience 10000))
         ?child <- (object (is-a expression)
                           (parent FALSE)
                           (name ?n))
         (object (is-a unary-expression)
                 (child ?n)
                 (name ?parent))
         =>
         (assert (parent-claim (parent ?parent)
                               (target ?n))))
(defrule MAIN::fix-parents:binary:left
         (declare (salience 10000))
         ?child <- (object (is-a expression)
                           (parent FALSE)
                           (name ?n))
         (object (is-a binary-expression)
                 (left-child ?n)
                 (name ?parent))
         =>
         (assert (parent-claim (parent ?parent)
                               (target ?n))))

(defrule MAIN::fix-parents:binary:right
         (declare (salience 10000))
         ?child <- (object (is-a expression)
                           (parent FALSE)
                           (name ?n))
         (object (is-a binary-expression)
                 (right-child ?n)
                 (name ?parent))
         =>
         (assert (parent-claim (parent ?parent)
                               (target ?n))))

(defrule MAIN::parent-collision-detected:binary:left
         (declare (salience 10000))
         ?f <- (parent-claim (parent ?parent)
                             (target ?n))
         ?f2 <- (parent-claim (parent ?parent2)
                              (target ?n))
         (test (neq ?f ?f2))
         ?k <- (object (is-a binary-expression)
                       (name ?parent2)
                       (left-child ?n))
         =>
         (retract ?f2)
         (modify-instance ?k
                          (left-child (duplicate-instance ?n))))

(defrule MAIN::parent-collision-detected:binary:right
         (declare (salience 10000))
         ?f <- (parent-claim (parent ?parent)
                             (target ?n))
         ?f2 <- (parent-claim (parent ?parent2)
                              (target ?n))
         (test (neq ?f ?f2))
         ?k <- (object (is-a binary-expression)
                       (name ?parent2)
                       (right-child ?n))
         =>
         (retract ?f2)
         (modify-instance ?k
                          (right-child (duplicate-instance ?n))))

(defrule MAIN::parent-collision-detected:unary
         (declare (salience 10000))
         ?f <- (parent-claim (parent ?parent)
                             (target ?n))
         ?f2 <- (parent-claim (parent ?parent2)
                              (target ?n))
         (test (neq ?f ?f2))
         ?k <- (object (is-a unary-expression)
                       (name ?parent2)
                       (child ?n))
         =>
         (retract ?f2)
         (modify-instance ?k
                          (child (duplicate-instance ?n))))

(defrule MAIN::recompute-parent-success
         (declare (salience 10000))
         ?f <- (recompute parents for ?contents $?rest)
         ?x <- (object (is-a expression)
                       (name ?contents))
         =>
         (retract ?f)
         (recompute-parent $?rest)
         (modify-instance ?x 
                          (parent FALSE)))

(defrule MAIN::recompute-parent-fail
         (declare (salience 10000))
         ?f <- (recompute parents for ?contents $?rest)
         (test (not (instancep ?contents)))
         =>
         (retract ?f)
         (recompute-parent ?rest))

(defrule MAIN::recompute-parent-done
         ?f <- (recompute parents for)
         =>
         (retract ?f))
(defrule MAIN::fulfill-parent-claims
         ?f <- (parent-claim (parent ?parent)
                             (target ?n))
         ?k <- (object (is-a expression)
                       (name ?n))
         =>
         (retract ?f)
         (modify-instance ?k
                          (parent ?parent)))


;; 

;; reductions

(defrule MAIN::eliminate-not-not
         "(not (not ?)) should be factored out to an identity node"
         ?nested <- (object (is-a not-expression)
                            (parent ?parent)
                            (name ?nest)
                            (child ?node))
         ?p <- (object (is-a not-expression)
                       (kind not)
                       (name ?parent)
                       (child ?nest))
         =>
         (unmake-instance ?nested ?p)
         (recompute-parent ?node)
         ; turn not-not into an identity node instead
         (make-instance ?parent of identity-expression
                        (child ?node)))

(defrule MAIN::eliminate-identity-identity
         "(ident (ident ?)) should be flattened"
         ?nested <- (object (is-a identity-expression)
                            (parent ?parent)
                            (name ?nest)
                            (child ?contents))
         ?p <- (object (is-a identity-expression)
                       (name ?parent)
                       (child ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p 
                          (child ?contents)))

(defrule MAIN::identity-not-merge
         "(identity (not)) => not"
         ?nested <- (object (is-a not-expression)
                            (kind not)
                            (parent ?parent)
                            (name ?nest)
                            (child ?contents))
         ?p <- (object (is-a identity-expression)
                       (kind identity)
                       (name ?parent)
                       (child ?nest))
         =>
         (unmake-instance ?nested ?p)
         (recompute-parent ?contents)
         (make-instance ?parent of not-expression
                        (child ?contents)))

(defrule MAIN::not-identity-merge
         "(not (identity)) => not"
         ?nested <- (object (is-a identity-expression)
                            (parent ?parent)
                            (name ?nest)
                            (child ?contents))
         ?p <- (object (is-a not-expression)
                       (name ?parent)
                       (child ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (child ?contents)))

(defrule MAIN::identity-binary-merge
         "(identity (binary-expr)) => or"
         ?nested <- (object (is-a binary-expression)
                            (parent ?parent)
                            (name ?nest)
                            (left-child ?left)
                            (right-child ?right))
         ?p <- (object (is-a identity-expression)
                       (name ?parent)
                       (parent ?grand)
                       (child ?nest))
         =>
         (bind ?kind
               (class ?nested))
         (unmake-instance ?nested ?p)
         (make-instance ?parent of ?kind
                        (parent ?grand)
                        (left-child ?left)
                        (right-child ?right)))

(defrule MAIN::distribute-and-to-or:left
         " (and (or Q R) P) => (or (and P Q) (and P R))"
         ?f <- (object (is-a and-expression)
                       (left-child ?orexp)
                       (right-child ?other)
                       (name ?top))
         ?k <- (object (is-a or-expression)
                       (name ?orexp)
                       (parent ?top)
                       (left-child ?left)
                       (right-child ?right))

         =>
         (recompute-parent ?left
                           ?right)
         (unmake-instance ?f ?k)
         (make-instance ?top of or-expression
                        (left-child (*and ?other
                                          ?left))
                        (right-child (*and ?other
                                           ?right))))


(defrule MAIN::distribute-and-to-or:right
         " (and P (or Q R)) => (or (and P Q) (and P R))"
         ?f <- (object (is-a and-expression)
                       (right-child ?orexp)
                       (left-child ?other)
                       (name ?top))
         ?k <- (object (is-a or-expression)
                       (name ?orexp)
                       (parent ?top)
                       (left-child ?left)
                       (right-child ?right))

         =>
         (recompute-parent ?left
                           ?right)
         (unmake-instance ?f ?k)
         (make-instance ?top of or-expression
                        (left-child (*and ?other
                                          ?left))
                        (right-child (*and ?other
                                           ?right))))
;; we don't support distributing or to and for two reasons:
;; 1. The gal chips treat or statements as separate groups of and statements
;; 2. Implementing both kinds will result in an infinite loop!


(defrule MAIN::demorgan-nor
         " (not (or A B)) => (and (not A) (not B))"
         ?f <- (object (is-a or-expression)
                       (parent ?parent)
                       (name ?orexp)
                       (left-child ?left)
                       (right-child ?right))
         ?k <- (object (is-a not-expression)
                       (name ?parent)
                       (child ?orexp))
         =>
         (unmake-instance ?f ?k)
         (recompute-parent ?left
                           ?right)
         (make-instance ?parent of and-expression
                        (left-child (*not ?left))
                        (right-child (*not ?right))))

(defrule MAIN::demorgan-nand
         " (not (and A B)) => (or (not A) (not B))"
         ?f <- (object (is-a and-expression)
                       (parent ?parent)
                       (name ?orexp)
                       (left-child ?left)
                       (right-child ?right))
         ?k <- (object (is-a not-expression)
                       (name ?parent)
                       (child ?orexp))
         =>
         (unmake-instance ?f ?k)
         (recompute-parent ?left
                           ?right)
         (make-instance ?parent of or-expression
                        (left-child (*not ?left))
                        (right-child (*not ?right))))
