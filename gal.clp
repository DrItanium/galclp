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
        (default ?NONE))
  (multislot children
             (storage local)
             (visibility public)))
(defclass MAIN::not-expression
  (is-a expression)
  (slot kind
        (source composite)
        (default-dynamic not))
  (multislot children
             (range 1 1)
             (source composite)
             (default ?NONE))
  (message-handler to-string primary))

(defmessage-handler not-expression to-string primary
                    ()
                    (str-cat " /" 
                             (send (nth$ 1 ?self:children) 
                                   to-string)
                             " "))

(defclass MAIN::identity-expression
  (is-a expression)
  (slot kind
        (source composite)
        (default-dynamic identity))
  (multislot children
             (range 1 1)
             (source composite)
             (default ?NONE))
  (message-handler to-string primary))

(defmessage-handler identity-expression to-string primary
                    ()
                    (str-cat (nth$ 1 ?self:children)))

(defclass MAIN::and-expression
  (is-a expression)
  (slot kind
        (source composite)
        (default-dynamic and))
  (multislot children
             (range 2 ?VARIABLE)
             (source composite)
             (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler and-expression to-string primary
                    ()
                    (bind ?result "")
                    (progn$ (?a ?self:children)
                            (bind ?result 
                                  (str-cat ?result " * " 
                                           (send ?a to-string)
                                           " ")))
                    ?result)

(defclass MAIN::or-expression
  (is-a expression)
  (slot kind
        (source composite)
        (default-dynamic or))
  (multislot children
             (range 2 ?VARIABLE)
             (source composite)
             (default ?NONE))
  (message-handler to-string primary))
(defmessage-handler and-expression to-string primary
                    ()
                    (bind ?result "")
                    (progn$ (?a ?self:children)
                            (bind ?result 
                                  (str-cat ?result " + " 
                                           (send ?a to-string)
                                           " ")))
                    ?result)

(deffunction defexpression
             (?parent ?kind $?children)
             (make-instance of expression
                            (kind ?kind)
                            (parent ?parent)
                            (children $?children)))
(deffunction *and
             (?a ?b $?rest)
             (make-instance of and-expression
                            (children ?a ?b ?rest)))
(deffunction *or
             (?a ?b $?rest)
             (make-instance of or-expression
                            (children ?a ?b ?rest)))

(deffunction *not
             (?a)
             (make-instance of not-expression
                            (children ?a)))

(deffunction *assign
             (?dest $?expression)
             (defexpression FALSE
                            assign
                            ?dest
                            $?expression))
(deffunction *identity
             (?a)
             (defexpression FALSE
                            identity
                            ?a))


(deffunction *xor
             (?a ?b)
             (*or (*and (*not ?a) ?b)
                  (*and ?a (*not ?b))))
(deffunction *neq
             (?a ?b)
             (*xor ?a ?b))
(deffunction *eq
             (?a ?b)
             (*not (*neq ?a ?b)))
;; @todo add support for multiple parents for sub expressions
(deffunction *mux21
             (?cond ?a ?b)
             (*or (*and (*not ?cond) ?a)
                  (*and ?cond ?b)))

(deffunction *mux42
             (?c0 ?c1 ?a ?b ?c ?d)
             (*mux21 ?c1
                    (*mux21 ?c0 ?a ?b)
                    (*mux21 ?c0 ?c ?d)))
(deffunction *mux83
             (?c0 ?c1 ?c2 ?a ?b ?c ?d ?e ?f ?g ?h)
             (*mux21 ?c2
                    (*mux42 ?c0 ?c1 ?a ?b ?c ?d)
                    (*mux42 ?c0 ?c1 ?e ?f ?g ?h)))
(deffunction recompute-parent
             ($?thing)
             (assert (recompute parents for ?thing)))
(deftemplate MAIN::parent-claim
             (slot parent
                   (type INSTANCE)
                   (default ?NONE))
             (slot target
                   (type INSTANCE)
                   (default ?NONE)))
;; parent recompute operations
(defrule MAIN::fix-parents
         (declare (salience 10000))
         ?child <- (object (is-a expression)
                           (parent FALSE)
                           (name ?n))
         (object (is-a expression)
                 (children $? ?n $?)
                 (name ?parent))
         =>
         (assert (parent-claim (parent ?parent)
                               (target ?n))))
(defrule MAIN::parent-collision-detected
         (declare (salience 10000))
         ?f <- (parent-claim (parent ?parent)
                             (target ?n))
         ?f2 <- (parent-claim (parent ?parent2)
                              (target ?n))
         (test (neq ?f ?f2))
         ?k <- (object (is-a expression)
                       (name ?parent2)
                       (children $?a ?n $?b))
         =>
         (retract ?f2)
         (modify-instance ?k
                          (children $?a (duplicate-instance ?n) $?b)))

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
         ?nested <- (object (is-a expression)
                            (kind not)
                            (parent ?parent)
                            (name ?nest)
                            (children ?contents))
         ?p <- (object (is-a expression)
                       (kind not)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested )
         (recompute-parent ?contents)
         ; turn not-not into an identity node instead
         (modify-instance ?p
                          (kind identity)
                          (children ?contents)))

(defrule MAIN::eliminate-identity-identity
         "(ident (ident ?)) should be flattened"
         ?nested <- (object (is-a expression)
                            (kind identity)
                            (parent ?parent)
                            (name ?nest)
                            (children ?contents))
         ?p <- (object (is-a expression)
                       (kind identity)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p 
                          (children ?contents)))

(defrule MAIN::identity-not-merge
         "(identity (not)) => not"
         ?nested <- (object (is-a expression)
                            (kind not)
                            (parent ?parent)
                            (name ?nest)
                            (children ?contents))
         ?p <- (object (is-a expression)
                       (kind identity)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (kind not)
                          (children ?contents)))

(defrule MAIN::not-identity-merge
         "(not (identity)) => not"
         ?nested <- (object (is-a expression)
                            (kind identity)
                            (parent ?parent)
                            (name ?nest)
                            (children ?contents))
         ?p <- (object (is-a expression)
                       (kind not)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (children ?contents)))

(defrule MAIN::merge-or-statements
         ?nested <- (object (is-a expression)
                            (kind or)
                            (parent ?parent)
                            (name ?nest)
                            (children $?contents))
         ?p <- (object (is-a expression)
                       (kind or)
                       (name ?parent)
                       (children $?a ?nest $?b))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (children ?a ?contents ?b)))


(defrule MAIN::merge-and-statements
         ?nested <- (object (is-a expression)
                            (kind and)
                            (parent ?parent)
                            (name ?nest)
                            (children $?contents))
         ?p <- (object (is-a expression)
                       (kind and)
                       (name ?parent)
                       (children $?a ?nest $?b))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (children ?a ?contents ?b)))

(defrule MAIN::convert-nand-to-not-if-makes-sense
         ?nested <- (object (is-a expression)
                            (kind and)
                            (parent ?parent)
                            (name ?nest)
                            (children ?child ?child))
         ?p <- (object (is-a expression)
                       (kind not)
                       (name ?parent)
                       (children ?nest))

         =>
         (unmake-instance ?nested)
         (recompute-parent ?child)
         (modify-instance ?p 
                          (children ?child)))

(defrule MAIN::convert-nor-to-not-if-makes-sense
         ?nested <- (object (is-a expression)
                            (kind or)
                            (parent ?parent)
                            (name ?nest)
                            (children ?child ?child))
         ?p <- (object (is-a expression)
                       (kind not)
                       (name ?parent)
                       (children ?nest))

         =>
         (unmake-instance ?nested)
         (recompute-parent ?child)
         (modify-instance ?p 
                          (children ?child)))

(defrule MAIN::identity-and-merge
         "(identity (and)) => and"
         ?nested <- (object (is-a expression)
                            (kind and)
                            (parent ?parent)
                            (name ?nest)
                            (children $?contents))
         ?p <- (object (is-a expression)
                       (kind identity)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (kind and)
                          (children $?contents)))

(defrule MAIN::identity-or-merge
         "(identity (or)) => or"
         ?nested <- (object (is-a expression)
                            (kind or)
                            (parent ?parent)
                            (name ?nest)
                            (children $?contents))
         ?p <- (object (is-a expression)
                       (kind identity)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (kind or)
                          (children $?contents)))
