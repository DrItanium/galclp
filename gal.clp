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
        (default ?NONE))
  (slot kind
        (type LEXEME)
        (storage local)
        (visibility public)
        (default ?NONE))
  (multislot children
             (storage local)
             (visibility public)))
(deffunction defexpression
             (?parent ?kind $?children)
             (make-instance of expression
                            (kind ?kind)
                            (parent ?parent)
                            (children $?children)))
(deffunction *and
             (?a ?b $?rest)
             (defexpression FALSE 
                            and 
                            ?a 
                            ?b 
                            $?rest))
(deffunction *or
             (?a ?b $?rest)
             (defexpression FALSE 
                            or 
                            ?a 
                            ?b 
                            $?rest))

(deffunction *not
             (?a)
             (defexpression FALSE
                            not
                            ?a))

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
(deffunction *nand
             (?a ?b)
             (*not (*and ?a ?b)))

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
             (?thing)
             (assert (recompute parents for ?thing)))
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
         (modify-instance ?child 
                          (parent ?parent)))
(defrule MAIN::recompute-parent-success
         (declare (salience 10000))
         ?f <- (recompute parents for ?contents)
         ?x <- (object (is-a expression)
                       (name ?contents))
         =>
         (retract ?f)
         (modify-instance ?x 
                          (parent FALSE)))

(defrule MAIN::recompute-parent-fail
         (declare (salience 10000))
         ?f <- (recompute parents for ?contents)
         (test (not (instancep ?contents)))
         =>
         (retract ?f))



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

