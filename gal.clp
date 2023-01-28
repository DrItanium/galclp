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

(defmessage-handler unary-expression to-string primary
                    ()
                    (str-cat (dynamic-get operator) 
                             (send ?self:left-child 
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
  (is-a expression)
  (slot operator
        (source composite)
        (default *))
  (slot kind
        (source composite)
        (default-dynamic and)))

(defclass MAIN::or-expression
  (is-a expression)
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

(defgeneric *and)
(defmethod *and
  (?a ?b)
  (make-instance of and-expression
                 (children ?a ?b)))
(defmethod *and
  (?a ?b $?rest)
  (*and (*and ?a ?b)
        (expand$ ?rest)))

(defgeneric *or)
(defmethod *or
  (?a ?b)
  (make-instance of or-expression
                 (children ?a ?b)))
(defmethod *or
  (?a ?b $?rest)
  (*or (*or ?a ?b)
        (expand$ ?rest)))


(deffunction *not
             (?a)
             (make-instance of not-expression
                            (children ?a)))

(deffunction *assign
             (?dest $?expression)
             (make-instance of assignment-expression
                            (children ?dest $?expression)))
(deffunction *identity
             (?a)
             (make-instance of identity-expression
                            (children ?a)))

(deffunction *xor
             (?a ?b)
             (*or (*and (*not ?a) ?b)
                  (*and ?a (*not ?b))))
(deffunction *neq
             (?a ?b)
             (*xor ?a ?b))
(deffunction *eq
             (?a ?b)
             (*not (*xor ?a ?b)))

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
         (unmake-instance ?nested ?p)
         (recompute-parent ?contents)
         ; turn not-not into an identity node instead
         (make-instance ?parent of identity-expression
                        (children ?contents)))

(defrule MAIN::eliminate-identity-identity
         "(ident (ident ?)) should be flattened"
         ?nested <- (object (is-a identity-expression)
                            (parent ?parent)
                            (name ?nest)
                            (children ?contents))
         ?p <- (object (is-a identity-expression)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p 
                          (children ?contents)))

(defrule MAIN::identity-not-merge
         "(identity (not)) => not"
         ?nested <- (object (is-a not-expression)
                            (kind not)
                            (parent ?parent)
                            (name ?nest)
                            (children ?contents))
         ?p <- (object (is-a identity-expression)
                       (kind identity)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested ?p)
         (recompute-parent ?contents)
         (make-instance ?parent of not-expression
                        (children ?contents)))

(defrule MAIN::not-identity-merge
         "(not (identity)) => not"
         ?nested <- (object (is-a identity-expression)
                            (parent ?parent)
                            (name ?nest)
                            (children ?contents))
         ?p <- (object (is-a not-expression)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested)
         (recompute-parent ?contents)
         (modify-instance ?p
                          (children ?contents)))

(defrule MAIN::identity-and-merge
         "(identity (and)) => and"
         ?nested <- (object (is-a and-expression)
                            (parent ?parent)
                            (name ?nest)
                            (children $?contents))
         ?p <- (object (is-a identity-expression)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested ?p)
         (recompute-parent ?contents)
         (make-instance ?parent of and-expression
                        (children $?contents)))

(defrule MAIN::identity-or-merge
         "(identity (or)) => or"
         ?nested <- (object (is-a or-expression)
                            (parent ?parent)
                            (name ?nest)
                            (children $?contents))
         ?p <- (object (is-a identity-expression)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested ?p)
         (recompute-parent ?contents)
         (make-instance ?parent of or-expression
                        (contents ?contents)))
