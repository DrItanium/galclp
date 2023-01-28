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
(deftemplate stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))
(deffacts MAIN::stages
          (stage (current optimization-stage1)
                 (rest flatten
                       optimization-stage2
                       display)))
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
        (default ?NONE))
  (multislot children
             (storage local)
             (visibility public)))


(defclass MAIN::binary-expression
  (is-a expression)
  (multislot children
             (source composite)
             (default ?NONE))
  (slot operator
        (storage shared)
        (visibility public)
        (default NEED_TO_OVERRIDE!))
  (message-handler to-string primary))

(defmessage-handler binary-expression to-string primary
                    ()
                    (str-cat
                             (send (nth$ 1 
                                         (dynamic-get children))
                                   to-string)
                             " "
                             (dynamic-get operator)
                             " "
                             (send (nth$ 2 
                                         (dynamic-get children))
                                   to-string)))
(defclass MAIN::unary-expression
  (is-a expression)
  (multislot children
             (source composite)
             (range 1 1)
             (default ?NONE))
  (slot operator
        (storage shared)
        (visibility public)
        (default NEED_TO_OVERRIDE!))
  (message-handler to-string primary))

(defmessage-handler unary-expression to-string primary
                    ()
                    (str-cat (dynamic-get operator) 
                             (send (nth$ 1 
                                         (dynamic-get children))
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
                 (children ?left ?right)))
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
                 (children ?a ?b)))
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
                            (children ?a)))

(deffunction *assign
             (?dest ?expression)
             (make-instance of assignment-expression
                            (children ?dest
                                      ?expression)))
(deffunction *identity
             (?a)
             (make-instance of identity-expression
                            (children ?a)))

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
                 (name ?parent)
                 (children $? ?n $?))
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
                          (children ?a 
                                    (duplicate-instance ?n (parent FALSE))
                                    ?b)))

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
;; in case we accidentally created unintentional copies we need to fix them up
(defrule MAIN::validate-no-duplicate-entries
         (declare (salience 9999))
         (object (is-a expression)
                 (name ?name)
                 (children $? ?n&:(instancep ?n) $?))
         ?f2 <- (object (is-a expression)
                        (name ~?name)
                        (children $?a ?n $?b))
         =>
         (modify-instance ?f2
                          (children ?a (duplicate-instance ?n (parent FALSE)) ?b)))

;; reductions

(defrule MAIN::eliminate-not-not
         "(not (not ?)) should be factored out to an identity node"
         ?nested <- (object (is-a not-expression)
                            (parent ?parent)
                            (name ?nest)
                            (children ?node))
         ?p <- (object (is-a not-expression)
                       (kind not)
                       (name ?parent)
                       (children ?nest))
         =>
         (unmake-instance ?nested ?p)
         (recompute-parent ?node)
         ; turn not-not into an identity node instead
         (make-instance ?parent of identity-expression
                        (children ?node)))

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

(defrule MAIN::identity-binary-merge
         "(identity (binary-expr)) => (binary-expr)"
         ?nested <- (object (is-a binary-expression)
                            (parent ?parent)
                            (name ?nest)
                            (children $?nodes)) 
         ?p <- (object (is-a identity-expression)
                       (name ?parent)
                       (parent ?grand)
                       (children ?nest))
         =>
         (bind ?kind
               (class ?nested))
         (unmake-instance ?nested ?p)
         (make-instance ?parent of ?kind
                        (parent ?grand)
                        (children $?nodes)))

(defrule MAIN::binary-identity-merge
         "(binary-expr (identity) ?) => (binary-expr ? ?)"
         ?nested <- (object (is-a identity-expression)
                            (parent ?parent)
                            (name ?node)
                            (children ?nest))
         ?p <- (object (is-a binary-expression)
                            (name ?parent)
                            (children $?a ?node $?b))
         =>
         (recompute-parent ?nest)
         (unmake-instance ?nested)
         (modify-instance ?p 
                          (children ?a ?nest ?b)))
(defrule MAIN::distribute-and-to-or:left
         " (and (or Q R) P) => (or (and P Q) (and P R))"
         (stage (current optimization-stage1))
         ?f <- (object (is-a and-expression)
                       (children ?orexp ?other)
                       (name ?top))
         ?k <- (object (is-a or-expression)
                       (name ?orexp)
                       (parent ?top)
                       (children ?left ?right))

         =>
         (recompute-parent ?left
                           ?right)
         (unmake-instance ?f ?k)
         (make-instance ?top of or-expression
                        (children (*and ?other
                                        ?left)
                                  (*and ?other
                                        ?right))))


(defrule MAIN::distribute-and-to-or:right
         " (and P (or Q R)) => (or (and P Q) (and P R))"
         (stage (current optimization-stage1))
         ?f <- (object (is-a and-expression)
                       (children ?other ?orexp)
                       (name ?top))
         ?k <- (object (is-a or-expression)
                       (name ?orexp)
                       (parent ?top)
                       (children ?left ?right))
         =>
         (recompute-parent ?left
                           ?right)
         (unmake-instance ?f ?k)
         (make-instance ?top of or-expression
                        (children (*and ?other 
                                        ?left)
                                  (*and ?other
                                        ?right))))
;; we don't support distributing or to and for two reasons:
;; 1. The gal chips treat or statements as separate groups of and statements
;; 2. Implementing both kinds will result in an infinite loop!


(defrule MAIN::demorgan-nor
         " (not (or A B)) => (and (not A) (not B))"
         ?f <- (object (is-a or-expression)
                       (parent ?parent)
                       (name ?orexp)
                       (children $?children))
         ?k <- (object (is-a not-expression)
                       (name ?parent)
                       (children ?orexp))
         =>
         (unmake-instance ?f ?k)
         (recompute-parent $?children)
         (bind ?statements 
               (create$))
         (progn$ (?c ?children)
                 (bind ?statements
                       ?statements
                       (*not ?c)))
         (make-instance ?parent of and-expression
                        (children ?statements)))

(defrule MAIN::demorgan-nand
         " (not (and A B)) => (or (not A) (not B))"
         ?f <- (object (is-a and-expression)
                       (parent ?parent)
                       (name ?orexp)
                       (children $?children))
         ?k <- (object (is-a not-expression)
                       (name ?parent)
                       (children ?orexp))
         =>
         (unmake-instance ?f ?k)
         (recompute-parent $?children)
         (bind ?statements 
               (create$))
         (progn$ (?c ?children)
                 (bind ?statements
                       ?statements
                       (*not ?c)))
         (make-instance ?parent of or-expression
                        (children ?statements)))

;; @todo reimplement redundant expression detection after I implement flattening

(defrule MAIN::convert-identity-and/or-to-identity
         (stage (current optimization-stage1))
         ?f <- (object (is-a and-expression|or-expression)
                       (name ?name)
                       (children ?a ?a))
         =>
         (recompute-parent ?a)
         (unmake-instance ?name)
         (make-instance ?name of identity-expression
                        (children ?a)))

(defrule MAIN::next-stage
         (declare (salience -10000))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f 
                 (current ?next)
                 (rest ?rest)))

(defrule MAIN::display-top-levels-to-console
         (stage (current display))
         ?f <- (object (is-a expression)
                       (parent FALSE))
         =>
         (printout stdout (send ?f to-string) crlf))
