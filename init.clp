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
(defmodule MAIN
           (export ?ALL))
(include logic/source-ident/module.clp)
(include logic/pld/module.clp)
(include lib/stage.clp)
(deffunction MAIN::begin
             ()
             )

(deffacts MAIN::stages
          (stage (current optimization-stage1)
                 (rest flatten
                       discovery
                       correlate
                       display)))






(include logic/annotations/types.clp)
(include logic/pld/types.clp)
(include logic/parent_ident/types.clp)
(include logic/expression/types.clp)
(include lib/extensions.clp)

(include logic/parent_ident/logic.clp)
(include logic/pld/logic.clp)
(include logic/annotations/logic.clp)

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
         (bind ?statements 
               (create$))
         (progn$ (?c ?children)
                 (bind ?statements
                       ?statements
                       (*not ?c)))
         (make-instance ?parent of or-expression
                        (children ?statements)))

;; @todo reimplement redundant expression detection after I implement flattening
(deffacts MAIN::allowed-identity-conversions
          (convert-to-identity and-expression)
          (convert-to-identity or-expression))
(defrule MAIN::perform-convert-to-identity
         (stage (current optimization-stage1))
         (convert-to-identity ?expr)
         ?f <- (object (is-a ?expr)
                       (name ?name)
                       (children ?a ?a))
         =>
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
(deffacts MAIN::allowed-flattening-targets
          (can-flatten and-expression)
          (can-flatten or-expression))

(defrule MAIN::flatten-legal-expressions
         "if we detect that we have an expression that has the same type as a child then merge; register new types via the fact interface to take advantage of this"
         (stage (current flatten))
         (can-flatten ?target)
         ?f <- (object (is-a ?target)
                       (name ?name)
                       (parent ?p)
                       (children $?children))
         ?f2 <- (object (is-a ?target)
                        (name ?p)
                        (children $?a ?name $?b))
         =>
         (unmake-instance ?f)
         (modify-instance ?f2 
                          (children $?a ?children $?b)))

(defrule MAIN::fixup-any-accidental-single-binary
         (stage (current flatten))
         (convert-to-identity ?expr)
         ?f <- (object (is-a ?expr)
                       (name ?name)
                       (children ?a))
         =>
         (unmake-instance ?name)
         (make-instance ?name of identity-expression
                        (children ?a)))

(defrule MAIN::remove-redundant-expressions
         (stage (current flatten))
         ?f <- (object (is-a expression)
                       (children $?a ?b $?c ?b $?d))
         =>
         (modify-instance ?f
                          (children ?a ?b ?c ?d)))


(defrule MAIN::annotate-child-expression-nodes
         (stage (current discovery))
         (object (is-a expression)
                 (children $? ?child $?)
                 (name ?name))
         (object (is-a expression)
                 (name ?child))
         =>
         (assert (annotation (target ?name)
                             (kind expression-node)
                             (args ?child))))

(defrule MAIN::annotate-non-expression-nodes
         (stage (current discovery))
         (object (is-a expression)
                 (children $? ?child $?)
                 (name ?name))
         (not (object (is-a expression)
                      (name ?child)))
         =>
         (assert (annotation (target ?name)
                             (kind non-expression-node)
                             (args ?child))))




(deffacts MAIN::clone-requests
          (annotation-clone-request (target-kind reverse-non-expression-node)
                                    (new-name input-to)))

(defrule MAIN::identify-leaf-nodes
         (declare (salience -1))
         (stage (current discovery))
         (object (is-a expression)
                 (name ?name))
         (not (annotation (target ?name)
                          (kind expression-node)))
         =>
         (assert (annotation (target ?name)
                             (kind leaf-node)
                             (args))))



(defrule MAIN::emit-plds
         (stage (current display))
         =>
         ; walk into emitter logic
         (focus EmitPLDLogic))
