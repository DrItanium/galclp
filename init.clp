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
(include logic/parent_ident/module.clp)
(include logic/stages/types.clp)
(include logic/annotations/types.clp)
(include logic/pld/types.clp)
(include logic/parent_ident/types.clp)
(include logic/expression/types.clp)

(deffunction MAIN::begin
             ()
             )

(deffacts MAIN::stages
          (stage (current optimization-stage1)
                 (rest flatten
                   discovery
                   correlate
                   cleanup
                   display)))

(deffacts MAIN::allowed-identity-conversions
          (convert-to-identity and-expression)
          (convert-to-identity or-expression))

(deffacts MAIN::allowed-flattening-targets
          (can-flatten and-expression)
          (can-flatten or-expression))

(deffacts MAIN::clone-requests
          (annotation-clone-request (target-kind reverse-non-expression-node)
                                    (new-name input-to)))

(deffacts MAIN::pld-emission-requests
          (annotation (target display)
                      (kind focus-on-stage)
                      (reversible FALSE)
                      (args EmitPLDLogic)))

(deffacts MAIN::structurally-similar-unary-nodes
          (annotation (target not-expression)
                      (kind allow-fusion)
                      (args))
          (annotation (target identity-expression)
                      (kind allow-fusion)
                      (args)))

(include logic/parent_ident/logic.clp)
(include logic/pld/logic.clp)
(include logic/annotations/logic.clp)
(include logic/stages/logic.clp)
(include logic/expression/logic.clp)

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



;; @todo reimplement redundant expression detection after I implement flattening
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


(defrule MAIN::display-top-levels-to-console
         (stage (current display))
         ?f <- (object (is-a expression)
                       (parent FALSE))
         =>
         (printout stdout (send ?f to-string) crlf))

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

(defrule MAIN::go-to-focus
         (stage (current ?stage))
         (annotation (target ?stage)
                     (kind focus-on-stage)
                     (args $?modules))
         =>
         (focus (expand$ ?modules)))


(defrule MAIN::drop-expressions-with-no-children
         "Drop expressions with no children"
         ?f <- (object (is-a expression)
                       (name ?name)
                       (children))
         =>
         (assert (removed ?name))
         (unmake-instance ?f))

(defrule MAIN::remove-dropped-children
         "If we find instance names which no longer reference anything, then remove them!"
         ?x <- (removed ?b)
         ?f <- (object (is-a expression)
                       (children $?a ?b $?c))
         =>
         (retract ?x)
         (modify-instance ?f
                          (children ?a ?c)))
(defrule MAIN::fuse-structurally-similar-nodes
         "(*and /A /A ...) => (*and /A ...) || (*and A A ...) => (*and A ...)"
         ;(stage (current cleanup))
         (annotation (target ?kind)
                     (kind allow-fusion))
         ?f <- (object (is-a and-expression)
                       (name ?parent)
                       (children $?a ?first $?c ?second $?e))
         (object (is-a ?kind)
                 (name ?first)
                 (children ?k))
         (object (is-a ?kind)
                 (name ?second)
                 (children ?k))
         =>
         (unmake-instance ?second)
         (assert (removed ?second))
         (modify-instance ?f
                          (children ?a ?first ?c ?e)))

;; These are bad rules since T and F will always be false which would eliminate the entire clause
(defrule MAIN::cancel-out-true-false-pairs:following-expression
         "(*and A (*not A) ...) => (and ...)"
         ;(stage (current cleanup))
         ?f <- (object (is-a and-expression)
                       (name ?parent)
                       (children $?a ?b $?c ?d $?e))
         ?z <- (object (is-a not-expression)
                       (name ?d)
                       (children ?b))
         =>
         (unmake-instance ?z)
         (assert (removed ?d))
         (modify-instance ?f
                          (children ?a ?c ?e)))

(defrule MAIN::cancel-out-true-false-pairs:leading-expression
         "(*and (*not A) A...) => (and ...)"
         ;(stage (current cleanup))
         ?f <- (object (is-a and-expression)
                       (name ?parent)
                       (children $?a ?b $?c ?d $?e))
         ?z <- (object (is-a not-expression)
                       (name ?b)
                       (children ?d))
         =>
         (unmake-instance ?z)
         (assert (removed ?b))
         (modify-instance ?f
                          (children ?a ?c ?e)))
(defrule MAIN::cancel-out-true-false-pairs:following-expression-identity
         "(*and A (*not A) ...) => (and ...)"
         ;(stage (current cleanup))
         ?f <- (object (is-a and-expression)
                       (name ?parent)
                       (children $?a ?b $?c ?d $?e))
         ?r <- (object (is-a identity-expression)
                       (name ?b)
                       (children ?k))
         ?z <- (object (is-a not-expression)
                       (name ?d)
                       (children ?k))
         =>
         (unmake-instance ?r ?z)
         (assert (removed ?b)
                 (removed ?d))
         (modify-instance ?f
                          (children ?a ?c ?e)))

(defrule MAIN::cancel-out-true-false-pairs:leading-expression-identity
         "(*and (*not A) A ...) => (and ...)"
         ;(stage (current cleanup))
         ?f <- (object (is-a and-expression)
                       (name ?parent)
                       (children $?a ?b $?c ?d $?e))
         ?r <- (object (is-a identity-expression)
                       (name ?d)
                       (children ?k))
         ?z <- (object (is-a not-expression)
                       (name ?b)
                       (children ?k))
         =>
         (unmake-instance ?r ?z)
         (assert (removed ?b)
                 (removed ?d))
         (modify-instance ?f
                          (children ?a ?c ?e)))
