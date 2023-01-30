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
