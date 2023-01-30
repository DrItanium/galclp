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

;; parent recompute operations
(defrule MAIN::fix-parents
         (declare (salience 10000))
         (object (is-a expression)
                 (name ?parent)
                 (children $? ?n $?))
         ?child <- (object (is-a expression)
                           (name ?n)
                           (parent ~?parent))
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
         (declare (salience 10000))
         (object (is-a expression)
                 (name ?name)
                 (children $? ?n $?))
         ?f2 <- (object (is-a expression)
                        (name ~?name)
                        (children $?a ?n $?b))
         (object (is-a expression)
                 (name ?n))
         =>
         (modify-instance ?f2
                          (children ?a (duplicate-instance ?n (parent FALSE)) ?b)))
