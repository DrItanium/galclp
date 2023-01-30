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

; Core logic for handling annotations and things like clone requests and such

(defrule MAIN::merge-redundant-annotations
         ?f <- (annotation (target ?target)
                           (kind ?k)
                           (args $?args0))
         ?f2 <- (annotation (target ?target)
                            (kind ?k)
                            (args $?args1))
         (test (neq ?f ?f2))
         =>
         (modify ?f 
                 (args ?args0 ?args1))
         (retract ?f2))

(defrule MAIN::define-reverse-annotation
         "In the cases where args is non empty then we can do a reverse back channel easily"
         ?f <- (annotation (target ?target)
                           (kind ?kind)
                           (reversible TRUE)
                           (args $?args))
         =>
         (progn$ (?a ?args)
                 (duplicate ?f
                            (target ?a)
                            (reversible FALSE)
                            (kind (sym-cat reverse- ?kind))
                            (args ?target))))

(defrule MAIN::settify-annotation-args
         "If the annotation isn't order dependent then it means you want it to only contain unique entries"
         ?f <- (annotation (order-is-important FALSE)
                           (args $?a ?b $?c ?b $?d))
         =>
         (modify ?f 
                 (args ?a ?b ?c ?d)))

(defrule MAIN::fulfill-clone-request
         (annotation-clone-request (target-kind ?kind)
                                   (new-name ?new-kind))
         ?f <- (annotation (kind ?kind))
         =>
         (duplicate ?f 
                    (kind ?new-kind)))
