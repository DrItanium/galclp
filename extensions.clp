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
; Extra logical constructs to help us out

(deffunction *xor
             (?a ?b)
             (*or (*and (*not ?a) ?b)
                  (*and ?a (*not ?b))))
(deffunction *xnor 
             (?a ?b)
             (*or (*and ?a ?b)
                  (*and (*not ?a)
                        (*not ?b))))
(deffunction *neq
             (?a ?b)
             (*xor ?a ?b))
(deffunction *eq
             (?a ?b)
             (*xnor ?a ?b))

(deffunction *eq2
             (?a0 ?a1
                  ?b0 ?b1)
             (*and (*eq ?a0 ?b0)
                   (*eq ?a1 ?b1)))
(deffunction *eq3
             (?a0 ?a1 ?a2
                  ?b0 ?b1 ?b2)
             (*and (*eq2 ?a0 ?a1
                         ?b0 ?b1)
                   (*eq ?a2 ?b2)))
(deffunction *eq4
             (?a0 ?a1 ?a2 ?a3
                  ?b0 ?b1 ?b2 ?b3)
             (*and (*eq3 ?a0 ?a1 ?a2
                         ?b0 ?b1 ?b2)
                   (*eq ?a3 ?b3)))

;; @todo add support for multiple parents for sub expressions
(deffunction *mux1->2
             (?cond ?a ?b)
             (*or (*and (*not ?cond) ?a)
                  (*and ?cond ?b)))

(deffunction *mux2->4
             (?c0 ?c1 ?a ?b ?c ?d)
             (*mux1->2 ?c1
                       (*mux1->2 ?c0 ?a ?b)
                       (*mux1->2 ?c0 ?c ?d)))
(deffunction *mux3->8
             (?c0 ?c1 ?c2 ?a ?b ?c ?d ?e ?f ?g ?h)
             (*mux1->2 ?c2
                       (*mux2->4 ?c0 ?c1 ?a ?b ?c ?d)
                       (*mux2->4 ?c0 ?c1 ?e ?f ?g ?h)))

(deffunction *half-adder
             (?a ?b)
             (create$ (*xor ?a ?b)
                      (*and ?a ?b)))
(deffunction *full-adder
             (?a ?b ?c)
             (bind ?ha0 
                   (*half-adder ?a 
                                ?b))
             (bind ?ha1 
                   (*half-adder ?c 
                                (nth$ 1 
                                      ?ha0)))
             (create$ (nth$ 1 
                            ?ha1)
                      (*or (nth$ 2 ?ha1)
                           (nth$ 2 ?ha0))))

(deffunction *mul2 
             (?a0 ?a1
                  ?b0 ?b1)
             (bind ?p0
                   (*and ?a0 ?b0))
             (bind ?ha0
                   (*half-adder (*and ?a1
                                      ?b0)
                                (*and ?a0
                                      ?b1)))
             (bind ?ha1
                   (*half-adder (nth$ 2 
                                      ?ha0)
                                (*and ?a1
                                      ?b1)))
             ; return a multifield so that we have to operate on it correctly
             (create$ ?p0
                      (nth$ 1 ?ha0)
                      (nth$ 1 ?ha1)
                      (nth$ 2 ?ha1)))

