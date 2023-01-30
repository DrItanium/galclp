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

(defmessage-handler OBJECT to-string primary () (str-cat ?self))
(defmessage-handler MULTIFIELD to-string primary () (implode$ ?self))

(defgeneric MAIN::*and)
(defgeneric MAIN::*or)

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
                    (bind ?message
                          (send (nth$ 1 (dynamic-get children)) 
                                to-string))
                    (progn$ (?c (rest$ (dynamic-get children)))
                            (bind ?message
                                  (str-cat ?message 
                                           " " (dynamic-get operator) " "
                                           (send ?c to-string))))
                    ?message)

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
  (multislot children
             (source composite)
             (range 2 2)
             (default ?NONE))
  (slot operator
        (source composite)
        (default =))
  (slot kind
        (source composite)
        (default-dynamic assignment)))

(defmethod MAIN::*and
  (?left ?right)
  (make-instance of and-expression
                 (children ?left ?right)))
(defmethod MAIN::*and
  (?a ?b (?rest MULTIFIELD))
  (*and ?a ?b
        (expand$ ?rest)))

(defmethod MAIN::*and
  (?a ?b $?rest)
  (*and (*and ?a ?b)
        (expand$ ?rest)))

(defmethod MAIN::*or
  (?a ?b)
  (make-instance of or-expression
                 (children ?a ?b)))
(defmethod MAIN::*or
  (?a ?b $?rest)
  (*or (*or ?a ?b)
       (expand$ ?rest)))
(defmethod MAIN::*or
  (?a ?b (?rest MULTIFIELD))
  (*or ?a ?b
       (expand$ ?rest)))


(deffunction MAIN::*not
             (?a)
             (make-instance of not-expression
                            (children ?a)))

(deffunction MAIN::*assign
             (?dest ?expression)
             (make-instance of assignment-expression
                            (children ?dest
                                      ?expression)))
(deffunction MAIN::*identity
             (?a)
             (make-instance of identity-expression
                            (children ?a)))
