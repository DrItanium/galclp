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
  (slot output-path
        (type LEXEME)
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

