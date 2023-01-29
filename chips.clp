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

; classes and logic around describing the different GAL chips we support (adaption of the GALasm 
; source code into CLIPS)

(defclass gal-olmc
  "stores information about a single Output Logic Macro Cell"
  (is-a USER)
  (slot active
        (storage local)
        (visibility public))
  (slot pin-type 
        (storage local)
        (visibility public))
  (slot tristate
        (storage local)
        (visibility public))
  (slot clock 
        (storage local)
        (visibility public))
  (slot async-reset
        (storage local)
        (visibility public))
  (slot async-preset
        (storage local)
        (visibility public))
  (slot feedback
        (storage local)
        (visibility public)))



(defclass gal-pin
  (is-a USER)
  (slot polarity
        (storage local)
        (visibility public))
  (slot index
        (type INTEGER)
        (storage local)
        (visibility public)
        (range 0 ?VARIABLE)))

(defclass fuse
  (is-a USER)
  (slot name 
        (storage local)
        (visibility public))
  (slot location
        (type INTEGER)
        (storage local)
        (visibility public)
        (range 0 ?VARIABLE)
        (default ?NONE)))

(deftemplate gal-chip-description
             (slot name
                   (type SYMBOL)
                   (default ?NONE))
             (slot olmc-count
                   (type INTEGER)
                   (range 0 ?VARIABLE)
                   (default ?NONE))
             (slot pin-count
                   (type INTEGER)
                   (range 0 ?VARIABLE)
                   (default ?NONE))
             (slot olmc-first-pin
                   (type INTEGER)
                   (range 0 ?VARIABLE)
                   (default ?NONE))
             (slot column-count
                   (type INTEGER)
                   (range 0 ?VARIABLE)
                   (default ?NONE)))
(defglobal MAIN
           ?*acw-adr* = 60
           ?*security-adr* = 61
           ?*erase-adr* = 63
           ?*sig-addr20ra10* = 40
           ?*max-fuse-adr20ra10* = 39
           ?*sig-addr22v10* = 44
           ?*max-fuse-adr22v10* = 43
           ?*sig-adr20* = 40
           ?*max-fuse-adr20* = 39
           ?*sig-adr16* = 32
           ?*max-fuse-adr16* = 31)
(deffacts MAIN::gal-chips
          (gal-chip-description (name GAL16V8)
                                (olmc-count 8)
                                (pin-count 20)
                                (olmc-first-pin 12)
                                (column-count (+ ?*max-fuse-adr16* 1)))
          (gal-chip-description (name GAL20V8)
                                (olmc-count 8)
                                (pin-count 24)
                                (olmc-first-pin 15)
                                (column-count (+ ?*max-fuse-adr20* 1)))

          (gal-chip-description (name GAL22RA10)
                                (olmc-count 10)
                                (pin-count 24)
                                (olmc-first-pin 14)
                                (column-count (+ ?*max-use-adr20ra10* 1)))

          (gal-chip-description (name GAL22V10)
                                (olmc-count 10)
                                (pin-count 24)
                                (olmc-first-pin 14)
                                (column-count (+ ?*max-use-adr22v10* 1)))
    )
