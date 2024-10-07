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
          (stage (current inspect-pld)
                 (rest optimization-stage1
                       flatten
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

(deftemplate MAIN::pin-information
             (slot target
                   (type LEXEME
                         INSTANCE)
                   (default ?NONE))
             (slot title
                   (type LEXEME)
                   (default ?NONE))
             (slot index
                   (type INTEGER)
                   (range 1 ?VARIABLE)
                   (default ?NONE))
             (slot clock-pin
                   (type SYMBOL)
                   (allowed-symbols UNKNOWN
                                    FALSE
                                    TRUE))
             (slot connected
                   (type SYMBOL)
                   (allowed-symbols UNKNOWN
                                    FALSE
                                    TRUE))
             (slot power-pin
                   (type SYMBOL)
                   (allowed-symbols UNKNOWN
                                    FALSE
                                    TRUE)))
(defclass MAIN::pin
  (is-a USER)
  (slot parent
        (type SYMBOL
              INSTANCE)
        (allowed-symbols FALSE))
  (slot index
        (type INTEGER)
        (range 1 ?VARIABLE)
        (default ?NONE))
  (slot kind
        (type SYMBOL)
        (allowed-symbols INPUT
                         I/O
                         CLK/INPUT
                         OE/INPUT
                         POWER)
        (default ?NONE))
  (slot must-be
        (type SYMBOL)
        (default-dynamic FALSE)))


(defclass MAIN::logic-device
  (is-a USER)
  (slot title
        (type LEXEME)
        (default ?NONE))
  (slot pin-count
        (type INTEGER)
        (range 4 ?VARIABLE)
        (default ?NONE))
  (slot olmc-count
        (type INTEGER)
        (range 1 ?VARIABLE)
        (default ?NONE))
  (multislot pins
             (type INSTANCE)
             (default ?NONE)))
             
(definstances MAIN::supported-devices
              (GAL22V10.P1 of pin (index 1) (kind CLK/INPUT))
              (GAL22V10.P2 of pin (index 2) (kind INPUT))
              (GAL22V10.P3 of pin (index 3) (kind INPUT))
              (GAL22V10.P4 of pin (index 4) (kind INPUT))
              (GAL22V10.P5 of pin (index 5) (kind INPUT))
              (GAL22V10.P6 of pin (index 6) (kind INPUT))
              (GAL22V10.P7 of pin (index 7) (kind INPUT))
              (GAL22V10.P8 of pin (index 8) (kind INPUT))
              (GAL22V10.P9 of pin (index 9) (kind INPUT))
              (GAL22V10.P10 of pin (index 10) (kind INPUT))
              (GAL22V10.P11 of pin (index 11) (kind INPUT))
              (GAL22V10.P12 of pin (index 12) (kind POWER) (must-be GND))
              (GAL22V10.P13 of pin (index 13) (kind INPUT))
              (GAL22V10.P14 of pin (index 14) (kind I/O))
              (GAL22V10.P15 of pin (index 15) (kind I/O))
              (GAL22V10.P16 of pin (index 16) (kind I/O))
              (GAL22V10.P17 of pin (index 17) (kind I/O))
              (GAL22V10.P18 of pin (index 18) (kind I/O))
              (GAL22V10.P19 of pin (index 19) (kind I/O))
              (GAL22V10.P20 of pin (index 20) (kind I/O))
              (GAL22V10.P21 of pin (index 21) (kind I/O))
              (GAL22V10.P22 of pin (index 22) (kind I/O))
              (GAL22V10.P23 of pin (index 23) (kind I/O))
              (GAL22V10.P24 of pin (index 24) (kind POWER) (must-be VCC))

              (GAL16V8.P1 of pin (index 1) (kind CLK/INPUT))
              (GAL16V8.P2 of pin (index 2) (kind INPUT))
              (GAL16V8.P3 of pin (index 3) (kind INPUT))
              (GAL16V8.P4 of pin (index 4) (kind INPUT))
              (GAL16V8.P5 of pin (index 5) (kind INPUT))
              (GAL16V8.P6 of pin (index 6) (kind INPUT))
              (GAL16V8.P7 of pin (index 7) (kind INPUT))
              (GAL16V8.P8 of pin (index 8) (kind INPUT))
              (GAL16V8.P9 of pin (index 9) (kind INPUT))
              (GAL16V8.P10 of pin (index 10) (kind POWER) (must-be GND))
              (GAL16V8.P11 of pin (index 11) (kind OE/INPUT))
              (GAL16V8.P12 of pin (index 12) (kind POWER))
              (GAL16V8.P13 of pin (index 13) (kind INPUT))
              (GAL16V8.P14 of pin (index 14) (kind I/O))
              (GAL16V8.P15 of pin (index 15) (kind I/O))
              (GAL16V8.P16 of pin (index 16) (kind I/O))
              (GAL16V8.P17 of pin (index 17) (kind I/O))
              (GAL16V8.P18 of pin (index 18) (kind I/O))
              (GAL16V8.P19 of pin (index 19) (kind I/O))
              (GAL16V8.P20 of pin (index 20) (kind POWER) (must-be VCC))

              (of logic-device
                  (title GAL16V8)
                  (pin-count 20)
                  (olmc-count 8)
                  (pins [GAL16V8.P1]
                    [GAL16V8.P2]
                    [GAL16V8.P3]
                    [GAL16V8.P4]
                    [GAL16V8.P5]
                    [GAL16V8.P6]
                    [GAL16V8.P7]
                    [GAL16V8.P8]
                    [GAL16V8.P9]
                    [GAL16V8.P10]
                    [GAL16V8.P11]
                    [GAL16V8.P12]
                    [GAL16V8.P13]
                    [GAL16V8.P14]
                    [GAL16V8.P15]
                    [GAL16V8.P16]
                    [GAL16V8.P17]
                    [GAL16V8.P18]
                    [GAL16V8.P19]
                    [GAL16V8.P20]))
              (of logic-device
                  (title GAL22V10)
                  (pin-count 24)
                  (olmc-count 10)
                  (pins [GAL22V10.P1]
                    [GAL22V10.P2]
                    [GAL22V10.P3]
                    [GAL22V10.P4]
                    [GAL22V10.P5]
                    [GAL22V10.P6]
                    [GAL22V10.P7]
                    [GAL22V10.P8]
                    [GAL22V10.P9]
                    [GAL22V10.P10]
                    [GAL22V10.P11]
                    [GAL22V10.P12]
                    [GAL22V10.P13]
                    [GAL22V10.P14]
                    [GAL22V10.P15]
                    [GAL22V10.P16]
                    [GAL22V10.P17]
                    [GAL22V10.P18]
                    [GAL22V10.P19]
                    [GAL22V10.P20]
                    [GAL22V10.P21]
                    [GAL22V10.P22]
                    [GAL22V10.P23]
                    [GAL22V10.P24]))
                    )

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
(defrule MAIN::decompose-pld-pinout
         (stage (current inspect-pld))
         (object (is-a pld)
                 (pinout $?pinout)
                 (name ?name))
         =>
         (bind ?pins
               (create$))
         (progn$ (?line $?pinout) 
                 (bind ?pins 
                       ?pins
                       (explode$ ?line)))
         ; at this point, we have a proper set of pins based on the GALASM layout
         (progn$ (?pin $?pins)
                 (assert (pin-information (target ?name)
                                          (title ?pin)
                                          (index ?pin-index)))))

(defrule MAIN::pin-is-connected
         (stage (current inspect-pld))
         ?f <- (pin-information (title ?title)
                                (connected UNKNOWN))
         =>
         (modify ?f
                 (connected (neq ?title
                                 NC))))

(defrule MAIN::is-power-pin
         (stage (current inspect-pld))
         ?f <- (pin-information (title ?title)
                                (power-pin UNKNOWN))
         =>
         (modify ?f 
                 (power-pin (not (neq ?title 
                                      VCC 
                                      GND)))))

(defrule MAIN::is-clock-pin
         (stage (current inspect-pld))
         ?f <- (pin-information (title ?title)
                                (index ?index)
                                (clock-pin UNKNOWN))
         =>
         (modify ?f
                 (clock-pin (and (eq ?title 
                                     CLK)
                                 (= ?index 
                                    1)))))


(defrule MAIN::unknown-target-device
         (stage (current inspect-pld))
         (pin-information (title VCC)
                          (power-pin TRUE)
                          (target ?pld)
                          (index ?index))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?target))
         (not (object (is-a logic-device)
                      (title ?target)
                      (pin-count ?index)))
         =>
         (printout stdout 
                   "Warning: given type: " ?target " is not a known chip type " crlf))
(defrule MAIN::pinout-count-doesnt-match-chip-pins
         (stage (current inspect-pld))
         (pin-information (title VCC)
                          (power-pin TRUE)
                          (target ?pld)
                          (index ?index))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?target))
         (object (is-a logic-device)
                 (title ?target)
                 (pin-count ?index2))
         (test (neq ?index
                    ?index2))

         =>
         (printout stdout 
                   "Warning: pinout describes " ?index " pins but the chip " ?target " requires " ?index2 " pins to be described" crlf)
         (halt))

(defrule MAIN::associate-pin-with-logic-device
         (declare (salience 10000))
         (stage (current inspect-pld))
         ?f <- (object (is-a pin)
                       (parent FALSE)
                       (name ?pin))
         (object (is-a logic-device)
                 (pins $? ?pin $?)
                 (name ?device))
         =>
         (modify-instance ?f 
                          (parent ?device)))
(defrule MAIN::pin-names-do-not-match
         (stage (current inspect-pld))
         (object (is-a pin)
                 (parent ?device&~FALSE)
                 (must-be ?title&~FALSE)
                 (index ?index))
         (pin-information (title ?bad-title&~?title)
                          (index ?index)
                          (target ?pld))
         (object (is-a pld)
                 (name ?pld)
                 (title ?pld-title)
                 (chip ?chip))
         (object (is-a logic-device)
                 (title ?chip)
                 (name ?device))
         =>
         (printout stderr
                   "ERROR: pin " ?index " of " ?pld-title " must be " ?title " but is " ?bad-title crlf)
         (halt))


(defrule MAIN::clk-keyword-used-in-bad-location
         (stage (current inspect-pld))
         (pin-information (title CLK)
                          (index ?index)
                          (target ?pld))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?chip)
                 (title ?pld-title))
         (object (is-a logic-device)
                 (title ?chip)
                 (name ?device))
         (object (is-a pin)
                 (parent ?device)
                 (index ?index)
                 (kind ~CLK/INPUT))
         =>
         (printout stderr
                   "ERROR: pin " ?index " of " ?pld-title " uses reserved keyword CLK when it is not allowed" crlf)
         (halt))

(defrule MAIN::oe-keyword-used-in-bad-location
         (stage (current inspect-pld))
         (pin-information (title OE)
                          (index ?index)
                          (target ?pld))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?chip)
                 (title ?pld-title))
         (object (is-a logic-device)
                 (title ?chip)
                 (name ?device))
         (object (is-a pin)
                 (parent ?device)
                 (index ?index)
                 (kind ~OE/INPUT))
         =>
         (printout stderr
                   "ERROR: pin " ?index " of " ?pld-title " uses reserved keyword OE when it is not allowed" crlf)
         (halt))


(defrule MAIN::VCC-keyword-used-in-non-power-location
         (stage (current inspect-pld))
         (pin-information (title VCC)
                          (index ?index)
                          (target ?pld))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?chip)
                 (title ?pld-title))
         (object (is-a logic-device)
                 (title ?chip)
                 (name ?device))
         (object (is-a pin)
                 (parent ?device)
                 (index ?index)
                 (kind ~POWER))
         =>
         (printout stderr
                   "ERROR: pin " ?index " of " ?pld-title " uses reserved keyword VCC when it is not allowed at this position" crlf)
         (halt))

(defrule MAIN::VCC-keyword-used-in-wrong-power-location
         (stage (current inspect-pld))
         (pin-information (title VCC)
                          (index ?index)
                          (target ?pld))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?chip)
                 (title ?pld-title))
         (object (is-a logic-device)
                 (title ?chip)
                 (name ?device))
         (object (is-a pin)
                 (parent ?device)
                 (index ?index)
                 (must-be ?expected&~VCC)
                 (kind POWER))
         =>
         (printout stderr
                   "ERROR: pin " ?index " of " ?pld-title " uses reserved keyword VCC when it should be " ?expected crlf)
         (halt))

(defrule MAIN::GND-keyword-used-in-non-power-location
         (stage (current inspect-pld))
         (pin-information (title GND)
                          (index ?index)
                          (target ?pld))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?chip)
                 (title ?pld-title))
         (object (is-a logic-device)
                 (title ?chip)
                 (name ?device))
         (object (is-a pin)
                 (parent ?device)
                 (index ?index)
                 (kind ~POWER))
         =>
         (printout stderr
                   "ERROR: pin " ?index " of " ?pld-title " uses reserved keyword GND when it is not allowed at this position" crlf)
         (halt))

(defrule MAIN::GND-keyword-used-in-wrong-power-location
         (stage (current inspect-pld))
         (pin-information (title GND)
                          (index ?index)
                          (target ?pld))
         (object (is-a pld)
                 (name ?pld)
                 (chip ?chip)
                 (title ?pld-title))
         (object (is-a logic-device)
                 (title ?chip)
                 (name ?device))
         (object (is-a pin)
                 (parent ?device)
                 (index ?index)
                 (must-be ?expected&~GND)
                 (kind POWER))
         =>
         (printout stderr
                   "ERROR: pin " ?index " of " ?pld-title " uses reserved keyword GND when it should be " ?expected crlf)
         (halt))
