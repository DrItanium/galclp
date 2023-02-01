(defglobal MAIN
           ; provide the ability to define at build time if A15 is high or low
           ?*shift-to-upper-bank* = TRUE)
; This is another example of a real device I use
; It provides matching and decoding support for interfacing with a WCH351Q in IOEXP mode
(definstances plds
              (of pld
                  (chip GAL16V8)
                  (title CH351QDecoderCircuit)
                  (pinout "RD WR M  A11 A12 A13 A14 A15 NC GND"
                          "NC NC NC NC  NC  NC  NC  XEN EN VCC"
                          "")

                  (description "Simple decoder circuit for the CH351 in IOEXP mode")
                  (expressions 
                    (*assign (*not XEN)
                             (*xor RD WR))
                    (*assign (*not EN)
                             (*and (*not M)
                                   (*xor RD WR)
                                   (*and (*not A11)
                                         (*not A12)
                                         A13
                                         (*not A14)
                                         (if ?*shift-to-upper-bank* then
                                           A15
                                           else
                                           (*not A15))))))
                  (output-path ch351decoder.pld))
              )
