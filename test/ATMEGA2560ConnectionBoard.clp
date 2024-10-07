
(definstances plds
              (of pld
                  (chip GAL22V10)
                  (title CH351QDecoderCircuit)
                  (pinout "CLK ADS BLAST DEN WR LOCK HLDA FAIL PE2 PE3 PG4 GND"
                          "NC PE7 PE6 PE5 PE4 PG5 HOLD RESET STG0 STG1 READY VCC"
                  )
                  (description "Allows simple reconfiguration of pins and functionality")
                  (expressions (*assign (*not READY)
                                        (*xor STG0 
                                              STG1))
                               (*assign STG0.R
                                        PG4)
                               (*assign STG1.R
                                        STG0)
                               (*assign RESET
                                        PE2)
                               (*assign HOLD
                                        PE3)
                               (*assign PE4
                                        ADS)
                               (*assign PE5
                                        HLDA)
                               (*assign PE6
                                        LOCK) ; this one will probably reused for something else eventually
                               (*assign PE7
                                        (*xor STG0 
                                              STG1))
                  )
                  (output-path ConnectionFunctionalityBoard.pld))
              )
