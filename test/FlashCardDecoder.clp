(definstances plds
              (of pld
                  (chip GAL16V8)
                  (title FlashCardDecoder)
                  (pinout "SEL0 SEL1 SEL2 SEL3 A19 A20 A21 A22 A23 GND"
                          "M    NC   NC   NC   NC  EN0 EN1 EN2 EN3 VCC"
                          "")

                  (description "Flash Decoder card that selects one of four 512k Flash chips"
                               ""
                               "Have to describe the terms fully since using the intermediate product terms"
                               "fails horribly")
                  (expressions 
                    (*assign (*not EN0)
                             (*and (*not M)
                                   (*eq3 A21  A22  A23
                                         SEL1 SEL2 SEL3)
                                   (*not A19)
                                   (*not A20)))
                    (*assign (*not EN1)
                             (*and (*not M)
                                   (*eq3 A21  A22  A23
                                         SEL1 SEL2 SEL3)
                                   A19
                                   (*not A20)))
                    (*assign (*not EN2)
                             (*and (*not M)
                                   (*eq3 A21  A22  A23
                                         SEL1 SEL2 SEL3)
                                   (*not A19)
                                   A20))
                    (*assign (*not EN3)
                             (*and (*not M)
                                   (*eq3 A21 A22 A23
                                         SEL1 SEL2 SEL3)
                                   A19
                                   A20)))
                  (output-path result.pld))
              )
