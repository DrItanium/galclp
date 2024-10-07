(deffunction MAIN::*eq-match
			 (?dest ?src0 ?src1)
			 (*assign (*not ?dest)
					  (*eq ?src0
						   ?src1)))
(definstances plds
              (of pld
                  (chip GAL22V10)
                  (title CH351QDecoderCircuit)
                  (pinout "NC SEL0 SEL1 SEL2 SEL3 SEL4 A19   A20    A21 A22 A23 GND"
				   		  "NC MA19 MA20 MA21 MA22 MA23 MADDR MADDR2 NC  NC  NC VCC")
                  (description "Decoder chip to allow decoding and also reconfiguration")
                  (expressions (*eq-match MA19 SEL0 A19)
							   (*eq-match MA20 SEL1 A20)
							   (*eq-match MA21 SEL2 A21)
							   (*eq-match MA22 SEL3 A22)
							   (*eq-match MA23 SEL4 A23)
							   (*assign (*not MADDR2)
										(*and MA20
											  MA21
											  MA22
											  MA23))
							   (*assign (*not MADDR)
										(*and (*eq SEL1 
												   A20)
											  (*eq SEL2 
												   A21)
											  (*eq SEL3 
												   A22)
											  (*eq SEL4 
												   A23)))
				  )
				   
                  (output-path RAMMatchCard.pld))
			  )
