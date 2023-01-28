This is a CLIPS expert system which allows you to do very complex expressions
and it will make sure that it is properly expressed that the GAL assembler will
understand. It's final product is a pld script which can be used with
galette/GALasm.

An example of how it works can be found in test/SRAMCardDecoder.clp. Next to it, you will
see the original horrible implementation inside of SRAMCardDecoder.pld

GALasm and galette require that you implement everything in terms of and/or/not operators without access to parens or automatic distribution of operators. 
This can be very painful when the expressions get sufficiently complex.
