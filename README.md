A simple CLIPS expert system to make writing GAL equations with more complex features easier

So I love galette and galASM but it is very simple and doesn't support features like xor, muxes, and
parens. So this library converts your expressions into the equations those tools understand. 

For example, the equation for an xor is:

C = /A * B + A * /B

But I would just like to do this:

C = A ^ B


This is a CLIPS expert system which allows you to do very complex expressions
and it will make sure that it is properly expressed that the GAL assembler will
understand. It's final product is a pld script which can be used with
galette/GALasm.

An example of how it works can be found in test/test.pld


