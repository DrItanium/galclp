A simple CLIPS expert system to make writing GAL equations with more complex features easier

So I love galette and galASM but it is very simple and doesn't support features like xor, muxes, and
parens. So this library converts your expressions into the equations those tools understand. 

For example, the equation for an xor is:

C = /A * B + A * /B

But I would just like to do this:

C = A ^ B

This is also really annoying as well:

/C = A * B

Instead I would like to do this following: 

C = /(A * B)

