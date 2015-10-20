---
title: My Allergy To Syntax In Programming Languages
origdate: 14 Nov 2012 (or earlier)
published: 2012-11-14
tags: programming, psychology
---


Since I got back into Lisp again (I had a wonderful flirtation with 
it in University, but it was presented as a toy language, so I 
didn't stick with it), I've been telling people how incredibly 
freeing it is, and how I seem to be allergic to syntax. I've had a 
hard time explaining this to other people, and finally hit on a way 
to do it. This essay is the result.

Before I get into this, some general comments on Lisp:

If you're saying "Eww, how can you count all those parens?", You're Doing It Wrong(external link). See scheme-style(external link) for how it actually works. Note that this objection is different from "I don't like all those parens", which is a perfectly legitimate personal choice, but see the rest of this essay.
If you're saying "But Lisp is a toy language", you're simply incorrect(external link).

Unless you want to make one of those two points, please feel free to 
email me about this essay.

Moving on. I use an example out of Perl. It doesn't actually 
matter what the example means, I don't think, except that it's 
setting a sub-set of a complex data structure hanging off the object 
"self".

Let's say we have:

$self->{'CONF'}->{'outerdatawindow'} = $outerdatawindow;

I have no idea how this would work in 
CLOS(external link) off the top of my 
head, but let's say it looks like this in CL:

(setf (outerdatawindow (CONF self)) outerdatawindow)

In both cases, I need to know:
