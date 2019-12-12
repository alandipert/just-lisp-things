---
date: "2019-12-12"
keywords: ["LOOP", "collect", "lists", "macros"]
title: "Collecting without LOOP: Macro Edition"
id: "urn:uuid:a2d4ab9a-1c99-11ea-a6e0-843a4b7c6000"
abstract: |
  Implementing COLLECTING with a macro interface instead of a function.
---

In a [previous post](collecting.html), I professed my fondness for the
[LOOP][loop] macro and introduced something like its `collect` keyword
delivered as a function called `COLLECTOR`.

`COLLECTOR` was cool, but I forgot to mention one big disadvantage of
`COLLECTOR` compared to `collect`: function call overhead.

In this post, we'll survey a few tools Common Lisp provides that can
be used to eliminate function call overhead, and use them to concoct a
new, [macro][macro]-based collecting contraption: `WITH-COLLECT`.

## Wait, function calls are slow?

Well, no, but there is some overhead associated with a function
call. Usually, at a minimum:

1. The function reference must be resolved
1. A stack frame must be allocated
1. Arguments must be copied to the stack

One way that a compiler can optimize away the overhead of a function
call is by _inlining_ the function. That's the process of replacing
the place where a function is called, the [call site][call-site], with
the implementation of the function.

Of course, functions are such a powerful tool that in most cases,
their overhead is well worth it. We're interested in inlining here
only because as library authors we want the collecting operation to be
as fast as possible, and because the use-case we have in mind &mdash;
collecting values in a single lexical block of code &mdash; doesn't
strictly require an actual function to exist.

Honestly, I'm not really used to thinking too much about function call
overhead.[^r] Much of my professional programming experience has been
with the JVM and JavaScript, two platforms with optimizing [tracing
JIT][jit] compilers that do all kinds of optimization for you,
including automatically inlining. As programs on these platforms run,
they are continually analyzed and recompiled in order to run faster.


## Inlining and CL

Common Lisp was specified before tracing JITs were a thing, but it
does have pervasive facilities for all kinds of _static_ optimization
strategies, including inlining. Mechanisms supporting inlining
directly or indirectly that I'm aware of include:

* Macros ([`DEFMACRO`][defmacro], [`MACROLET`][macrolet])
* Compiler macros ([`DEFINE-COMPILER-MACRO`][cmacro])
* The [`INLINE`][inline] [declaration][declare]

Here's an example showing the functional way, `COLLECTOR`, compared to
what I have in mind with `WITH-COLLECT`. Both expressions evaluate to
`(2 4 6 8 10)`:

~~~{.lisp}
;; with COLLECTOR from previous post
(let ((collect (collector)))
  (dotimes (i 10 (funcall collect))
    (let ((i (+1 i)))
      (when (evenp i)
        (funcall collect i)))))
      
;; with WITH-COLLECT, which we haven't implemented yet
(with-collect collect
  (dotimes (i 10)
    (let ((i (1+ i)))
      (when (evenp i)
        (collect i)))))
~~~

In this post, we'll address the function call overhead question with a
new, [macro][macro]-based collecting contraption, `WITH-COLLECT`. But
before we do, let's establish some context, and get into background on
stuff like tracing JITs and Common Lisp's tools for inlining
statically.

## Tracing JITs

Honestly, I'm not really used to thinking too much about function call
overhead.[^r] Much of my professional programming experience has been
with the JVM and JavaScript, two platforms with optimizing [tracing
JIT][jit] compilers that do all kinds of optimization for you,
including optimizing function calls. As programs on these platforms
run, they are continually analyzed and recompiled in order to run
faster.

One key JIT optimization, _inlining_, involves the replacement of
frequent function calls with the functions' implementations. This
saves allocating a stack frame and possibly other work, such as
copying arguments, and can make a big difference in a "hot loop".

If the called function is updated by the user, the inlined version of
the [call site][call-site] becomes invalid, and is replaced with a new
version that again calls the function.

Tracing JITs are really cool.[^jit] They make a lot of code run a lot
faster automatically, which probably saves us all a lot of
time. There's at least one downside, though: you can't count on what
the JIT's going to do with your code at runtime. You have to do your
best to make sure your program will be fast enough without it, and
then hope to be pleasantly surprised when the JIT kicks in.

## Inlining and CL

Common Lisp was specified before tracing JITs were a thing, but it
does have pervasive facilities for all kinds of _static_ optimization
strategies, including inlining. Mechanisms supporting inlining
directly or indirectly that I'm aware of include:

* Macros ([`DEFMACRO`][defmacro], [`MACROLET`][macrolet])
* Compiler macros ([`DEFINE-COMPILER-MACRO`][cmacro])
* The [`INLINE`][inline] [declaration][declare]

[loop]: http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
[jit]: https://en.wikipedia.org/wiki/Tracing_just-in-time_compilation
[call-site]: https://en.wikipedia.org/wiki/Call_site
[macro]: http://www.gigamonkeys.com/book/macros-defining-your-own.html
[defmacro]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm#defmacro
[macrolet]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm
[cmacro]: http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm
[inline]: http://www.lispworks.com/documentation/HyperSpec/Body/d_inline.htm
[declare]: http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm#declare

[^r]: Except when I use [R](http://adv-r.had.co.nz/Performance.html#language-performance)
[^jit]: One of the first papers about JITs (and one of the few I've read...) is [this one](https://people.cs.umass.edu/~emery/classes/cmpsci691s-fall2004/papers/bala00dynamo.pdf) about "Dynamo", a JIT developed at HP Labs around the year 2000. On the surface, Dynamo was a [PA-8000 CPU](https://en.wikipedia.org/wiki/PA-8000) emulator, but actually it was an optimizing JIT compiler. The craziest thing I remember from the paper is them mentioning that one of their test programs ran faster _on_ Dynamo, _on_ the PA-8000 than it did _directly_ on the PA-8000. 
