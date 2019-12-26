---
date: "2019-12-26"
keywords: ["LOOP", "collect", "lists", "macros"]
title: "Collecting without LOOP: Macro Edition"
id: "urn:uuid:a2d4ab9a-1c99-11ea-a6e0-843a4b7c6000"
abstract: |
  Implementing COLLECTING with a macro interface instead of as a function.
---

In a [previous post](collecting.html), I professed my fondness for the
[LOOP][loop] macro and introduced something like its `collect` keyword
delivered as a function called `COLLECTOR`.

`COLLECTOR` was cool. Maybe the coolest thing about it was that the
function returned by `COLLECTOR` could be passed as an argument to
other functions. But that's a pretty obscure affordance that I think
is overshadowed by two unfortunate characteristics:

1. Syntax: Usage syntax is clouded by [`FUNCALL`][funcall]
1. Performance: Collecting incurs the overhead of a function call

The syntax problem is subjective. Maybe you don't mind seeing
`FUNCALL` here and there. The performance problem is also subjective;
your Lisp implementation might inline the collecting function, or you
just might not care.

But my treatment of the problem wouldn't be complete without
suggesting an approach that addressed the potential syntax and
performance problems. That approach, of course, is the one involving
[macros][pcl-macros].

## A Macro-based API

`COLLECTOR` had a functional API, in the sense that it returned a
function. As a return value, the returned function could not be called
like a conventional Lisp operator; `FUNCALL` was necessary.

With macros, however, it's possible to support operator syntax. Here's
what I have in mind:

~~~{.lisp}
(with-collect collect
  (dotimes (i 10)
    (let ((i (1+ i)))
      (when (evenp i)
        (collect i)))))
~~~

* `WITH-COLLECT` is the name of the macro. Its first argument &mdash;
  `COLLECT` &mdash; is a symbol, the name to use for the collecting
  operator within the context of the body.
* `COLLECT` is called like a function.
* Instead of calling `COLLECT` with no arguments to return the list
  head, `WITH-COLLECT` evaluates to the head of the collected list.

## Implementation

~~~{.lisp}
(defmacro with-collect (collect-name &body body)
  (let ((head (gensym "head"))
        (tail (gensym "tail")))
    `(let ((,head nil)
           (,tail nil))
       (macrolet ((,collect-name (item)
                     `(cond
                       ((null ,',tail)
                        (setq ,',tail (cons ,item nil)
                              ,',head ,',tail))
                        (t (let ((new-tail (cons ,item nil)))
                             (setf (cdr ,',tail) new-tail
                                   ,',tail new-tail))))))
         ,@body
         ,head))))
~~~

Like `COLLECTOR`, code for maintaining the tail of the list is in
there. Unlike `COLLECTOR`, that code now lives in a macro instead of a
function.

In particular, the operative code is inside the body of a local macro
defined by [`MACROLET`][macrolet]. Like a normal macro defined via
[`DEFMACRO`][defmacro], `MACROLET` is a way to replace occurrences of
operator calls with arbitrary code. But unlike `DEFMACRO`, definitions
created by `MACROLET` are _local_ and not _global_: they are only
usable within the specified lexical extent.

The pickle with `WITH-COLLECT` is we needed to parameterize the name
of the macro defined by `MACROLET` &mdash; we wanted to give the user
the ability to pick their own name for the collecting operator.

We also need to parameterize the body of the `MACROLET`; we want the
user to supply the code within which the `MACROLET` will apply.

The way to parameterize any syntax, even that of `MACROLET`, is with a
macro. So, we wrap everything in a `DEFMACRO` that accepts the
`COLLECT-NAME` and `BODY` arguments.

One thing that might stick out to you is the peculiar `,',`
syntax. What's happening is, the presence of two backticks (one macro
is producing another) makes two levels of unquoting necessary. Since
the innermost unquote yields a symbol, it must be wrapped in a quote
to prevent its evaluation as a value.

## Trade-offs

I think it's fair to say that we simplified the API, because the
entire expression now evaluates to the collected list. The collection
operation now also looks like a function call and is not obscured by
`FUNCALL`.

Code size is larger, because every occurrence of the user-named
collecting operator now expands to the collection operation code
instead of being a function call.

I like it though. All things considered, I think I generally prefer
something like `WITH-COLLECT` to `COLLECTING`.

## Further reading

There's a [COLLECTING entry on CLiki][collecting-clwiki] with a bunch
of pointers to other macro implementations.

The _Code-Walking with Macrolet_ section of [Chapter 5 of Let Over
Lambda][lol] is a nice treatment of `MACROLET`. As far as I can tell,
the most common use of `MACROLET` is inside `DEFMACRO`s in order to
support a global macro's operator semantics.

[loop]: http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
[funcall]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm
[pcl-macros]: http://www.gigamonkeys.com/book/macros-defining-your-own.html
[inline]: http://www.lispworks.com/documentation/HyperSpec/Body/d_inline.htm
[macrolet]: http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm
[defmacro]: http://www.lispworks.com/documentation/HyperSpec/Body/m_defmac.htm#defmacro
[collecting-clwiki]: https://www.cliki.net/COLLECTING
[lol]: https://letoverlambda.com/textmode.cl/guest/chap5.html
