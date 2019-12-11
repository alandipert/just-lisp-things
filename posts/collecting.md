---
date: "2019-12-07"
keywords: ["LOOP", "collect", "lists"]
title: "Collecting without LOOP"
id: "urn:uuid:29175a02-069e-11ea-b588-843a4b7c6000"
abstract: |
  All about LOOP's collect keyword, and how to collect lists on our own,
  without LOOP.
---

[`LOOP`][loop] is a [famously
divisive][weinreb-loop] iteration
[DSL][dsl]. Personally, I love it. Iteration is central to
programming, I don't have any scruples about imperative code,
and I already know a million weird languages, so what's one
more. Plus, it's a [zero cost
abstraction](https://boats.gitlab.io/blog/post/zero-cost-abstractions/)
and an awesome example of what a Lisp macro is capable of.

But I digress. In this post, I want to talk specifically about the
`collect` keyword of `LOOP`: how it works, and how we can make our own
version of it that's useful in situations `LOOP` isn't desirable or
otherwise applicable.

## Collecting 101

Consider the following example that evaluates to the list `(2 4 6 8 10)`:

~~~{.lisp}
(loop for i from 1 to 10
      when (evenp i)
      collect i)
~~~

It counts from 1 to 10, and when an even number is encountered, that
value is appended to a list and that list is returned. The `LOOP`
syntax makes it all look pretty simple, but just beneath the syntax
some _fanciness_ is happening.

This fanciness is necessary because it's not efficient to append to a
linked list made of cons cells. You have to find the last cell in
order to mutate it, and finding the last cell means traversing all the
cells that precede it. The longer the list is, the longer it will
take.

The way around this problem &mdash; the _fanciness_ &mdash; is to
maintain both the head *and* the tail of the list you're appending
to. When you want to append to the list, you [`CONS`][cons] up a new
tail. When you're done appending, you return the head.

The following example also returns the list `(2 4 6 8 10)` but uses a
simplified form of `LOOP` and maintains the result list manually. It's
kind of hairy, but don't worry, we'll clean it up shortly:

~~~{.lisp .numberLines}
(let ((head nil)
      (tail nil)
      (i 1))
  (loop
   (when (> i 10) (return head))
   (when (evenp i)
     (if (null tail)
         (setq tail (cons i nil)
               head tail)
       (let ((new-tail (cons i nil)))
         (setf (cdr tail) new-tail
               tail new-tail))))
   (incf i)))
~~~

Here's a line-by-line breakdown of what's going on:

* **1-2**: `HEAD` and `TAIL` both start as `NIL`.
* **3**: initialize `I` to 1
* **4**: Enter an infinite loop...
* **5**: If `I` is greater than 10, [`RETURN`][return] from the nearest enclosing [`BLOCK`][block] named `NIL`, which in this case is the one established by `LOOP`.
* **6**: If `I` is even...
* **7-9**: This code handles the case of the first even number we want to keep. A new cons cell is created with a car of the number, and `HEAD` and `TAIL` are both set to it.
* **10-12**: This handles the case of all subsequent even numbers. A new tail is created, and the cdr of the existing tail `TAIL` is set to point to it. Then, `TAIL` is set to `NEW-TAIL`.
* **13**: `I` is incremented, and the `LOOP` continues.

## Encapsulated collecting

It wouldn't be fun to have to write that head/tail management code all
the time, which is one reason that `LOOP` and its `collect` keyword
are so useful. But collecting doesn't have to be so wordy, even
without `LOOP`.

One way to make collecting more accessible is to wrap it in a
function-based interface. Consider the following function,
`COLLECTOR`:

~~~{.lisp}
(defun collector ()
  "Maintains the head of a list and returns a function that appends an
item to the list. When the returned function is called with no
arguments, returns the head."
  (let ((tail nil)
        (head nil))
    (lambda (&optional (item nil argument?))
      (cond
        ((not argument?) head)
        ((null tail) (setq tail (cons item nil)
                           head tail))
        (t (let ((new-tail (cons item nil)))
             (setf (cdr tail) new-tail
                   tail new-tail)))))))
~~~

`COLLECTOR` encapsulates the collecting logic in the form of a
function that returns a function. The returned function can be called
two ways:

1. With an argument, which causes the argument to be collected
1. Without an argument, which returns the head of the list containing collected items

Here's some example usage:

~~~{.lisp}
(setq collect (collector))
(funcall collect 1)
(funcall collect 2)
(funcall collect 3)
(funcall collect) ;=> (1 2 3)
~~~

> Note that [`FUNCALL`][funcall] is necessary here because we have stored the function returned by `COLLECTOR` into a variable and Common Lisp is a [Lisp-2][lisp-2].

Our code for collecting even numbers gets a lot clearer with `COLLECTOR`:

~~~{.lisp}
(let ((collect (collector))
      (i 1))
  (loop
   (cond
    ((> i 10) (return (funcall collect)))
    ((evenp i) (funcall collect i)))
   (incf i)))
~~~

We can arguably simplify even further by using the other iteration
constructs [`DO`][do] or [`DOTIMES`][dotimes]:

~~~{.lisp}
;; with DO
(do ((collect (collector))
     (i 1 (1+ i)))
    ((> i 10) (funcall collect))
  (when (evenp i)
    (funcall collect i)))

;; with DOTIMES
(let ((collect (collector)))
  (dotimes (i 10 (funcall collect))
    (let ((i (1+ i)))
      (when (evenp i)
        (funcall collect i)))))
~~~

## Other observations and considerations

* I was motivated to write this post after writing a [depth-first topological sort](https://gist.github.com/alandipert/af7093ef1719ddc736ee5deb37748b04). The algorithm naturally yielded results in reverse of the order I wanted, so I replaced my use of a list as a stack with `COLLECTOR`. I could also have reversed the stack before returning it, but I wanted to avoid the performance penalty.
* I briefly noted why we needed `funcall` but that's a huge topic. If
you're coming from another Lisp or otherwise up for a deep dive, check out
[Technical Issues of Separation in Function Cells and Value
Cells](https://www.dreamsongs.com/Separation.html).
* I used [SETF][setf] to update the car of a cons cell without explaining it, but it's CL's generalized assignment operator and explained very well in [Practical Common Lisp][pcl-variables]. Alternatively, [`RPLACA` and `RPLACD`][rplaca-rplacd] could have been used to mutate conses.
* It's amazing how much mileage you can get out of the humble mutable cons cell!

[weinreb-loop]: http://www.paulgraham.com/loop.html
[DSL]: https://en.wikipedia.org/wiki/Domain-specific_language
[loop]: http://www.lispworks.com/documentation/HyperSpec/Body/m_loop.htm
[push]: http://www.lispworks.com/documentation/HyperSpec/Body/m_push.htm
[cons]: http://www.gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html
[return]: http://www.lispworks.com/documentation/HyperSpec/Body/m_return.htm
[block]: http://www.lispworks.com/documentation/HyperSpec/Body/s_block.htm
[do]: http://www.lispworks.com/documentation/HyperSpec/Body/m_do_do.htm
[dotimes]: http://clhs.lisp.se/Body/m_dotime.htm
[lisp-2]: https://en.wikipedia.org/wiki/Common_Lisp#The_function_namespace
[queue]: https://people.eecs.berkeley.edu/~russell/code/utilities/queue.lisp
[pcl-variables]: http://www.gigamonkeys.com/book/variables.html
[rplaca-rplacd]: http://clhs.lisp.se/Body/f_rplaca.htm
[funcall]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm
[setf]: http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm