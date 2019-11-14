---
date: "2019-11-13"
keywords: ["query", "trees"]
title: "When You Wish Upon a TAGBODY"
id: "urn:uuid:29175a02-069e-11ea-b588-843a4b7c6000"
abstract: |
  Something about TAGBODY.
---

Blah have some code.

## Transforming

A tree can be represented as a table of paths. For example, the following
structures have different affordances but are informationally equivalent:

~~~{.lisp}
; Tree
(list
  (eq people 1)
  `(foo ,(+ 1 2)))
~~~
