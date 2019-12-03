---
title: "91. Decode Ways"
date: 2019-12-01T23:36:12+08:00
draft: false
tags: ["LeetCode"]
---

问题并不复杂，要求对指定信息按指定方式解码。

假设输入是 `s`，求解函数是 `foo`，
则 `foo(s) = foo(s[0]) * foo(char[1:]) +
foo(s[:2]) * foo(s[2:])`。

递归加动态规划，很容易就写出有 Bug 的代码来。
一些特殊情况需要考虑完全，
不然就增加了很多试错的时间。

一开始写了个递归的版本，修修改改，最后通过了。
但看了 [StefanPochmann][one_line] 的版本，
发现并不需要这么麻烦。

[one_line]: https://leetcode.com/problems/decode-ways/discuss/30379/1-liner-O(1)-space

研究了一下他的思路，自己用 Common Lisp 实现了一下：

```lisp
(defun num-decodings (s)
  (let ((prev "")
        (pw 0)
        (cw (if (string> s "0")
                1
                0)))
    (loop for curr across s
       do (psetf pw cw
                 prev curr
                 cw (+ (if (char> curr #\0)
                           cw
                           0)
                       (if (< 9 (parse-integer (format nil "~a~a" prev curr)) 27)
                           pw
                           0)))
       finally (return cw))))
```

即便使用 `psetf` 平行赋值，代码行数也比 Python 的实现要多。
当然，这也跟我对 Common Lisp 并不熟悉有关，
以后再看看。

