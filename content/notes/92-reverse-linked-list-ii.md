---
title: "92. Reverse Linked List II"
date: 2019-12-02T14:06:47+08:00
draft: true
tags: ['LeetCode']
---

反转链表指定部分，稍微思考一下就有了思路，几个临界点需要注意一下。

一开始使用一个大的循环，在循环中每次判断临界值。
这样写出来的代码不高效，也不优雅。

之后看了 LeetCode 讨论区，发现了更好的解决办法。

Python 中对变量平行赋值时，有个小知识点：

> There's a special rule that if any of the left-hand-side variables "overlap", the assignment goes left-to-right.

具体可看 [这个](https://stackoverflow.com/questions/52143328/how-python-assign-multiple-variables-at-one-line-works)。

所以在对几个点进行变换时，必须用一个临时变量把某个变量的值保存起来再赋值。

使用 Common Lisp 实现代码时，把列表当链表，
现有的操作符很容易就写出简洁的代码来：

```lisp
(defun reverse-between (head m n)
  (push 0 head)
  (let ((end-node (nthcdr n head)))
    (setf (cdr (nthcdr (1- m) head))
          (reverse (subseq head m (1+ n))))
    (setf (cdr (last head))
          (cdr end-node))
    (cdr head)))
```

同样的思路，只使用 `car` 和 `cdr` 实现起来也不难：

```lisp
(defun reverse-between (head m n)
  (let* ((dummy (push 0 head))
         (prev dummy)
         (curr))
    (loop repeat (1- m)
          do (setf prev (cdr prev)))
    (setf curr (cdr prev))
    (loop repeat (- n m -1)
          with p = prev
          do (psetf curr (cdr curr)
                    (cdr curr) p
                    p curr)
          finally (psetf (cdr prev) p
                         (cddr prev) curr))
    (cdr dummy)))
```

这里的 `psetf` 直接就是平行赋值了。
