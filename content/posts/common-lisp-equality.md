---
title: "Common Lisp 中的相等性判断"
date: 2020-06-03T00:18:08+08:00
draft: false
tags: ['Common Lisp']
---

Common Lisp 判断对象是否相等的函数比较多，这里记录一下目前遇到的用于判断相等的函数。

## 相等性判断函数

### `=`

函数 [`=`][f_eq_sle] 用于判断数值（numbers）是否相等（不判断类型），并且只能用于判断数值类型的元素。


语法：

```lisp
;;; = &rest numbers+ => generalized-boolean

(= 1 1.0 #c(1 0))  ; => T
```

### `CHAR=` / `STRING=`

[`char=`][f_chareq] 用于判断多个字符是否相等，区分大小写。此外，不同实现中如果定义两个字符不同，`char=` 返回 `NIL`。

> If two characters differ in any [implementation-defined][1] [attributes][2], then they are not [char=][3].

[1]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_i.htm#implementation-defined
[2]: http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#attribute
[3]: http://www.lispworks.com/documentation/HyperSpec/Body/f_chareq.htm#charEQ

语法：

```lisp
;;; char= &rest characters+ => generalized-boolean

(char= #\d #\d)  ; => T
```

[`string=`][f_stgeq_] 用于判断多个字符串是否相等，区分大小写。

语法：

```lisp
;;; string= string1 string2 &key start1 end1 start2 end2 => generalized-boolean

(string= "foo" "foo")  ; => T
```

可以比较两个字符串中指定的子序列。

### `CHAR-EQUAL` / `STRING-EQUAL`

[`char-equal`][f_chareq] 用于判断字符是否相等，且忽略大小写。语法结构与 `char=` 相同。同样的，不同实现结果可能不同。

> ... might have an implementation-defined behavior for non-simple characters.

```lisp
(char-equal #\A #\a)  ; => T
```

[`string-equal`][f_stgeq_] 与 `string=` 相似，但忽略了大小写。

```lisp
(string-equal "foo" "Foo")  ; => T
```

### `EQ`

[`eq`][f_eq] 判断两个对象是否相同。不同的实现中，数值和字符可能相同，也可能不同。两个看起来一样的列表通常也不同。

> Returns true if its arguments are the same, identical object; otherwise, returns false.

语法：

```lisp
;;; eq x y => generalized-boolean

(eq 'a 'a)  ; => T
```

### `EQL`

当两个对象 `x` 和 `y` 满足以下一个条件时：

1. `x` 和 `y` 相同（`EQ`）。
2. 两者都是数值，且值与类型相同。
3. 两者是字符且表示相等的字符。

[`eql`][f_eql] 判定 `x` 和 `y` 相等。对于浮点类型，`eql` 不确保值相等的浮点数一定相等，且在区分正零和负零的实现中，`(eql -0.0 0.0)` 返回 `NIL`。

### `EQUAL`

[`equal`][f_equal] 判断两个对象是否是同构的（structurally similar, or isomorphic）。

1. 对于符号，相当于使用 `eq`。
2. 对于数值和字符，相当于使用 `eql`。
3. 对于 `cons` / `bit-vector` / `string`，下降为元素间使用 `equal`。
4. 对于目录名（pathname），目录各自部分相同时才相同。
   > `pathnames` that are equal should be functionally equivalent.
5. 其他情况，使用 `eq`。

### `EQUALP`

[`equalp`][f_equalp] 判断相等的条件比 `equal` 要更宽松：

1. 对于字符，使用 `char-equal`。
2. 对于数值，使用 `=`。
3. 对于 `cons` / `bit-vector` / `string` / `structure` / `hash-table`，下降为对包含的元素使用 `equalp`
4. 其他情况，使用 `eq`

### `TREE-EQUAL`

[`tree-equal`][f_tree_e] 用于判断 `cons` 组成的树是否相等，`test` 参数指定判断谓词。

语法：

```lisp
;;; tree-equal tree-1 tree-2 &key test test-not => generalized-boolean

(let ((tree1 '(1 (1 2)))
      (tree2 '(1 (1 2))))
  (tree-equal tree1 tree2))  ; => T
```

## 总结

对于数值型对象，一般使用 `=` 就足够了，使用 `eq` 并不安全，不推荐。而字符和字符串，`char=` 和 `string=` 或 `char-equal` 和 `string-equal` 满足绝大部分使用场景了。`tree-equal` 用于判断树。

其他几个函数严格性排序如下：`eq > eql > equal > euqalp`，一般情况下，我们使用 `equal` 就可以了，除非知道自己需要做什么。

### 更多

更多细节，欢迎访问 [Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)。

[f_eq_sle]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq_sle.htm#EQ
[f_chareq]: http://www.lispworks.com/documentation/HyperSpec/Body/f_chareq.htm
[f_stgeq_]: http://www.lispworks.com/documentation/HyperSpec/Body/f_stgeq_.htm
[f_eq]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eq.htm
[f_eql]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm
[f_equal]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm
[f_equalp]: http://www.lispworks.com/documentation/HyperSpec/Body/f_equalp.htm
[f_tree_e]: http://www.lispworks.com/documentation/HyperSpec/Body/f_tree_e.htm
