---
title: "Common Lisp 项目的代码结构"
date: 2020-06-07T23:28:33+08:00
draft: false
tags: ['Common Lisp']
---

相比于 Python 项目简单清晰的目录结构，Common Lisp 的项目结构要复杂不少，尤其是大型项目。但当你知道每个文件/目录的作用时，会发现这目录结构也不复杂。

以 [Postmodern][Postmodern] 项目为例，看看实际项目是怎样组织代码的。

[Postmodern]: https://github.com/marijnh/Postmodern

该项目目录结构如下：

```
├── cl-postgres/...
├── cl-postgres.asd
├── postmodern/
│   ├── connect.lisp
│   ├── ...
│   ├── package.lisp
│   └── tests
│       ├── ...
│       ├── test-package.lisp
│       └── tests.lisp
├── postmodern.asd
├── simple-date/...
├── simple-date.asd
├── s-sql/...
├── s-sql.asd
└── ...
```

这里省略了部分目录和文件，但不影响我们解读该目录结构。可以点击链接了解完整的目录结构。

首先看最外层的文件，这里列出的都是以 `.asd` 为后缀的文件，这些文件描述了源代码间的依赖关系，使它们能按正确的顺序进行编译和加载。而这依靠的便是 ASDF 自动编译系统。

ASDF，全称 Another System Definition Facility，该构建系统指定了 Common Lisp 程序中各系统的组成及控制各组件能按正确的顺序进行编译、加载和测试等等。

> ASDF, or Another System Definition Facility, is a build system: a tool for specifying how systems of Common Lisp software are made up of components (sub-systems and files), and how to operate on these components in the right order so that they can be compiled, loaded, tested, etc.

这里不讨论 ASDF 的具体用法。直接看看 `postmodern.asd` 文件的内容：

```lisp
;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defpackage :postmodern-system
  (:use :common-lisp :asdf)
  (:export :*threads*))
(in-package :postmodern-system)

...

(defsystem "postmodern"
  ...
  :components
  ((:module "postmodern"
            ...
  )))

(defsystem "postmodern/tests"
  ...
  )
```

第一行的作用是让 Emacs 为该文件添加正确的语法支持。

代码首先定义了一个名为 `postmodern-system` 的包（package），之后 `defsystem` 定义的系统（system）描述了对应项目的代码结构。

`defsystem` 宏的两个主要的选项中，
- `:depends-on` 指定该项目依赖的其他 ASDF 格式的项目，
- `:components` 定义了有依赖关系的源代码文件。

要注意的是，系统定义文件（system definition file）名与系统名（由 `defsystem` 定义）必须保持一致。如要同时定义其他系统，系统名需要使用前缀 `<system-name>/`。


在该项目中，`postmodern.asd` 文件对应的系统是 `postmodern`，额外的系统 `postmodern/tests` 使用的前缀是 `postmodern/`。

看下 `postmodern/` 目录中 `package.lisp` 的内容，可以发现该文件专门定义包，并且这个包存放的是实际的逻辑代码。

现在再看其他的文件或目录，代码结构都是类似的，理解起来并不难。

至此，我们已经了解了 Common Lisp 项目的代码结构是怎样组织的。相比 Python 来说，多了一些东西，但学习之后，并没有什么难以理解的地方。

想要了解 ASDF 的具体的用法，可以查阅 [ASDF Manual][asdf_man]。

[asdf_man]: https://common-lisp.net/project/asdf/asdf.html

