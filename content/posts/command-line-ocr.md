---
title: "在终端中使用 OCR"
date: 2023-03-12T22:22:21+08:00
draft: false
tags: [Tools]
categories: []
# showToc: false
# TocOpen: true
# description: "Desc Text."
---

最近比较常用到 OCR 的功能，主要用的是 [tesseract](https://github.com/tesseract-ocr/tesseract)，再搭配终端访问剪贴板内容和一些其他工具，整体体验很流畅。

首先需要安装 tesseract，Arch 下可一条命令解决：`sudo pacman -S tesseract tesseract-data-chi_sim tesseract-data-eng`，后两个是语言数据，`chi_sim` 和 `eng` 分别对应简体中文和英文。然后搭配 `xclip` 或 `wl-paste` 就可以食用了：

```shell
# tesseract usage: `tesseract FILE OUTPUTBASE [...]`
# If FILE is stdin or - then the standard input is used.
# If OUTPUTBASE is stdout or - then the standard output is used.

xclip -selection clipboard -t image/png -o|tesseract stdin stdout -l chi_sim+eng
# on Wayland, you can yuse wl-paste:
wl-paste -t image/png|tesseract -l chi_sim stdin stdout
```

为什么需要指定 MIME 类型？Linux 下剪贴板有多种类型（selection），主要包括 `PRIMARY`、`CLIPBOARD` 和 `SECONDARY`，常用的是 `PRIMARY` 和 `CLIPBOARD`。

- `PRIMARY` selection 用于当前选中的文本，不需要显式复制，鼠标中键就能粘贴。
- `CLIPBOARD` selection 显式复制粘贴，并支持[多种数据格式](https://stackoverflow.com/questions/3571179/how-does-x11-clipboard-handle-multiple-data-formats)。

对于图片，我们使用的是 `CLIPBOARD`，并且 MIME 类型为图像（比如 `image/png`）。

可以使用 alias，比如 `alias ocr=wl-paste -t image/png|tesseract -l chi_sim stdin stdout`，方便使用。

实际使用时，如果图片中有中文，那么得到的结果中每个中文之间都有空格，不便之后的使用。于是花了点时间糊了个脚本，用于去除多余的空格和修复不正确的标点符号：

```python
def remove_spaces_around_chinese(text: str):
    """Remove all spaces between Chinese characters"""
    text = re.sub(f"([\u4e00-\u9fff])([ \u3000]+|\n)([\u4e00-\u9fff])", r"\1\3", text)
    text = re.sub(f"([\u4e00-\u9fff])([ \u3000]+|\n)([\u4e00-\u9fff])", r"\1\3", text)

    return text


def remove_redundant_sapces(text: str) -> str:
    text = re.sub(f" +([{NON_PREFIX_SPACE_PUNCTUATION}])", r"\1", text)
    text = re.sub(f"([{NON_SUFFIX_SPACE_PUNCTUATION}]) +", r"\1", text)
    text = re.sub(f"""([{NON_PREFIX_SPACE_PUNCTUATION_EN}#'"]) +""", r"\1 ", text)
    text = re.sub("([\u4e00-\u9fff])( *)([‘’“”])", r"\1\3", text)
    text = re.sub("([‘’“”])( *)([\u4e00-\u9fff])", r"\1\3", text)

    return text.rstrip()


def _standardlize_repl(matchobj: re.Match):
    return matchobj.group().translate(ENGLISH_TO_CHINESE_TABLE)


def fix_mixed_punctuation(text: str) -> str:
    text = re.sub("([\u4e00-\u9fff，。：；！？])(\\s*)([,.:;!?()])", _standardlize_repl, text)
    text = re.sub("([\u4e00-\u9fff，。：；！？])(\\s*)([,.:;!?()])", _standardlize_repl, text)

    return text
```

完整的代码放在 Gist 上：[format_text.py](https://gist.github.com/rrcgat/c75159efb6ee5a0b50a36bb3291d1c3e)，使用时再加一条管道即可：`<...> | format_text`。

对于 macOS，可以参考 @laixintao 的用法：[pngpaste - | tesseract stdin stdout](https://www.kawabangga.com/posts/4876)。

想详细了解 Linux 剪贴板，可参考：

- [What is the X11 Secondary  selection ?](https://sjohannes.wordpress.com/2010/02/28/what-is-the-x11-secondary-selection/)  
- [Clipboard - ArchWiki](https://wiki.archlinux.org/title/clipboard)  
- [The Secondary Selection](http://www.cs.man.ac.uk/~lindsec/secondary-selection.html)  
- [X Selections , Cut Buffers, and Kill Rings.](https://www.jwz.org/doc/x-cut-and-paste.html)

