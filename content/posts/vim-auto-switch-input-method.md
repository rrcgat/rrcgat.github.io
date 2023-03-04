---
title: "关于 Vim 自动切换输入法"
date: 2023-03-05T00:14:57+08:00
draft: false
tags: [Vim]
categories: []
showToc: false
---

最近在折腾 Vim 配置，花了不少时间优化，可以说是差生文具多了。主要是把包管理器从 Packer 换成了 lazy.nvim，然后从 [LazyVim](https://github.com/LazyVim/LazyVim) 中抄了一些配置，顺便把目录组织结构换成 LazyVim 那样的方式进行组织。

然后前段时间又在推上看到关于 Vim 编辑器下中英文切换的讨论。想要实现的效果是 Vim 在离开 `INSERT` 模式时，切换输入法到英文状态，当再次进入 `INSERT` 模式时，又自动切换回去。

[im-select](https://github.com/daipeihust/im-select) 是比较多人推荐的工具，用于在命令行中切换输入法。该项目介绍的用法适用于 VSCodeVim，对于纯 Vim 没有提及。

Vim 可以在不同事件发生时自动执行预先设置的命令，自动切换输入法应该也是类似。找到一个插件 [vim-barbaric](https://github.com/rlue/vim-barbaric)，看了下里面的源码，思路也是如此。于是往自己的 Vim 配置中加了以下几行代码：

```lua
-- Auto switch input method
local myim = vim.fn.system('ibus engine')
local function switch_eng()
  myim = vim.fn.system('ibus engine')
  -- print("switch to eng from: " .. myim)
  os.execute("ibus engine xkb:us::eng")
end

local function restore_normal_im()
  -- print("restore to: " .. myim)
  os.execute("ibus engine " .. myim)
end

vim.api.nvim_create_autocmd({ "InsertLeave" }, {
  group = augroup("switch_im"),
  callback = switch_eng
})

-- Restore to original im when InsertEnter and VimLeave happens
vim.api.nvim_create_autocmd({ "InsertEnter", "VimLeave" }, {
  group = augroup("restore_im"),
  callback = restore_normal_im
})

```

适用于 Linux 下的 Neovim + ibus（+rime）。使用体验提升不明显，可能是比较少在 Vim 下输入中文。同时这种切换方式是输入法之间的切换，对于习惯使用 Shift 键在输入法内切换中英文状态的人来说，不太友好。
