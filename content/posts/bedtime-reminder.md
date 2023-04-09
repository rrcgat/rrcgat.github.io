---
title: "一个睡眠提醒小工具"
date: 2023-04-09T13:14:20+08:00
draft: false
tags: [tools]
categories: []
# showToc: false
# TocOpen: true
# description: "Desc Text."
---

之前有段时间太累，但一到晚上又很精神，思绪也比较活跃，很容易就把时间抛到脑后了。第二天是周末还好，可以睡晚点。但如果是工作日，很容易睡眠不足。经历了几次身体不适之后，决定写个脚本，提醒自己睡觉。

晚上通常都是使用电脑，且系统是 Linux，很容易想到使用定时器来做这件事。粗暴一点的做法是直接用定时器在特定时间进行提醒，但缺点也很明显，如果系统锁屏了但未休眠，那么定时任务依旧会在安排的时间点执行。最终决定写个脚本配合定时器一起运行。

首先要解决的是如何判断当前系统的锁屏状态。当前的系统桌面是 Gnome + Wayland，往这个方向搜索得到的结果不是很清晰，有些方法是过时的。最终测试下来，使用 `dbus-send` 的方式最靠谱。使用以下命令可判断系统是否锁屏：

```shell
# If it is locked, boolean true will be print
dbus-send --session --print-reply --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.GetActive | grep 'boolean true'
```

然后就是确定提醒的时间点了。不同时间点，提醒的内容的应该有区别。这个比较简单，获取当前系统时间，跟设置的时间点进行比较即可。通知工具使用 `notify-send`，很方便。

只是简单地通知的话，感觉可能没啥效果，于是想到加上强制休眠的时间点，最后做成了 systemd 的定时器，放在这里：[bedtime-reminder](https://github.com/rrcgat/bedtime-reminder)。

实际使用了一段时间，效果挺好的，但基本上是卡点睡觉。等适应后，可以把时间调早一点。但通知的作用不大，最后靠的是强制休眠。果然还是 deadline 最有效。
