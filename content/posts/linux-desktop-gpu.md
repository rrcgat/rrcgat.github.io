---
title: "Linux Desktop GPU"
date: 2023-09-02T09:09:42+08:00
draft: false
tags: [linux]
categories: []
showToc: false
# TocOpen: true
# description: "Desc Text."
---
最近在 Linux 桌面上分别尝试了 AMD 和 NVIDIA 的独立显卡（使用的都是 Wayland 协议），踩了一些坑，分享出来。

## AMD

对于 AMD 显卡，安装一路顺畅，跟着 Arch Wiki 安装完系统，装个桌面就能用了。进入桌面后跟笔记本的核显使用体验一致，并且后续使用并无异常，比睡眠后唤醒，无黑屏问题等。

如果只是想安静地作为桌面显示用的显卡，AMD 表现很完美。但要是想玩一些模型，比如 Whisper 和 Llama 2，那坑就来了。

既然想玩模型，那多少是知道 AMD 显卡在炼丹领域是比不上 NVIDIA 的。但具体相差多远呢？PyTorch 有 ROCm 的选项支持 AMD 显卡，但模型就不一定能运行了。

实际测试下来，能跑的模型也有，比如：

- ChatGLM
- Stable Diffusion web UI

ChatGLM 直接启动会报错，显卡不是最新的。根据 [WenJieLife](https://github.com/WenJieLife) 的[提示](https://github.com/RadeonOpenCompute/ROCm/issues/2270#issuecomment-1631370345)，设置环境变量解决：`export HSA_OVERRIDE_GFX_VERSION=10.3.0 HCC_AMDGPU_TARGET=gfx1030`。但如果使用量化模型，就不行了。Stable Diffusion web UI 同样需要进行一些配置，但有现成的教程可参考：

- [A卡ROCm运行Stable Diffusion AI画图](https://www.bilibili.com/read/cv19172736?spm_id_from=333.999.0.0)
- [Stable Diffusion 使用 AMD ROCm 显卡加速](https://vicfree.com/2023/03/stable-diffusion-amd-gpu-rocm-ubuntu/)

后续尝试 Llama 2 模型，跑不动，[WhisperX](https://github.com/m-bain/whisperX) 需要 NVIDIA 显卡。

## NVIDIA

安装时建议按照 [NVIDIA - ArchWiki](https://wiki.archlinux.org/title/NVIDIA) 中的步骤进行。

经过一系列设置后，第一次进入桌面，NVIDIA 似乎没那么 Fxxk？使用后发现此时用的是 Xorg，如果要使用 Wayland，需要加个软链接 `ln -s /dev/null /etc/udev/rules.d/61-gdm.rules`。重新进入，发现颜色不太对，细看发现夜览模式没有了：

![](/images/screenshot-night-light.png)

问题追踪 [Gnome night light not working under Wayland](https://forums.developer.nvidia.com/t/gnome-night-light-not-working-under-wayland/193590/72)：

> The 535 driver will not have GAMMA_LUT support. I’m unsure about 545.

距离这条评论有几个月了，驱动版本依旧是 535。

除此之外，如果你掉以轻心，那么你可能陷入以下困境：睡眠后唤醒，屏保首先不正常，鼠标指针也可能丢失了。进入桌面后，可能面对的是经过打码后的软件（尤其是使用了 GPU 渲染加速的终端），并且文字可能消失。一番尝试无果后，你可能选择重启，然后搜索解决方案。大概率你会找到 [setup-nvdia-suspend.sh](https://gist.github.com/bmcbm/375f14eaa17f88756b4bdbbebbcfd029) 和 [NVIDIA/Tips and tricks](https://wiki.archlinux.org/title/NVIDIA/Tips_and_tricks)。前一个链接似乎说的就是现在的睡眠环境问题，设置好了后，重来一遍，发现还是有码。一通折腾后，你发现一个 trick，即调整字体大小或缩放能让渲染重新变得正常。依靠这个 trick 活过一段时间后，再次进入桌面，发现此时的桌面已经是层层加码，甚至软件已经透明了，这意味着你已经找不到使用那个 trick 的路了，重启桌面是唯一的路……

解决方法藏在了上述的第二个链接的 [Preserve video memory after suspend](https://wiki.archlinux.org/title/NVIDIA/Tips_and_tricks#Preserve_video_memory_after_suspend) 部分。很有迷惑性的 video memory，没想到会跟这个有关。

跑模型很顺滑，但如果系统或驱动更新了，那么 PyTorch 会跑不起来，`nvidia-smi` 也会报错。日常使用还是建议跑在更稳健的系统上，安装好环境后坚持到下一次更新换代……

## 选择建议

如果只是想舒服地使用 Linux Wayland 桌面，不想折腾各种配置，那么**AMD/核显**是最好的选择。如果还想跑模型，玩玩炼丹，那 NVIDIA 依旧是首选，并且经过一番配置后，桌面使用问题也不大。
