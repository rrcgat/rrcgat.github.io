---
title: "GnuPG"
date: 2019-12-29T15:12:15+08:00
draft: true
tags: []
---

最近换用 [chezmoi](https://github.com/twpayne/chezmoi) 同步自己的 dotfiles，
需要用到 GnuPG 工具，记录一下。

## 介绍

看一下几个相关的名词：

### PGP

> [Pretty Good Privacy](https://en.wikipedia.org/wiki/Pretty_Good_Privacy) (PGP) is an encryption program that provides cryptographic privacy and authentication for data communication.

PGP 是一套提供了信息加密与验证的应用程序，也是最初的商业软件名。

### OpenPGP

> PGP Inc. proposed to the IETF that there be a standard called [OpenPGP](https://en.wikipedia.org/wiki/Pretty_Good_Privacy#OpenPGP). 
> They gave the IETF permission to use the name OpenPGP to describe this new standard as well as any program that supported the standard.

OpenPGP 实现了与 PGP 兼容的的标准。

### GnuPG

> [GNU Privacy Guard](https://en.wikipedia.org/wiki/GNU_Privacy_Guard) (GnuPG or GPG), a free-software replacement for Symantec's PGP cryptographic software suite, 
> compliant with RFC 4880, the IETF standards-track specification of OpenPGP.

GnuPG 是实现了 OpenPGP 标准的自由软件，`gpg` 是其命令行工具。

> GnuPG 用环境变量 `$GNUPGHOME` 定位配置文件的位置，默认情况下此变量并未被设置，会直接使用 `$HOME`，所以默认的配置目录是 `~/.gnupg`。
> 默认的配置文件是 `~/.gnupg/gpg.conf` 和 `~/.gnupg/dirmngr.conf`。

## 创建密钥

使用 `--full-generate-key` 选项一步步生成新的密钥对。

```sh
gpg --full-generate-key
```

1. 首先选择密钥类型，默认（`1`）即可。
2. 下一步设置密钥长度，同样选择默认（`2048`）。[为什么不是 4096？](https://www.gnupg.org/faq/gnupg-faq.html#no_default_of_rsa4096) 看个人选择。
3. 设置过期时间，选择 `0` 用不过期。再次确认。
4. 输入名称（`rrcgat`）。
5. 输入电子邮件（`rrcgat@gmail.com`）。
6. 输入注释，如果需要的话。
7. 确认 USER-ID，`rrcgat <rrcgat@gmail.com>`
8. 设置密钥密码。可选。

## 使用

命令中需要使用到 user-id 的部分，可以使用 key ID、指纹、名字或邮箱替代。

> Whenever a `user-id` is required in a command, it can be specified with your key ID, fingerprint, a part of your name or email address, etc.

### 加密

要加密 `doc` 文件，使用以下命令，会在 `doc` 文件所在目录生成 `doc.gpg`。

```sh
gpg --recipient <user-id> --encrypt doc
```

### 解密

要解密 `doc.gpg`，使用 `--decrypt` 参数，
`--output` 或 `-o` 指定解密后的文件,
不指定解密内容输出到标准输出上。

```sh
gpg --output doc --decrypt doc.gpg
```

### 导入公钥

使用 `--import` 选项导入密钥。

```sh
gpg --import public.key
```

### 导出公钥

`--export` 导出密钥，`--armor` 指定输出 ASCII 格式。

```
gpg --output public.key --armor --export <user-id>
```

### 使用公钥服务器

如果需要的话：

```
gpg --send-keys <key-id>
```

## 维护

### 备份

选项 `--export-secret-keys` 导出密钥。

```
gpg --output private.asc --export-secret-keys --armor <user-id>
```

### 恢复

```
gpg --import privkey.asc
```

### 子密钥

使用时，可以把主密钥隔离，只使用子密钥。

`--quick-add-keys` 添加子密钥，
`fpr` 替换为对应的 `key-id`，
`algo` 为加密算法，
`usage` 是子密钥的用途（`encr`、`sign`、`auth`），
如 `gpg --quick-add-key <key-id> rsa2048 sign`。

```
gpg --quick-add-key fpr [algo [usage [expire]]]
```

或者使用交互式命令行添加。

导出子密钥时，
`subkey-id` 指定要导出的子密钥，
不加 `!` 将导出所有的子密钥。

```
gpg -a -o subkey.gpg --export-secret-subkeys [subkey-id]!
```

卸载主密钥，使用子密钥。`!`不能漏，不然会把所有密钥从本地移除。

```
gpg --delete-secret-keys <key-id>!
```

更多用法，可以仔细看看 man 手册，介绍很全。

## 参考
[GnuPG - ArchWiki](https://wiki.archlinux.org/index.php/GnuPG)
[生成你的主密钥](https://linux.cn/article-9529-1.html)
[生成 PGP 子密钥](https://linux.cn/article-9607-1.html)
[manual page of gpg](#)
