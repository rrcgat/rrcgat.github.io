---
title: "Python 进程间通信"
date: 2020-05-25T00:14:58+08:00
draft: false
showToc: true
tags: [Python]
---

## 进程间通信的几种方式

参考维基百科的分类，进程间通信主要有以下几种：

- 文件（File）
- 信号（Signal）
- 套接字（Socket）
- Unix 域套接字（Unix domain socket）
- 消息队列（Message queue）
- 匿名管道（Anonymous pipe）
- 命名管道（Named pipe）
- 共享内存（Shared memory）
- 消息传递（Message passing）
- 内存映射文件（Memory-mapped file）

看下使用 Python 怎么实现。

环境：Arch Linux + Python3.8

### 文件

符合直觉，数据存在硬盘上，可基于文本或二进制。

对于 pa.py

```python
SHARED_FILE = '/tmp/shared_file'


def send():
    with open(SHARED_FILE, 'w', encoding='utf-8') as f:
        f.write('some data...')
```

对于 pb.py

```python
def receive():
    with open(SHARED_FILE, encoding='utf-8') as f:
        print(f.read())
```

为了避免竞争，数据流向是单向的，可使用两个或以上的文件进行双向通信。

### 信号

通信偏向底层，且一般不用于传输数据。

Python 中有 `os.kill(pid, sig)` 以及 `signal` 模块。
`os.kill` 发送 `sig` 给进程 `pid`，一般会意外终止进程 `pid`。

```python
import os
import signal


os.kill(48523, signal.SIGKILL)
```

`signal` 模块的例子可以看 [signal — Set handlers for asynchronous events][1] 中给出的，简洁明了。

[1]: https://docs.python.org/3/library/signal.html#example

### 套接字

使用 `socket` 模块，C/S 形式的通信。

对于 pa.py

```python
# Echo server program
import socket


HOST = '127.0.0.1'
PORT = 50007


def send():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind((HOST, PORT))  # 绑定地址与端口
        s.listen(1)  # 监听的连接数
        conn, addr = s.accept()
        with conn:
            print('Connected by', addr)
            while True:
                data = conn.recv(1024)
                if not data:
                    break
                conn.sendall(data)
```

对于 pb.py

```python
# Echo client program
import socket


HOST = '127.0.0.1'
PORT = 50007


def receive():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((HOST, PORT))
        s.sendall(b'Hello, world')
        data = s.recv(1024)
    print('Received', repr(data))
```

例子来自于官方文档中的 [socket — Low-level networking interface][2]，稍作修改。

在 Python3.8 中，新增了 `socket.create_server` 函数，合并了 `bind` 这一步。
对应的 `socket.create_connection` 函数用于创建连接。

[2]: https://docs.python.org/3/library/socket.html#example

### Unix 域套接字

Unix 域套接字不经过网络，直接在内核中通信。

使用方法与 `socket.AF_INET` 或 `socket.AF_INET6` 相似，
只需把对应的 `socket.AF_INET` 或 `socket.AF_INET6` 改为 `socket.AF_UNIX`，
绑定或连接的地址由 `(HOST, PORT)` 变为本地文件，如 `'/tmp/unix_socket'`。

### 消息队列

Python 标准库中，[`multiprocessing.Queue`][3] 只能用于有关系的进程中，
而借助第三方库的 `ipcqueue` 或 `sysv_ipc`，可在独立的进程中通信。

[3]: https://docs.python.org/3/library/multiprocessing.html#multiprocessing.Queue

`ipcqueue` 支持 `POSIX` 和 `System V` 形式的消息队列。

看下 `POSIX` 标准的消息队列。

pa.py

```python
from ipcqueue import posixmq


def send():
    q = posixmq.Queue('/foo')

    q.put([1, 'A'])
    q.put([2, 'B'], priority=2)  # priority 参数设置优先级
    q.put([3, 'C'], priority=0)
```

pb.py

```python
from ipcqueue import posixmq


def receive():
    q = posixmq.Queue('/foo')

    print(q.get())
    print(q.get())
    print(q.get())

    q.close()
    q.unlink()
```

而 `System V` 消息队列稍有不同。

pa.py

```python
from ipcqueue import sysvmq


def send():
    q = sysvmq.Queue(1)

    q.put([1, 'A'])
    q.put([2, 'B'], msg_type=2)
    q.put([3, 'C'], msg_type=2)
    q.put([4, 'D'], msg_type=1)
```

pb.py

```python
from ipcqueue import sysvmq


def receive():
    q = sysvmq.Queue(1)

    print(q.get(msg_type=2))  # msg_type 参数设置消息类型
    print(q.get())
    print(q.get())
    print(q.get())

    q.close()
```

以上代码修改自 [ipcqueue documentation][ipcqueue]。

两者用法类似，但有一些不同，主要有：

1. POSIX message queue 的标识符为字符，且必须以 `/` 开头；
   而 System V message queue 标识符为整数，且不能为负数。
   为 0 时，表示私有队列。
2. POSIX message queue 可以设置优先线，读取消息按 FIFO 方式来读；
   System V message queue 无优先级概念，但可以设置消息类型，
   并按消息类型以 FIFO 方式读取，而 POSIX message queue 不区分消息类型。

### 匿名管道

匿名管道用于父子进程间打开读写通道进行通信，
在大多数 Shell 中可以使用 `|` 来创建。

```python
import os
import sys


def main():
    r, w = os.pipe()

    if os.fork():
        # Parent process receive data
        os.close(w)
        r = os.fdopen(r)
        print('Parent reading', r.read())
        r.close()
        sys.exit(0)
    else:
        # Child process write data
        os.close(r)
        w = os.fdopen(w, 'w')
        w.write('Hello, world')
        w.close()
        sys.exit(0)
```

要双向通信，可打开两条管道。`multiprocessing.Pipe` 可直接支持双工管道。

### 命名管道

不同于匿名管道，命名管道利用了文件系统，可用于两个无关的进程中进行通信。

pa.py

```python
import os
import errno


FIFO = '/tmp/named_pipe'


def send():
    try:
        os.mkfifo(FIFO)
    except OSError as oe:
        if oe.errno != errno.EEXIST:
            raise oe

    with open(FIFO, 'w') as f:
        f.write('Hello, world')
```

pb.py

```python
import os


FIFO = '/tmp/named_pipe'


def receive():
    try:
        os.mkfifo(FIFO)
    except OSError:
        pass

    with open(FIFO) as f:
        while True:
            data = f.read()
            if len(data):
                print(data)
            else:
                break
```

两者均是阻塞直到另一方打开，但可以使用 `os.open` 进行非阻塞通信。

代码修改自 [Python read named PIPE][4]，且仅适用于 Unix 平台。
Windows 系统需要借助 `win32pipe` 和 `win32file`，
具体可以看 [Named Pipes Communication between Python Server and Python Client on Window][5]。

[4]: https://stackoverflow.com/questions/39089776/python-read-named-pipe
[5]: https://medium.com/datadriveninvestor/named-pipes-communication-between-python-server-and-python-client-on-window-8cdf64504801

### 共享内存

共享内存是可以被不同进程同时访问的内存空间，可跨不同进程通信。

Python3.8 中新增了 [`multiprocessing.shared_memory`][6] 模块以支持共享内存通信，
使用的是 System V 风格。之前提到的 [`sysv_ipc`][sysv_ipc] 也可以用于共享内存通信，同样是 System V 风格。

[6]: https://docs.python.org/3/library/multiprocessing.shared_memory.html

官方文档中使用 Numpy 演示 `multiprocessing.shared_memory`。
查看[源代码][7]，发现对象销毁会调用 `self.close` 关闭共享内存的访问，
所以必须保证两个进程活着才行。


[7]: https://github.com/python/cpython/blob/3.8/Lib/multiprocessing/shared_memory.py

`sysv_ipc` 除了消息队列和共享内存，还可使用信号量进行通信。

### 消息传递

> 消息传递是一项在计算机上激活动作（即运行程序）的技术。
> 与传统按名字调用程序的方式不同，
> 消息传递使用对象模型（object model）区别常规功能与特定实现。
> 被激活的程序发送一条消息，并依靠对象选择并执行对应的代码。
> 使用中间层的理由基本分为两类：封装和分发。

以上文字翻译自 [Message passing][8]，可能不太准确。

[8]: https://en.wikipedia.org/wiki/Message_passing#Overview

常见的消息传递系统有 RPC、RMI、MPI。

以 MPI 为例子。Python 中实现包括 pyMPI、mpi4py、pypar、MYMPI 和 ScientificPython 的子模块 MPI。这里使用 [mpi4py][9] 模块。

[9]: https://mpi4py.readthedocs.io/en/stable/

```python
import numpy
from mpi4py import MPI


comm = MPI.COMM_WORLD
rank = comm.Get_rank()

rand_num = numpy.zeros(1)

if rank == 1:
    rand_num = numpy.random.random_sample(1)
    print("Process", rank, "drew the number", rand_num[0])
    comm.Send(rand_num, dest=0)

if rank == 0:
    print("Process", rank, "before receiving has the number", rand_num[0])
    comm.Recv(rand_num, source=1)
    print("Process", rank, "received the number", rand_num[0])
```

将代码保存为 mpi.py，执行命令 `mpiexec -n 2 python mpi.py`。
`-n` 选项指定进程数。同时启动一组进程，
每个进程都有一个唯一的编号（在这是 `rank`），
根据不同的编号，程序执行不同的代码。
不同进程可以通过 `Send` 和 `Recv` 进行通信。

代码参考自 [Python MPI: Message Passing][10]。

[10]: https://nyu-cds.github.io/python-mpi/02-messagepassing/

### 内存映射文件

与共享内存类似，但将内存映射为文件。直接看看怎么用。

这里直接使用标准库中的 [`mmap`][11] 模块。

[11]: https://docs.python.org/3/library/mmap.html

pa.py

```python
import mmap
import os


path = '/tmp/mmapfile'


def send():
    fd = os.open(path, os.O_CREAT | os.O_TRUNC | os.O_RDWR)
    data = b'Hello, world'
    os.write(fd, b'\x00' * len(data))

    mm = mmap.mmap(fd, 0)
    mm[:] = data
```

pb.py

```python
import mmap
import os


path = '/tmp/mmapfile'


def receive():
    fd = os.open(path, os.O_RDONLY)
    mm = mmap.mmap(fd, 0, prot=mmap.PROT_READ)
    print(mm[:])
```

文件描述符为 `-1` 时，表示映射匿名内存。

Windows 有一点不同，还可直接使用 `mmap.mmap(0, length, tagname)`进行通信。

## 更多参考

- [Welcome to ipcqueue’s documentation!][ipcqueue]
- [System V IPC for Python][sysv_ipc]
- [细说linux IPC（十）：system V 消息队列][cnblog_system_v]
- [mq_overview(7) - Linux man page][mq_overview]

[ipcqueue]: https://pythonhosted.org/ipcqueue/#
[sysv_ipc]: http://semanchuk.com/philip/sysv_ipc/
[cnblog_system_v]: https://blog.csdn.net/gentleliu/article/details/41806415
[mq_overview]: https://linux.die.net/man/7/mq_overview
