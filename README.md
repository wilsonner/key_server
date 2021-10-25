### 简介
一个简易的 KEY 服务器, 基于 TCP 实现，服务端可以自定义 KV，客户端可以根据 KEY 获取 服务端的 VALUE

### 设计方案
工程包含两个应用，主应用 `key_server` 和 `cache`，两个应用均是一个监督者进程多个 `simple one for one` 子进程树结构。`cache` 插件实际管理 KV，VALUE 采用一个 `gen_server` 进程管理，KEY 与 `gen_server` 的 pid 维护在 `ets` 表内，所以通过 KEY 可以先找到 pid 再获取到 VALUE。主应用 `key_server` 是一个基于 TCP 的服务器，每个连接采用一个 `gen_server` 管理，进程收到 `socket` 数据后按照如下二进制协议解析：第一字节前4位代表协议版本号后四位代表消息 id 第二字节开始剩余部分代表 KEY，完成解析后进程会调用 `cache` 插件的函数检索对应 VALUE，并将检索结果以及消息 id 统一应答给客户端

### 服务端模块介绍
- `key_server`：监听8888端口，accept socket，创建连接管理进程，绑定 socket 和 连接管理进程
- `key_server_msg_handler`：连接管理进程，每个连接分配一个，接收客户端数据，协议解析，KEY 检索，客户端应答
- `sc_store`：维护 KEY 与 VLAUE 管理进程 pid 的关系
- `sc_element`：VALUE 管理进程，每个 VALUE 分配一个
  - `sc_element:create`：新增 VALUE 进程，可以自定义过期时间，若超过过期时间未访问进程关闭
  - `sc_element:fetch`：查询 pid 进程维护的 VALUE
  - `sc_element:replace`：查询 pid 进程维护的 VALUE，若存在则替换为新的 VALUE
  - `sc_element:delete`：关闭 pid 进程
- `simple_cache` 对外提供 KV 的 新增，删除，检索 接口
  - `simple_cache:insert`：新增 KEY 和 VALUE
  - `simple_cache:delete`：删除 KEY 和 VALUE
  - `simple_cache:lookup`：检索 KEY 对应 VALUE

### 服务端启动步骤
```ini
rebar3 shell
```

### 客户端使用步骤
```ini
{ok, Sock} = gen_tcp:connect("localhost", 8888, [binary, {active,false}]),
gen_tcp:send(Sock, <<1:4, 2:4, “Tom”>>),
gen_tcp:recv(Sock, 0>),
gen_tcp:close(Sock).
```
