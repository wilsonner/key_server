### 简介
一个简易的 KEY 服务器, 基于 TCP 实现，客户端可以根据 KEY 获取 VALUE


### 服务端使用步骤
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

