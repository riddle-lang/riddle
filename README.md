# Riddle

<p align="center">
  <img src="resources/logo.svg" alt="Riddle logo" width="160" />
</p>

> 一个用 Rust 编写的实验性语言项目，聚焦语法设计、类型系统与可执行生成的探索。

好吧这个项目已经被废弃了，你可以去找用 MoonBit 实现的 Riddle

## ✨ 特性

- Rust 风格语法：函数、结构体、枚举、泛型、trait 与 impl
- 语法与语义清晰：显式类型、基本字面量、块表达式
- 小而美的语言内核，方便扩展与实验

## 🚀 快速开始

### 依赖

- Rust（stable）
- Cargo
- Rust crates（详见 `Cargo.toml`）：
  - `pest = "2.8.5"`
  - `pest_derive = "2.8.5"`
  - `cranelift = "0.116.1"`
  - `cranelift-module = "0.116.1"`
  - `cranelift-object = "0.116.1"`
  - `cranelift-native = "0.116.1"`
  - `cranelift-frontend = "0.116.1"`
  - `target-lexicon = "0.12"`

### 运行示例
```bash
cargo run
```
## 🧩 语言一瞥
```riddle
extern "C" {
    fun printf(fmt: &str, ...) -> int;
}

fun main() -> int {
    let answer = 42;
    printf("hello %d", answer);
    return 0;
}
```
## 📦 项目结构

- `src/frontend` — 语法与 AST
- `src/hir` — 中间表示与类型推断
- `src/codegen` — 代码生成（基于 Cranelift）
- `resources/logo.svg` — 项目标识

## 🗺️ 路线图

- [ ] 完善错误提示与诊断
- [ ] 更多标准库/运行时支持
- [ ] 更丰富的表达式与模式匹配
- [ ] 测试与基准体系

## 🤝 贡献

欢迎 PR 与 Issue。提交前建议格式化代码并附上简要说明。

## 📄 License

Apache-2.0