# Riddle

Riddle 是一个使用 Rust 编写的编程语言基础设施项目，旨在为 Riddle 语言提供解析、高级中间表示（HIR）构建以及后续的编译或解释支持。

## 特性

- **基于 Pest 的语法解析**：使用 [Pest](https://pest.rs/) 解析器生成器定义了 Riddle 语言的语法。
- **高级中间表示 (HIR)**：提供了一套结构化的 HIR 体系，用于表示模块、函数、变量、类型和表达式。
- **灵活的 HIR 构建器**：包含 `HirBuilder` 模式，方便以编程方式构建和操作 HIR。
- **强类型系统支持**：HIR 层级内置了对基本类型、复合类型（结构体、枚举）的支持。

## 项目结构

- `src/riddle.pest`: Riddle 语言的语法定义。
- `src/hir/`: 高级中间表示的核心实现。
  - `builder.rs`: 用于构建 HIR 的辅助工具。
  - `module.rs`: 顶层模块结构，容纳函数和类型定义。
  - `types.rs` & `type_expr.rs`: 类型系统及类型表达式定义。
  - `expr.rs` & `stmt.rs`: 表达式与语句的 HIR 定义。
- `src/main.rs`: 项目入口，包含 HIR 构建的演示示例。

## 语法概览

Riddle 语言支持以下特性：

- **变量声明**：`var x: int = 42;`
- **函数定义**：
  ```rust
  fun add(a: int, b: int): int {
      a + b
  }
  ```
- **结构体与枚举**：支持强类型的复合数据结构。
- **块表达式**：支持类 Rust 的块作用域和返回值。

## 快速开始

确保你已经安装了 [Rust](https://www.rust-lang.org/)。

1. 克隆或下载本项目。
2. 运行示例程序：
   ```bash
   cargo run
   ```

该程序将演示如何使用 `HirBuilder` 创建一个模块、定义一个函数并添加局部变量，最后打印出生成的 HIR 结构。

## 开发状态

本项目目前处于早期开发阶段，主要专注于 HIR 架构的搭建和语法解析的完善。

## 许可证

[Apache License 2.0](LICENSE)
