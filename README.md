# violet

[![Run Tests](https://github.com/dannypsnl/violet/actions/workflows/ci.yaml/badge.svg)](https://github.com/dannypsnl/violet/actions/workflows/ci.yaml)

> **Warning** The project still in early stage.

A programming language focuses on

- dependent type
- effect system
- semantic versioning
- separate compilation

### Usage

Load module into REPL

```
violet example/module.vt
```

#### Example

```
data Unit | unit

data Nat
| zero
| suc Nat

data Bool
| true
| false

def zero? (n : Nat) : Bool =>
match n
| zero => true
| suc _ => false

record T {
  a : Nat,
  b : Bool,
}

def t (x : Nat) : T => (x, zero? x)
```

### Develop

You will need to install lean, via any package manager would be fine. Especially recommend vscode plugin (https://marketplace.visualstudio.com/items?itemName=leanprover.lean4), install it and wait, it should install `elan`, `lean`, and `lake` for you.

Build the project

```shell
lake build
```

[^1]: https://dl.acm.org/doi/10.1145/3408983
