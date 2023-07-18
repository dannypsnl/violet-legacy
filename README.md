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

record T where
  a : Nat;
  b : Bool;

def t (x : Nat) : T => (x, zero? x)
```

### Develop

You will need to install lean, via any package manager would be fine. Especially recommend vscode plugin (https://marketplace.visualstudio.com/items?itemName=leanprover.lean4), install it and wait, it should install `elan`, `lean`, and `lake` for you.

Build the project

```shell
lake build
```

#### Theory

Here are some related theories we already applied or going to use.

- elaboration[^1]
- universe polymorphism[^2] will try mugen
- termination checker[^3] will use lexicographic recursion
- type class[^4]
- indexed data type[^5]

[^1]: Elaboration with first-class implicit function types: https://dl.acm.org/doi/10.1145/3408983
[^2]: An Order-Theoretic Analysis of Universe Polymorphism: https://favonia.org/files/mugen.pdf
[^3]: foetus - Termination Checker for Simple Functional Programs: https://www2.tcs.ifi.lmu.de/~abel/foetus.pdf
[^4]: Tabled Typeclass Resolution: https://arxiv.org/pdf/2001.04301.pdf
[^5]: A SIMPLER ENCODING OF INDEXED TYPES: https://arxiv.org/pdf/2103.15408v4.pdf
