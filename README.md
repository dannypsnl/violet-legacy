# violet

> **Warning** The project still in early stage.

A programming language focuses on

- dependent type
- effect system
- semantic versioning
- separate compilation

### Usage

Type checking

```shell
violet check example/module.vt
```

Open REPL with module

```
violet example/module.vt
```

#### Example

```
data Nat
| zero
| suc Nat

data Bool
| true
| false

def zero? (n : Nat) : Bool =>
elim n
| zero => true
| suc _ => false
```

### Develop

You will need to install idris, via any package manager would be fine, here is an example for macOS.

```shell
brew install idris2
```

Build commands

```shell
idris2 --build
```

#### Editor

There has a `.editorconfig` for this repository, please follow it. We pick tab indention, to ensure people can access the code. Considering visually impaired people and many disabled, all you need is just config the width of tab to fit your personal favourite.

#### Nix

If you know and willing to use nix, we have nix flakes supporting.

```shell
nix flake check
nix build .
```
