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

You will need to install lean, via any package manager would be fine. Especially recommend vscode plugin (https://marketplace.visualstudio.com/items?itemName=jroesch.lean#review-details), install it and wait, it should install elan, lean, and lake for you.

Build the project

```shell
lake build
```
