# violet

A programming language focuses on

- separate compilation
- semantic versioning
- static typing
- typed macro

### Example

> **Warning** The project still in early stage, so everything in this section might not sync with source code

```scm
(: inc : int -> int)
(define (inc n) (+ 1 n))
```

### Develop

You will need to install LLVM, via any package manager would be fine, here is an example for macOS.

```shell
brew install llvm
```

And you might like to install the following racket libraries manually

1. https://github.com/dannypsnl/racket-llvm
2. https://github.com/dannypsnl/reporter
