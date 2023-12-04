# Femto Language Compiler

Welcome! You've found the reference (and only) compiler for the Femto programming language. Femto is a small systems programming language, especially targeting embedded microcontrollers and other smaller platforms. Some of Femto's key goals include:

* Strong ABI compatibility and interoperability with C. We don't just work with C, we love it!
* Safety where possible, *without* a strict demand at the language level (i.e. Rust)
* Avoid implicit or non-obvious behavior
* Simplicity. Femto is not an object oriented programming language.
* Femto aims to generate straight forward, easy to understand machine code where possible.
* Include a formal language specification (might not happen for some time)
* Language stability, after an initial period of development
* Tooling for facilitating interoperability with C and C++ via libclang.

Femtoc is implemented in the [Zig](https://ziglang.org) programming language. It is currently very early in development, and is unlikely to be useful, at least without significant hand-holding from a C driver. Nonetheless, feel free to check it out, or contribute! Transitioning away from Zig to a self-hosted compiler is not on the roadmap but may happen at some point (or not). Currently femtoc depends on LLVM as a compiler backend. While this is the primary focus and will be for some time, a custom backend is planned. This will serve as an educational project, and target niche architectures (Z80, 8086, 68K, 6502, RV32I, etc). We might also support [QBE](https://c9x.me/compile/) in the future.

Very very long term goals include:
* De-facto interoperability with a subset of C++ via the [Itanium C++ ABI](https://itanium-cxx-abi.github.io/cxx-abi/abi.html)
* Formal verification for range checks, nullability, and algorithm verification (correctness and complexity) via Z3/CVC5/other theorem provers.

Some other important links:
[tree-sitter-femto](https://github.com/coderkalyan/tree-sitter-femto): Tree sitter grammar for femto. This should help you with your neovim experience.
Once there's a standard library and language server, they will be linked here.
