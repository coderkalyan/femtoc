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

# Overview of Features
(WIP) This is a list of features that differentiate femto from C and some other well-known systems languages. Many languages implement these subsets of these features, and these aren't necessarily unique to femto. Some of these features are inspired by:
* C
* Zig
* Rust
* Hare
* Go

* variables are immutable by default
* explicit width types only (int/long/short/double => u32/u64/u16/f64)
* no implicit unsafe casts, like integer truncation, sign changes, or float to/from integer
* all datatypes (including arrays) copy by value
* variables cannot be declared without defining (except explicitly with `undefined`)
* `defer` keyword for running teardown code at all exit codepaths
* arrays know their length at compile time
* default "safe" pointers aren't indexable
* slices (implemented using wide pointers) that store length information at runtime
* `yield` for emiting values from a block (treats a long block as an expression)
* u8 slices solve C's null termination issues
* support for interfaces (backed by vtables)
* compile time code execution (eventually)
* improved switch cases with completeness check, ranges, no break needed
* annotations to guide (or enforce) optimizations - inlining, unrolling, linkage
* heap allocation as an API - bring your own allocator, like Zig
* unified loop syntax with `for`
