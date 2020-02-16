# formallang

[![This project is considered experimental](https://img.shields.io/badge/status-experimental-critical.svg)](https://benknoble.github.io/status/experimental/)

A formal-languages library for Clojure.

## Features

- Deterministic Finite Automata (DFA)

**Coming soon**
- Regular expressions (via NFA)
- Context-free grammars

## Getting Started

deps.edn:

```clojure
{:deps
 {github-benknoble/formallang
  {:git/url "https://github.com/benknoble/formallang"
   :tag "v1.0.0"}}}
```

Then execute `clojure -Sresolve-tags` to update the `:sha` from `:tag`.

---

Originally built to support a regular-expression/context-free grammar
counter-example system (codename: wrongex, **coming soon**). See also [CS:
Generating property-based
counter-examples](https://cs.stackexchange.com/q/119661/61762)
