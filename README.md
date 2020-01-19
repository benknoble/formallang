# formallang

[![This project is considered experimental](https://img.shields.io/badge/status-experimental-critical.svg)](https://benknoble.github.io/status/experimental/)

A formal-languages library for Clojure.

## Features

- Regular expressions
- Deterministic Finite Automata (DFA)
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
