# Lingo

A simple LLM wrapper with few convenience commands for language learning.

## Prerequisites

- [ollama](https://ollama.com/) is installed and running locally (e.g. as a service or via `ollama serve` in separate terminal)
- appropriate ollama model (e.g. `gemma3`) is pulled locally (e.g. `ollama pull gemma3`)

## Installation

Run `cabal install` in the root of the repository.
This should make `lingo` binary available on your `PATH`.
The project uses recent GHC features, so it assumes GHC >= 9.12.2.


## Usage

lingo works as a REPL supporting few commands on top of plain LLM interactions

```
$ lingo # starts a REPL
en> # prompt signifies the output language
en> :help
Available commands:
  :d, :define <string>               generate a definition of the word/expression <string>
  :e, :example <string>              generate an example sentence using the word/expression <string>
  :h, :help                          show this help message
  :q, :quit                          exit the REPL
  :set lang <language code>          set the output language (en, pt, de, cs)
  :set model                         interactive model selection
  :t, :translate [inlang] <string>   translate <string> to output language - specify input language to disambiguate
  <RAW_PROMPT>                       send prompt to the underlying LLM as is
```