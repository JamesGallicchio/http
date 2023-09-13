# Http definitions for Lean 

Intended to be a base package for handling HTTP traffic.
Modeled after the [rust `http` package](https://crates.io/crates/http).

### Usage

Add the following to your project's `lakefile.lean`:
```
require Http from git "https://github.com/JamesGallicchio/http" @ "main"
```
When running `lake update`, there will be some warnings because one of the
dependencies is not up to date. This should be fixed soon.

### Contributors

Forked from `lurk-lab/Http.lean`.
