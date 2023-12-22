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

### Contributing

If you would like to contribute, please open a PR!
Make sure to add yourself to the authors list below,
in case we ever need to contact the copyright holders.

### Authors

[JamesGallicchio](https://github.com/JamesGallicchio)

Originally forked from [`lurk-lab/Http.lean`](https://github.com/lurk-lab/Http.lean).
Although all of the source material has been rewritten,
the authors of that library at the time of the fork included:

[Anderssorby](https://github.com/Anderssorby)
[arthurpaolino](https://github.com/arthurpaolino)
[xubaiw](https://github.com/xubaiw)
