# friendly-tramp-path

An Emacs package that allows a more permissive syntax for TRAMP paths.

## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

To require everything:

```el
(use-package friendly-tramp-path
  :quelpa (friendly-tramp-path :fetcher github :repo "p3r7/friendly-tramp-path"
  :after tramp))
```
