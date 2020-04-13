# friendly-tramp-path [![MELPA](https://melpa.org/packages/friendly-tramp-path-badge.svg)](https://melpa.org/#/friendly-tramp-path)

An Emacs package that allows a more permissive syntax for TRAMP paths.

It deals with how cumbersome it is for users to have to precise, when prompted for a path, the `/<method>:` prefix even though vars `tramp-default-method` and `tramp-default-method-alist` exist.

It provides function `friendly-tramp-path-dissect` that is a drop-in replacement for `tramp-dissect-file-name`. It behaves the same but allows the following formats:

 - `/<method>:[<user>[%<domain>]@]<host>[%<port>][:<localname>]` (regular TRAMP format)
 - `[<user>[%<domain>]@]<host>[%<port>][:<localname>]` (permissive format)


## Installation

The package is available on [Melpa](https://melpa.org/).

With `use-package`:

```el
(use-package friendly-tramp-path
  :after tramp
  ;; ...
  )
```

Manually:

    M-x package-install friendly-tramp-path


## Example usage

Convert a path in permissive format to a TRAMP VEC:

```el
(let* ((path "pi@raspberry:/home/pi/"))
    (friendly-tramp-path-dissect vec))
```

Convert a path in permissive format back into regular TRAMP format:

```el
(let* ((path "pi@raspberry:/home/pi/")
       (vec (friendly-tramp-path-dissect vec))
       (method (tramp-file-name-method vec))
       (user (tramp-file-name-user vec))
       (domain (tramp-file-name-domain vec))
       (host (tramp-file-name-host vec))
       (port (tramp-file-name-port vec))
       (localname (tramp-file-name-localname vec)))
    (tramp-make-tramp-file-name method user domain host port localname))
```
