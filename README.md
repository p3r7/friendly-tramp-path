# friendly-tramp-path

An Emacs package that allows a more permissive syntax for TRAMP paths.

It deals with how cumbersome it is for users to have to precise, when prompted for a path, the `/<method>:` prefix even though vars `tramp-default-method` and `tramp-default-method-alist` exist.

It provides function `friendly-tramp-path-disect` that is a drop-in replacement for `tramp-dissect-file-name`. It behaves the same but allows the following formats:

 - `/<method>:[<user>[%<domain>]@]<host>[%<port>][:<localname>]` (regular TRAMP format)
 - `[<user>[%<domain>]@]<host>[%<port>][:<localname>]` (permissive format)


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

To require everything:

```el
(use-package friendly-tramp-path
  :quelpa (friendly-tramp-path :fetcher github :repo "p3r7/friendly-tramp-path"
  :after tramp))
```

## Example usage

Convert a path in permissive format to a TRAMP VEC:

```el
(let* ((path "pi@raspberry:/home/pi/"))
    (friendly-tramp-path-disect vec))
```

Convert a path in permissive format back into regular TRAMP format:

```el
(let* ((path "pi@raspberry:/home/pi/")
       (vec (friendly-tramp-path-disect vec))
       (method (tramp-file-name-method vec))
       (user (tramp-file-name-user vec))
       (domain (tramp-file-name-domain vec))
       (host (tramp-file-name-host vec))
       (port (tramp-file-name-port vec))
       (localname (tramp-file-name-localname vec)))
    (tramp-make-tramp-file-name method user domain host port localname))
```
