# eslintd-fix

[![MELPA](https://melpa.org/packages/eslintd-fix-badge.svg)](https://melpa.org/#/eslintd-fix)
[![MELPA Stable](https://stable.melpa.org/packages/eslintd-fix-badge.svg)](https://stable.melpa.org/#/eslintd-fix)

Minor-mode to automatically fix javascript with [eslint_d][]. Built with a focus
on speed, you will typically barely notice a delay when saving, if at all. 

This package was recently rewritten to use a direct network connection to
[eslint_d][] rather than a node shell command. This is significantly faster, but
could have introduced bugs. If you notice anything out of the ordinary, please
report it.

## Installation

You can install this package from [Melpa][]

```
M-x package-install RET eslintd-fix RET
```

## Usage

Ensure that you have [eslint_d][] `5.2.0+` installed and in your path.

Then, in your `init.el`:

```elisp
(add-hook 'js2-mode-hook 'eslintd-fix-mode)
```

## Options

You can change the location of eslint_d. 

```elisp
(setq eslintd-fix-executable "/my/path/eslint_d")
```

See `M-x customize-group eslintd-fix` for more.

## Thanks

* [@mantoni][] for [eslint_d][].

[eslint_d]: https://github.com/mantoni/eslint_d.js
[@mantoni]: https://github.com/mantoni
[Melpa]: http://melpa.milkbox.net/
