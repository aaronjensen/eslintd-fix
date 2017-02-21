# eslintd-fix [![MELPA](https://melpa.org/packages/eslintd-fix-badge.svg)](https://melpa.org/#/eslintd-fix)

Minor-mode to automatically fix javascript with [eslint_d][].

## Installation

You can install this package from [Melpa][]

```
M-x package-install RET eslintd-fix RET
```

## Usage

Ensure that you have [eslint_d][] installed and in your path.

Then, in your `init.el`:

```elisp
(add-hook 'js2-mode-hook 'eslintd-fix-mode)
```

## Options

You can change the location of eslint_d. 

```elisp
(setq eslintd-fix-executable "/my/path/eslint_d")
```

## Thanks

* [@mantoni][] for [eslint_d][].

[eslint_d]: https://github.com/mantoni/eslint_d.js
[@mantoni]: https://github.com/mantoni
[Melpa]: http://melpa.milkbox.net/
