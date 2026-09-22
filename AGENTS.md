# Cat Exit Watchdog Agent Guide

## Scope

This repository is an Emacs package. Keep the package usable independently of Cat Emacs.

## Compatibility

- Support Emacs 29.1 and later.
- Keep the Emacs Lisp watchdog functional on every platform.
- Treat native stack capture as optional enhancement selected by `system-type` and executable availability.
- Preserve `save-buffers-kill-emacs` semantics; diagnostics must never skip save, process, Custom, or exit confirmation.
- Avoid mandatory external package dependencies.

## Verification

For Emacs Lisp changes, run:

1. `check-parens` on every modified Lisp file.
2. Byte compilation with warnings treated as errors.
3. A short-return check proving the helper is canceled.
4. A controlled-delay check proving an Elisp log is generated; where a native debugger is available, verify its log too.
5. `git diff --check`.
