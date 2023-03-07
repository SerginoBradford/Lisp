## DESCRIPTION

> The goal of this project is to implement an interpreter for a minimalist dialect of LISP in Haskell.

## PREREQUISITES
What do you need to install ?
```bash
stack
make
```

## HOW TO BUILD
Clone and go into `lisp` directory.
Then,
```bash
$ make
```

## HOW TO RUN TEST
Clone and go into `lisp` directory.
Then,
```bash
$ make tests_run
```


## HOW TO RUN
Clone and go into `lisp` directory.
Then,
```bash
$ ./lisp
```
You can now enter your command into the REPL Mode. If your commands are into a file, just enter it name into the command line. Entry should be between parentheses, except for file name.

## FEATURES
The program take a list of files as command line arguments and interpret them sequentially. Symbols defined in a file will still be define in subsequent files.
This interpreter support the following types: signed integers, symbols, list.
The builtins commands are: if, +, -, mod, div, *, quote, lambda, define. It also handle a repl mode for interactive commands.

---

<div align="center">

<a href="https://github.com/SerginoBradford" target="_blank"><img src="https://cdn.jsdelivr.net/npm/simple-icons@3.0.1/icons/github.svg" alt="github.com" width="30"></a>

</div>