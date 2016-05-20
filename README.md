# clasp

A LISP built in Scala to learn some basic programming language theory. Simple
interpreted language with basic features (ie addition, string stuff, if
statements, etc.)

### Building

For development purposes, simply use `sbt` within the directory and use it
normally.

For deployment, use `sbt install` and `sbt uninstall`. It'll install a bash
script named `clasp` into `/usr/local/bin` that executes the `clasp.jar` within
the project's directory.

Simply run the language by calling `clasp` in your terminal. For help call
`clasp help`.

### Editing

* [Vim](http://github.com/crockeo/clasp.vim)
* Other Editors: Make your own! (Sorry)

### Licensing

Refer to `LICENSE`.
