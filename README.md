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

### Licensing

Refer to `LICENSE`.
