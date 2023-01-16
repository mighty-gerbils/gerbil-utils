# Gerbil Clan

A collection of utilities for [Gerbil Scheme](http://cons.io/).
This collection homesteads the toplevel name `clan` in the global package namespace.
It is intended to host utilities that, if and when complete, well-designed and well-documented,
are headed for the `std/` hierarchy of standard libraries,
but are not there yet (and for some of them may never be).
Maintenance of the utilities is collegial, with `fare` as current lead maintainer.
All are welcome to join the effort,
as long as they are ready to uphold high standards of quality as they collaborate
on the joint code base;
and even those who do not have the time and dedication to be maintainers
can contribute code, documentation, and suggestions.

## Using

You can install using the Gerbil package manager:

```
gxpkg install github.com/fare/gerbil-utils
```

Alternatively, you can clone the repo and build the utils using the build script:
```
$ git clone https://github.com/fare/gerbil-utils.git
$ cd gerbil-utils
$ gxpkg link github.com/fare/gerbil-utils $PWD
$ ./build.ss
```

If you are using NixOS or nixpkgs, see also the file `gerbil-nix-env.sh`
for interactive development of Gerbil software.

## Contributing

Contribution by all Gerbil hackers is welcome, as the current curator though,
I may insist that all code should follow a style and naming conventions that are
consistent with the existing body of code, or at least with themselves
(which weaker requirement only applies for code that doesn't interact strongly
with the rest of the codebase).

## Compatibility

The APIs of functions in this collection should *not* be considered stable unless otherwise noted;
still, efforts are being made to not *gratuitously* introduce backward incompatibility.
When the APIs are stable, the functions and macros will be moved to the standard library,
under the prefix `std/` instead of `clan/` --- though slightly different style standards may
apply there, as vyzo is the curator and may have different tastes.

That said, current version of `gerbil-utils` requires Gerbil v0.17 or later to work.

## Testing

Here are the coding conventions for the tests in projects that use `clan/testing`:
  - All test files are under the `t/` subdirectory, with a name generally the same
    as the file being tested, e.g. `foo-test.ss` to test the functionality in `foo.ss`.
  - A test file name ends in `-test.ss` for unit-tests, or `-integrationtest.ss` for integration tests.
  - A test file defines a single `test-suite` with the same name as the file,
    e.g. `t/foo-test.ss` has
    `(def foo-test (test-suite "testing foo" (test-case "test case 1" ...) ...))`
  - A unit-test is a test that can run without any external service (e.g. geth)
  - An integrationtest is a test that requires some external services to run against (e.g. geth)
  - if there are dependencies between tests, number them in a total order
    that extends the dependency partial order, so we don't spend time debugging something
    that depends on something else that already failed. e.g. `20-bar-integrationtest.ss`
    may depend on helpers or service connections defined in `10-foo-integrationtest.ss`.
  - Your project can use the same convention by defining a file `unit-tests.ss` similar
    to that in this directory.
  - See [gerbil-ethereum](https://github.com/fare/gerbil-ethereum)
    for a more elaborate example of a project using this test infrastructure.
