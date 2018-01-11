# Gerbil Utils

A collection of utilities for Gerbil Scheme.
This collection homesteads the toplevel name "utils" in the global package namespace.
It is supposed to host utilities that, when complete, well-designed and well-documented,
are headed for src/std/ but are not there yet (and for some of them may never be).

## Using

Clone the repo and build the utils using the build script:
```
$ git clone https://github.com/fare/gerbil-utils.git
$ cd gerbil-utils
$ ./build.ss deps && ./build.ss

# if you want to build emacs TAGS
$ ./build.ss tags
```

If you are using NixOS or nixpkgs, see also the file `gerbil-nix-env.sh`
for interactive development of Gerbil software.

## Contributing

Contribution by all Gerbil hackers is welcome, as the current curator though,
I may insist that all code should follow a style and naming conventions that are
consistent with the existing body of code, or at least with themselves
(which weaker requirement only applies for code that doesn't interact strongly
with the rest of the codebase).

The APIs of functions in this collection should not be considered stable unless otherwise noted;
still, efforts should be made to not *gratuitously* introduce backward incompatibility.
When the APIs are stable, the functions and macros will be moved to the standard library,
under the prefix `std/` instead of `utils/` --- though slightly different style standards may
apply there, as vyzo is the curator and may have different tastes.
