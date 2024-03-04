# Forge
![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/tnelson/Forge?label=version)
![GitHub issues](https://img.shields.io/github/issues/tnelson/Forge)
![GitHub Workflow Status (branch)](https://img.shields.io/github/actions/workflow/status/tnelson/Forge/continuousIntegration.yml?branch=main)



Welcome to Forge! Forge is a tool and language built for **teaching** introductory formal methods and modeling.

Forge is heavily adapted from the excellent [Alloy](https://alloytools.org/), a more widely used and somewhat more scalable tool. Forge and Alloy even use the same engines! Forge adds a number of features designed to support teaching and learning, including:
- language levels, including froglet (`#lang froglet`, previously `#lang forge/bsl`) for starting out without using relational operators;
- partial instance bounds (`inst`) for optimization and testing;
- ease of scriptability (Forge is a Racket library as well as a language, and has a REPL); and
- interaction modes designed for use in the classroom and on assignments.

Forge also uses [Sterling](https://sterling-js.github.io/), an expanded and scriptable visualizer.

## Documentation

The [Forge documentation page](https://csci1710.github.io/forge-documentation/) is the best resource for Forge syntax, etc.; the Wiki on this repo may not be up to date. 
Note that the documentation is a living document and work in progress; we will be updating it live as students and other users ask questions.

For broader historical context on Forge, the [Alloy documentation
](http://alloytools.org/documentation.html) or an [Alloy demo](http://alloytools.org/tutorials/day-course/) may be useful. We don't suggest using these as reference for Forge itself, however.

## Contributing
To contribute, fork the repository (or open a new branch if a core contributor) and make your changes. When making a pull request back into main, please prepend the pull request title with one of `[patch]`, `[minor]`, or `[major]` so that the corresponding version number is updated. If no version update is specified, the minor version will be updated automatically.


## Installation from source
To install from source, use the following commands. Make sure that you have [Racket](https://racket-lang.org/) installed (any version should work, although we suggest the latest: 8.3), and the `raco` package manager is in your path. You will also need Java installed (we suggest version 11 or later). 

```
git clone https://github.com/tnelson/Forge
cd Forge
raco pkg install ./forge ./froglet
```

To update, `git pull` the repo and run `raco setup forge` to recompile.

Running on the development branch, `dev`, requires `checkout dev` before installing; this may be unstable, however.
