# Forge
![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/tnelson/Forge?label=version)
![GitHub issues](https://img.shields.io/github/issues/tnelson/Forge)
![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/tnelson/Forge/Continuous%20Integration/master?label=tests)

Welcome to Forge! Forge is a tool and language built for **teaching** introductory formal methods and modeling.

Forge is heavily adapted from the excellent [Alloy](https://alloytools.org/), a more professional and somewhat more scalable tool. Forge and Alloy even use the same engines! Forge adds a number of features designed to support teaching and learning, including:
- slightly restricted and somewhat modified syntax;
- integration of [Electrum-style](https://haslab.github.io/Electrum/) support for model checking;
- ease of scriptability (Forge is a Racket library as well as a language, and has a REPL); and
- interaction modes designed for use in the classroom and on assignments.

## Documentation


- [Official Forge documentation (work in progress; living document)](https://github.com/tnelson/Forge/wiki)

- [Alloy documentation
](http://alloytools.org/documentation.html)
- [A quick reference guide to Alloy operators](http://www.ics.uci.edu/~alspaugh/cls/shr/alloy.html) (most of which Forge shares)
- [A guide to syntax and basic examples in Alloy](http://alloytools.org/tutorials/day-course/)

## Contributing
To contribute, fork the repository (or open a new branch if a core contributor) and make your changes. When making a pull request back into master, please prepend the pull request title with one of `[patch]`, `[minor]`, or `[major]` so that the corresponding version number is updated. If no version update is specified, the minor version will be updated automatically.


## Installation from source
To install from source, use the following commands. Make sure that you have [Racket](https://racket-lang.org/) 7.9 (exactly) along with the raco package manager. You will also need Java 8.
```
git clone https://github.com/tnelson/Forge
cd Forge/forge
raco pkg install
```
Running on the development branch, `dev`, is possible but not recommended for most users.