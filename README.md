[![Build Status](https://travis-ci.com/jcs090218/project-sln.svg?branch=master)](https://travis-ci.com/jcs090218/project-sln)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [project-sln](#project-sln)
    - [How to further develop for your favorite language?](#how-to-further-develop-for-your-favorite-language)
    - [Supported Langauges](#supported-langauges)
    - [Todo List](#todo-list)
    - [Contribution](#contribution)

<!-- markdown-toc end -->


# project-sln
> Project structure organizer.

The goal of this package is to generate project cache file that holds information
of the current project. Using this cache file can do the following.

* Code Navigation
* Auto completion
* Tooltip

This package does not rely on any Tag System or Any Searcher that gives you all
the possible results. This package should generate rule that could highly reliable
on.


## Supported Langauges

* C#


## How to further develop for your favorite language?

If you are willing to implement a programming language for this package.

#### 1. You need a targeted language parser (To identify tokens).

I'm currently using [parse-it](https://github.com/jcs-elpa/parse-it)
and have all the needed keys store inside `project-sln-mode-extension`
variable. You can use any parser you want as long it gives you the AST to
indentify the token you need to indentify.

#### 2. You need to know JSON. Cache file is output in JSON using pure elisp.

You can read [json.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/json.el)
built inside Emacs. If you are confused about the output of JSON format, please
check [_json](./_json/) directory for examples.


## Todo List

- [ ] Reliable code navigation.
- [ ] `company-sln` or `company-project-sln`, a company plugin that uses
this plugin to do auto-completion.
- [ ] Support as much programming languages as possible.


## Contribution
If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
