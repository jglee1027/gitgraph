# Introduction

`gitgraph` is a CLIM-based tool to display and editor graphical representation
of the commit history in s-expression.

# QuickStart

## Download

```bash
$ cd ~/quicklisp/local-projects
~/quicklisp/local-projects$ git clone http://github.com/jglee1027/gitgraph.git
```

## Run

```lisp
CL-USER> (ql:quickload :gitgraph)
CL-USER> (gitgraph:gitgraph)
```

## Build

# Commands
The supported commands are as follows:

- Open
- Save
- Reload
- Undo (Meta + Left)
- Redo (Meta + Right)
- Zoom in
- Quit
- commit before
- commit after
- commit delete
- branch new left
- branch new right
- branch rm

# License

It is distributed under the GNU GENERAL PUBLIC LICENSE Version 3.  See the
accompanying [COPYING](COPYING) file for more details.
