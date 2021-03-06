# Introduction

`gitgraph` is a CLIM-based tool to display and editor graphical representation
of the commit history in s-expression.

![demo](doc/demo.gif)

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

```bash
$ ./build.sh
;; loading system "gitgraph"
$ ./gitgraph
```

# Commands
The supported commands are as follows:

- Open
- Save
- Reload
- Pretty
- Undo (Meta + Left)
- Redo (Meta + Right)
- Zoom in
- Zoom out
- Quit
- commit after
- commit before
- commit delete
- branch new left
- branch new right
- branch rm
- merge

# License

It is distributed under the GNU GENERAL PUBLIC LICENSE Version 3.  See the
accompanying [COPYING](COPYING) file for more details.
