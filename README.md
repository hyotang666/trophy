# TROPHY 0.10.1
## What is this?
Gamificated common lisp environment.

## Gamification.
[Gamification](https://en.wikipedia.org/wiki/Gamification).

## Usage

### REPL
Trophy has its own REPL.

```lisp
* (trophy:repl :your-name)
```

At first time, S-exp above invokes debugger because trophy does not know you.
Once restart CONTINUE is spacified to make new user,
your name and status is saved.

To quit trophy REPL, input `:q` in the trophy REPL.

```lisp
TROPHY> :q

*
```
### SET-ENV
Trophy REPL understand only input form.
This means trophy never understand lisp file which are loaded.

If you want trophy to understand lisp file, `SET-ENV` may what you want rather than REPL.

```lisp
* (trophy:set-env :your-name)
```

SET-ENV never save your status.
To save status, do like below.

```lisp
* (trophy:set-env nil)
```

## From developer

### Product's goal
?

### License
MIT

### Developed with
SBCL

### Tested with

## Installation

To install, [roswell](https://github.com/roswell/roswell) is strongly recommended.

### Requirements
#### libncursesw

```shell
sudo apt-get install libncursesw5-dev
```

Author uses libncursesw5-dev, but libncursesw6-dev will work fine too.

#### lem

```shell
ros install cxxxr/lem
```
### finally

```
ros install hyotang666/trophy
```

### load

To load, [quicklisp](https://www.quicklisp.org/beta/) is strongly recommended.
If you install roswell, quicklisp is already setup.

From shell.

```shell
ros run
```

then

```lisp
* (ql:quickload :trophy)
```
