Tags History Browsing
=====================

Overview
--------

This package offers functionality to supplement the different tags
operations, usually bound to M-./M-\*.  It currently supports etags.el
and gtags.el, and should be easily extendable to other backends.

As you navigate through a source tree it can become easy to forget how
you got to your current location -- you could call M-\* repeatedly to
pop back up the stack, but this would lose your current location.
This module allows you to view the path taken, and if desired to jump
immediately back to any intermediate location, delete any extranous
locations, etc.

I wrote this for my own use, and my own amusement -- typically I'll
think about writing something for emacs, then have a quick hunt and
find something that already does the job.  This time I followed
through to completion (I was stuck in an airport), but if you're after
an alternative then when I did go looking I found:
[http://www.emacswiki.org/emacs/EtagsStack](http://www.emacswiki.org/emacs/EtagsStack).
I'd like to think that this offers something a little bit different;
it supports gtags as well as etags out of the box and in theory at
least can be extended to others, and it offers a few more operations
on the chronological trace.  Of course, if you need any of that is up
to you :)

The functionality and interface is inspired by browse-kill-ring (and
in fact, I borrowed the navigation code from that package).

Installation and Usage:
-----------------------

Setup:

* clone the repository, or just copy tags-view.el somewhere
* Make sure it is in your path: `(add-to-list load-path checkout-location)`

When you want to use it, make sure that it is loaded
(`(require 'tags-view)` or similar), then call `M-x tv-view-history`.

This will pop open a buffer with your tag locations displayed,
optionally with surrounding lines of context from their source files,
with the most recent at the top.  Type `C-h m` to see what commands
are available, but in summary:

* `n` and `p` navigate down and up the list;
* RETURN closes the history buffer and jumps to that location (without
  alterating the tags stack);
* `o` displays the tag under point in another window, without leaving
  the history buffer (useful to get greater context as you browse);
* `d` will delete the tag under point from the stack (useful if you
  only want to think about a few key functions in the trace).

Customising:
------------

The most likely variable you are going to want to customise is
`tv-context-lines`, which defaults to 0 -- this is the number of lines
of context displayed before and after the actual location of each tag.

The algorithm for determining which of etags, gtags etc is currently
in use is just to start either from the directory of the current
buffer (or `pwd` if no file is associated when you invoke it), then
search upwards until it finds one of TAGS or GTAGS (with preference
for the latter).  You can customise this with your own algorithm
though by setting `tv-determine-backend-function`, which should be a
function of no arguments that returns a symbol denoting the backend to
use (eg, `'etags`), or `'none` if nothing can be determined.

If you want to add another backend you want to look at at the previous
function, and also `tv-backend-list`, whose format is documented in
the variable itself.  Currently this just involves specifying
functions to return the list of data structures (buffer/position) for
the backend, and to delete a stack location if requested.

Bugs:
-----

Bound to be heaps.  Does anyone want to write some tests?
