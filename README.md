Mpages
======

Mpages is my emacs "mode" for writing a Morning Pages style diary.
A few weeks ago I read some blog post about this meditative type daily practice
called Morning Pages. Its basically a diary but with a different mindset. Instead
of cherishing your thoughts, writing well, or trying to create something of any value
its purpose is to just be a garbage dump for whatever is on your mind. Get it out,
get it on paper, and throw it away.

It should be done first thing in the morning to clear out the cobwebs. Rant, emote, say
whatever. It really doesn't matter and it isn't meant to be read ever again. I barely know
how to write with a pen and paper any more, so I fired up emacs and went to town. The goal
is 3 pages which is about 750 words. It takes about 15 minutes.

A couple weeks later and I'm still doing it. There's something about putting the random
circular thoughts that are preoccupying your mind into complete sentences. When I dwell
in thought, the same thoughts just keep going in circles. But after 15minuts of dumping
it all out. I feel finished. Thoughts have been had. Move on.

I like it so much I decided to whip up some emacs lisp(my first) to make it a little easier 
to open up a new file, and keep track of how much I've written.

Instructions
============

Download the file. Put it wherever you like your elisp files and add

```(load "/path/to/mpages-mode.el")```

to your .emacs file.

Then type ```M-x mp-today``` and a buffer will open up ready for you to type.
The first time you run it, it will prompt you for a directory to put your diary
files. After that you'll get a 
timer for how long you've been typing and a word count in the header. It will turn green when
you've typed enough.

```M-x customize``` will give you a few configurable options in the Text->Mpages group

Learn More
==========

Here are some links:

http://juliacameronlive.com/basic-tools/morning-pages/

http://750words.com/
