Mpages
======

Mpages is my emacs "mode" for writing a Morning Pages style diary.
A few weeks ago I read a blog post about this meditative type daily practice
called Morning Pages. Its essentially a diary but with a different mindset. Instead
of cherishing your thoughts, writing well, or trying to create something of any value
its sole purpose is to act as a garbage dump for anything on your mind. Get it out,
get it on "paper", and throw it away.

It should be done first thing in the morning to clear out the cobwebs. Rant, emote, say
whatever. It really doesn't matter and it isn't meant to be read ever again. I barely know
how to write with a pen and paper any more, so I fired up emacs and went to town. The goal
is 3 pages which is about 750 words. It takes about 15 minutes.

It only took a few days before I was hooked. There's something about putting the random
circular thoughts that are preoccupying your mind into complete sentences. When I dwell
in thought, they just keep going in circles. But after 15 minutes of dumping
it all out. I feel finished. Thoughts have been had. I move on.

I think at the end of the day we all want to talk about ourselves, share our feelings, dwell on
our concerns, and revel in our victories big or small. The problem is nobody else really cares
all that much. Except for Emacs. Emacs loves you and will listen to you blather on as long as you
like.

I liked the activity so much I decided to whip up some emacs lisp(my first) to make it a little easier 
to do it daily. Give it a try!

Instructions
============

Preferred method of installation is through MELPA and the package system. Search for ```mpages```.

Otherwise download the file. Put it wherever you like your elisp files and add

```(load "/path/to/mpages.el")```

to your .emacs file.

Then type ```M-x mpages-today``` and a buffer will open up ready for you to type.
The first time you run it, it will prompt you for a directory to put your diary
files. After that you'll get a 
timer for how long you've been typing and a word count in the header. It will turn green when
you've typed enough.

```M-x customize``` will give you a few configurable options in the Text->Mpages group

Learn More
==========

Here are some links that talk more about the Morning Pages concept:

http://juliacameronlive.com/basic-tools/morning-pages/

http://750words.com/
