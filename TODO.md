# Todo

how to test getting a variable before starting?



maybe makunbound could get rid of it?
 to test again
 then just do the read variable first?


make sure directory exists and if not
 create it recursively





should customizable morning pages directory
should ask the first time its run to set that


should make sure that directory exists before finding the file
 test with a broken directory to see what happens


not sure if it should be a mode or not
if not a mode then remove references to "mode"


prefix all functions and variables with with mpages
 and get rid of the mode stuff


create readme to explain what it is


# Future
zoom font default +1

could check for ido and do ido read-directory-name

some testing just to figure out how to do it with emacs

create a cask package

blog post on the process

disappearing text (make it feel fading away)

could make time only count while in the buffer, currently
it just records start time, so if I step away to another
buffer it will keep the timer going. But really it should only
count while in that buffer. Timer tick should just add a second
and that count should get converted into a time and formatted


could check to see if running timer is the same as the buffer local
version. Not sure if that is possible with the timer method call
but that way it could restrict to the exact buffer that started the mode
and not if multiples are defined (would go well with counter above)
