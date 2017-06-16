Installing Pymacs and Ropemacs
==============

ropemacs is a powerful tool, but unfortunately it requires
[Pymacs](https://github.com/pinard/Pymacs), which seems to haphazardly have
installation instructions available.

I found via the Fork network, a repository which has an installation script.
The installation script as of this commit still points to the pinard v0.25
release, which I take to mean that the other commits in this repo are not yet
ready for prime time.

To install Pymacs and rope, run the following:

``` bash
    > chmod +x install-pymacs.sh
    > ./install-pymacs.sh
	
```

Once that is done, you'll want to go into the plugins directory and edit the
Makefile to include the appropriate python binary (e.g. python2.7) and run `make`
in order to create pymacs.el

This forked repository does have an updated readme with installation instructions and
links to the most recent Pymacs documentation on wwwarchive.
	
