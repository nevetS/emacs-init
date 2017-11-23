Emacs Init
===========
This is a clone-able emacs configuration.

Feature list and documentation to follow.  For now, you can check the
Commentary in the list files.

## Features

### General

*  Separate configuration files for each package so they can easily be removed
   and inspected
*  References git repos of many packages for easy upgrades and trials of
   development-in-progress (simply change the release tag)
*  For packages installed from elpa repos, automatically runs installation if
   package is missing

### Elpa Packages

*  [expand-region](https://github.com/magnars/expand-region.el) (expand selection by C-=)
*  [flycheck](http://www.flycheck.org/en/latest/) Syntax checking across
   multiple languages
*  [jedi](http://tkf.github.io/emacs-jedi/latest/) Python autocompletion for
   emacs
*  [magit](https://magit.vc/) Git integration with emacs
*  [web-beautify](https://github.com/yasuyk/web-beautify) formatting of HTML,
   CSS, Javascript, JSON
*  `yaml-mode` syntax highlighting of yaml files
*  [highlight-parentheses-mode](https://www.emacswiki.org/emacs/HighlightParentheses) highlight
   parens surrounding a given point
   
### Other Packages

*  [autopair]() automatically create paired parens, quotes, etc.
*  [dockerfile-mode]() syntax highlighting for dockerfiles
*  [fci]() fill-column indicator
*  [graphviz]() syntax highlighting for graphviz .dot files
*  [ido]() directory and buffer completion
*  [js2-mode]() syntax highlighting and formatting for javascript
*  [markdown-mode]() syntax highlighting / visual formatting for markdown
*  [org]() org mode 
*  [unbound]() find unbound keys for new functions
*  [window-number]() number emacs windows and navigate between them easily
*  [yasnippet]() Handy dandy templated auto completion

### Personal Configuration

*  When emacs is called from the command line with a file, it will open in
   single-window mode (deleting scratch and messages buffers)
*  no lockfiles
*  integrates highlight-parentheses mode with autopair mode
*  outline minor mode turned on by default
*  starts emacs-server if it is not already started
*  startup screen is disabled

## Trying out this configuration ##

/Applications/Emacs.app/Contents/MacOS/Emacs -q -l ./init.el  --debug-init
