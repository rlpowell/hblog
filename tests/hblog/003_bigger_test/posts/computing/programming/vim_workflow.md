---
origdate: '14 Nov 2012 (or earlier)'
published: '2012-11-14'
tags: 'programming, sysadminning'
title: My Vim Workflow
---

Notes-to-self on how I use Vim for more serious tasks (i.e. programming)
because I sometimes go a long time without doing any serious coding and
I forget these things.

This is basically an ordered list; given a fresh vim in which you want
to start serious coding, start at the top here and try it, moving down
until you have the behaviour you want.

-   <space>ff brings up fuzzyfinder in file mode; type any subset of the
    file's name, enter to open in new tab
-   FuzzyFinder normally matches *only* on the basename; to match on a
    (portion of a) directory, enter some text and then /, and which
    point you're matching on only the basename again
-   <space>fb brings up fuzzyfinder in buffer mode; switches to an open
    tab with that buffer if it can
-   &lt; and &gt; move between tabs
-   <space>\[123...\] switch to particular tabs, by the numbers in the
    tab bar
-   <space><space> to switch to the last tab
-   <space>q closes the quicklist, i.e. what Ack uses
-   <space>Q closes the current tab
-   <space>d closes the current buffer (i.e. takes that file out of
    vim's purview)
-   <space>\[hjkl\] changes splits in the current tab
-   <space>n makes a new, empty tab
-   <space>s splits
-   <space>s or " , type some of file name, hit tab, enter: switch to
    tab with that name

Various Misc Tools

-   <space>a opens a new tab to run Ack
-   <space>A does Ack -a instead
-   <space>T launches Tabularize; best used in visual mode, defaults to
    aligning by commas
-   <space>t launches Tagbar, a browser for functions and such

Git Checkins (vim-fugitive)

-   <space>gd takes you straight to the diff against the staged version
-   dp/do puts or gets (obtains) a diff segment between the files; in
    visual mode, needs a space first
-   writing the staged version (on the left) stages the changes selected
-   <space>gc commits
-   <space>gs opens :Gstatus, see :help fugitive for detals of the keys,
    but basically D to diff the file against the staged version, cc to
    commit
