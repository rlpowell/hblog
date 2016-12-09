Robin Lee Powell's Hakyll based blog.
[Hakyll](https://github.com/jaspervdj/hakyll) is a
static website generator library written in Haskell.  All the
non-trivial HBlog code is in Haskell.

It has the following novel-compared-to-normal-Hakyll features:

- Wiki-style markdown links (i.e. [my other post](I Love Red) works)
  - Extra support for wiki-style links that are easy to enter on a
    touch keyboard;  "qwl my other post qwu I Love Red qw" is the same
    as the example above.  This looks terrible, but on a swipe
    keyboard you can trivially teach these "words", and since nothing
    starts with q followed by another consonant, they should be very
    easy to swipe.  This support is handled by a preprocessor
    program called "unphone".
  - Extra support for fuzzy wiki links, i.e. [my other post](Red) is
    equivalent to the above unless there are two posts with "Red" in
    the title.  This support is handled by a preprocessor
    program called "rectifier". 
- Support for initial both publication dates and last modified dates
  - Dates are retrieved from git by default; the original
    publication date can be overriden in the header
  - Sorting is normally by last modified date, but both are shown
- This repo also has a TikiWiki-to-Markdown converter, using Pandoc;
  nothing to do with Hakyll, or even with the blog really, but most
  of my extant content is in TikiWiki, so...
