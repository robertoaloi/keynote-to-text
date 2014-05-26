# Quick Start

'keynote-to-text' is a simple script which converts Apple Keynote files into plain-text files. It requires Erlang to be installed on your machine. Usage:

    $ git clone git@github.com:robertoaloi/keynote-to-text.git
    $ cd keynote-to-text
    $ make
    $ ./keynote-to-text /PATH/TO/SLIDESHOW.key

The output produced by the 'keynote-to-text' script currently contains two sections:

1. The list of resources (e.g. images, videos) embedded in the slideshow
2. A list of "strings" extracted from the slides

## Git Integration

Ever had some Apple Keynote files under your git repository? Wouldn't it be nice if you could diff them to a certain extent? Include the following into your Git configuration file (i.e. `~/.gitconfig`):

    [diff "keynote"]
      binary = true
      textconv = /PATH/TO/KEYNOTE/keynote-to-text

Then, edit the Git attributes file as follows (e.g. `/PATH/TO/PROJECT/.gitattributes`):

    *.key diff=keynote

Now you can get diffs from your Apple Keynote files:

    git diff awesome_slideshow.key

Some sample output from `git diff` is shown below. You can notice how I have removed an image, edited some text and made a non-printable change on slide 82.

![Git Diff Sample Output](/screenshots/riak-diffs.png "Git Diff Sample Output")

Clone it, try it, love it.

## Compatibility Notes

The script is not compatible with Apple Keynote 6 (yet). Pull requests are welcome.
