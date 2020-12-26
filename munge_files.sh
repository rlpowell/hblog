#!/bin/bash

set -e
# set -x

export PATH="$HOME/bin:$HOME/.local/bin:$PATH:/usr/local/bin/:/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin"

# rectifier is fundamentally whole-tree based; it needs to know
# about everybody to know what things are available to link to.
#
# Similarly, unphone sort of needs to run everywhere to make sure
# everything is ready for rectifier.
#
# So, run them everywhere into a new tree, then walk the files in
# the trees for diffs.  If a diff is found, copy, check in with the
# original file's timestamp.

indir="$1"
if [ ! -d "$indir" ]
then
  echo "First argument must be the source directory."
  exit 1
fi
# Make sure it ends in a / ; the git log "#" based substitution
# stuff needs it
indir="${indir%/}/"

outdir="$indir"
tempdir="/tmp/munge_files.$$"
origpwd=$(pwd)

testing=''
if [ -d "$2" ]
then
  echo "Second argument detected; entering testing mode."
  testing="true"
  outdir="$2"
  rsync -a "$indir/" "$outdir/"
  cd "$outdir"
  git init .
  git add .
  git commit -m "Initial checkin" -a
  echo "Sleeping to help with sync."
  sleep 10
  cd $origpwd
fi

MAILX=mailx
if [ "$testing" ]
then
  MAILX="echo mailx --"
fi

mkdir -p $tempdir

snipdir () {
  fname="$1"
  dir="$2"
  echo "$fname" | sed -r "s|^$dir/*||"
}

echo "Checking and pandoc-ing every file; this may take a while."
find $indir -type f -name '*.md' | sort | while read fname
do
  if [[ $fname =~ 'conflicted copy' ]]
  then
    echo "Dropbox conflicted file: $fname; bailing"
    echo "Dropbox conflicted file: $fname" | $MAILX -v -S smtp=mail -S hostname=hblog-container -r rlpowell@digitalkingdom.org -s 'CONFLICTED FILE Found: Dropbox' rlpowell@digitalkingdom.org
    exit 1
  fi

  if [[ $fname =~ 'sync-conflict' ]]
  then
    echo "SyncThing conflicted file: $fname; bailing"
    echo "SyncThing conflicted file: $fname" | $MAILX -v -S smtp=mail -S hostname=hblog-container -r rlpowell@digitalkingdom.org -s 'CONFLICTED FILE Found: Syncthing' rlpowell@digitalkingdom.org
    exit 1
  fi

  short=$(snipdir "$fname" "$indir")
  mkdir -p $(dirname "$tempdir/$short")

  # Before you make changes to any of the data handling here, read
  # and update the "Dates" section in DESIGN-CODE.  This stuff is
  # complicated.
  filedate="$(date --iso-8601=seconds -r "$fname")"
  cd "$(dirname $fname)"

  # Make sure everything is checked in and updated
  if [ ! "$(git ls-files "$(basename $fname)")" ]
  then
    # Never been checked in before
    echo "Found new file $fname"
    git add "$(basename $fname)"
    cat $(basename $fname) | $MAILX -v -S smtp=mail -S hostname=hblog-container -r rlpowell@digitalkingdom.org -s "NEW FILE: hblog automation: initial checkin: \"$fname\"" rlpowell@digitalkingdom.org
    # Before you make changes to any of the data handling here, read
    # and update the "Dates" section in DESIGN-CODE.  This stuff is
    # complicated.
    git commit -m "Automated initial checkin of $fname at $(date) ; file date is $filedate" \
      --date="$filedate" \
      "$(basename $fname)"
    echo "Sleeping to help with sync."
    sleep 10
  elif ! git diff --quiet "$(basename $fname)"
  then
    # Needs changes checked in
    echo "Found changes to $fname"
    GIT_EXTERNAL_DIFF=/tmp/hbgwdiff.sh git --no-pager diff "$(basename $fname)"
    GIT_EXTERNAL_DIFF=/tmp/hbgwdiff.sh git --no-pager diff "$(basename $fname)" | $MAILX -v -S smtp=mail -S hostname=hblog-container -r rlpowell@digitalkingdom.org -s "FILE CHANGES: hblog automation: external changes: \"$fname\"" rlpowell@digitalkingdom.org
    # Before you make changes to any of the data handling here, read
    # and update the "Dates" section in DESIGN-CODE.  This stuff is
    # complicated.
    git commit -m "Automated checkin of external changes to $fname at $(date) ; file date is $filedate" \
      --date="$filedate" \
      "$(basename $fname)"
    echo "Sleeping to help with sync."
    sleep 10
  fi

  cd "$origpwd"
  touch -d "$filedate" "$fname"

  mkdir -p ~/.hblog-cache/
  cache_file=~/.hblog-cache/$(echo $fname | sed 's;/;_;g')
  if [ "$testing" ] || [ ! -f $cache_file ] || ! diff -q $fname $cache_file >/dev/null 2>&1
  then
    echo "File $fname does not match its cached version; running pandoc on it to normalize its syntax."
    pandoc -s -f markdown -t markdown --wrap=preserve -o $tempdir/$short $fname
    # Pandoc likes to turn
    #   [test](ched: The Chunk Editor)
    # into
    #   [test](ched:%20The%20Chunk%20Editor)
    # , and screw that.  Also Rectifier puts it back.  Haven't
    # figured out how that even works, but it leads to bullshit
    # checkins, so we fix it.
    sed -i '/[]](/s/%20/ /g' "$tempdir/$short"
    cp "$fname" "$cache_file"
    touch -d "$filedate" "$fname" "$cache_file" "$tempdir/$short"
  else
    echo "File $fname matches its cached version; not updating."
  fi
done

checkin () {
  new_file="$1"
  orig_file="$2"
  type="$3"

  if [ ! -f "$orig_file" ]
  then
    echo "The file $orig_file doesn't exist, but I copied everything to that area earlier; bailing."
    exit 1
  elif ! diff -q "$new_file" "$orig_file" >/dev/null 2>&1
  then
    echo "Found $type changes to $orig_file"

    # Divide the old size by the new size; if that number is large,
    # then the new file is much smaller; freak out.
    size_ratio=$(($(stat -c %s $orig_file) / $(stat -c %s $new_file)))
    if [ "$size_ratio" -gt 3 ]
    then
      echo "Massive file shrink: $orig_file has gone from $(stat -c %s $orig_file) bytes to $(stat -c %s $new_file) bytes; bailing."
      echo "Massive file shrink: $orig_file has gone from $(stat -c %s $orig_file) bytes to $(stat -c %s $new_file) bytes." | \
        $MAILX -v -S smtp=mail -S hostname=hblog-container -r rlpowell@digitalkingdom.org -s "hblog: FILE SHRINK FOUND: $orig_file" rlpowell@digitalkingdom.org
      exit 1
    fi

    /tmp/hbgwdiff.sh x "$orig_file" x x "$new_file" || true
    /tmp/hbgwdiff.sh x "$orig_file" x x "$new_file" | $MAILX -v -S smtp=mail -S hostname=hblog-container -r rlpowell@digitalkingdom.org -s "FILE CHANGES: hblog automation: $type differences: \"$orig_file\"" rlpowell@digitalkingdom.org

    usedir="$indir"
    if [ "$testing" ]
    then
      usedir="$outdir"
    fi

    # Before you make changes to any of the data handling here, read
    # and update the "Dates" section in DESIGN-CODE.  This stuff is
    # complicated.
    short=$(snipdir "$orig_file" "$usedir")
    prevdate="$(git -C "$usedir" log --format='%ai' -n 1 "$short")"

    touch -d "$prevdate" "$new_file"
    cp -p "$new_file" "$orig_file"
    export TZ=America/Los_Angeles
    export GIT_AUTHOR_DATE="$prevdate"
    git -C "$usedir" commit -m "Automated Checkin: $type differences found in $orig_file at $(date) ; preserving previous checkin date of $prevdate" \
      --date="$prevdate" \
      "$short"
    echo "Sleeping to help with sync."
    sleep 10

    # When it's all done, we want the apparent file date to have not changed.
    touch -d "$prevdate" "$orig_file"
  fi
}

checkin_dir () {
  new_file_dir="$1"
  orig_file_dir="$2"
  type="$3"

  find $new_file_dir -type f | while read fname
  do
    short=$(snipdir "$fname" "$new_file_dir")
    checkin "$new_file_dir/$short" "$orig_file_dir/$short" "$type"
  done
}

checkin_dir "$tempdir" "$outdir" "pandoc to-and-from markdown"

rm -rf "$tempdir/"
mkdir -p "$tempdir"
unphone "$outdir" "$tempdir"

checkin_dir "$tempdir" "$outdir" unphone

rm -rf "$tempdir/"
mkdir -p "$tempdir"
rectifier "$outdir" "$tempdir"

checkin_dir "$tempdir" "$outdir" rectifier

if [ "$testing" ]
then
  cd "$outdir"
  git log -p | \
    sed -r -e 's/^commit \S+$/commit [hash]/' \
    -e 's/^Date:   .*/Date:   [date]/' \
    -e 's/^(    Automated Checkin: .* found at) .*/\1 [date]/' \
    -e 's/^(    Automated Checkin: .* found in .* at) [^;]* (; preserving previous checkin date of) .*/\1 [date] \2 [date]/' \
    -e 's/^index [^.]*..[^.]*$/index [hash]..[hash]/' \
    >git_log
  cd "$origpwd"
  rm -rf "$outdir/.git/"
fi

rm -rf "$tempdir/"
