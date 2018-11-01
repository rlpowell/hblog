#!/bin/bash

set -e
# set -x

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

echo "Building hblog."
stack build hblog
stack install

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
  cd $origpwd
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
    echo "Dropbox conflicted file: $fname" | mailx -v -S smtp=mail -r rlpowell@digitalkingdom.org -s 'CONFLICTED FILE FOUND' rlpowell@digitalkingdom.org
    exit 1
  fi

  short=$(snipdir "$fname" "$indir")
  mkdir -p $(dirname "$tempdir/$short")

  # Make sure everything is checked in
  origdate="$(git -C "$indir" log --format='%ai' -n 1 "$short")"
  if [ -z "$origdate" ]
  then
    origdate="$(date --iso-8601=seconds -r "$fname")"
  fi
  cd "$(dirname $fname)"

  # Needs changes checked in
  if ! git diff --quiet "$(basename $fname)"
  then
    echo "Found changes to $(basename $fname)"
    git --no-pager diff "$(basename $fname)"
    git commit -m "Automated checkin of external changes to $fname at $(date)" \
      --date="$origdate" \
      "$(basename $fname)"
  fi

  # Never been checked in before
  if [ ! "$(git ls-files "$(basename $fname)")" ]
  then
    echo "Found new file $(basename $fname)"
    git add "$(basename $fname)"
    git commit -m "Automated initial checkin of $fname at $(date)" \
      --date="$origdate" \
      "$(basename $fname)"
  fi
  cd "$origpwd"
  touch -d "$origdate" "$fname"

  mkdir -p ~/.hblog-cache/
  cache_file=~/.hblog-cache/$(echo $fname | sed 's;/;_;g')
  if [ ! -f $cache_file ] || ! diff -q $fname $cache_file >/dev/null 2>&1
  then
    echo "File $fname does not match its cached version; running pandoc on it to normalize its syntax."
    stack exec pandoc -- -s -f markdown -t markdown --wrap=preserve -o $tempdir/$short $fname
    # Pandoc likes to turn
    #   [test](ched: The Chunk Editor)
    # into
    #   [test](ched:%20The%20Chunk%20Editor)
    # , and screw that.  Also Rectifier puts it back.  Haven't
    # figured out how that even works, but it leads to bullshit
    # checkins, so we fix it.
    sed -i '/[]](/s/%20/ /g' "$tempdir/$short"
    cp "$fname" "$cache_file"
    touch -d "$origdate" "$fname" "$cache_file" "$tempdir/$short"
  else
    echo "File $fname matches its cached version; not updating."
  fi
done

checkin () {
  infile="$1"
  outfile="$2"
  type="$3"

  if [ ! -f "$outfile" ]
  then
    echo "The file $outfile doesn't exist, but I copied everything to that area earlier; bailing."
    exit 1
  elif ! diff -q "$infile" "$outfile" >/dev/null 2>&1
  then
    echo "Found $type changes to $outfile"
    diff -u "$outfile" "$infile" || true
    short=$(snipdir "$outfile" "$indir")
    origdate="$(git -C "$indir" log --format='%ai' -n 1 "$short")"
    touch -d "$origdate" "$infile"
    cp -p "$infile" "$outfile"
    export TZ=America/Los_Angeles
    export GIT_AUTHOR_DATE="$origdate"
    export GIT_COMMITTER_DATE="$origdate"
    git -C "$indir" commit -m "Automated Checkin: $type differences found in $outfile at $(date)" \
      --date="$origdate" \
      "$short"

    # When it's all done, we want the apparent file date to have not changed.
    touch -d "$origdate" "$outfile"
  fi
}

checkin_dir () {
  cindir="$1"
  coutdir="$2"
  type="$3"

  find $cindir -type f | while read fname
  do
    short=$(snipdir "$fname" "$cindir")
    checkin "$cindir/$short" "$coutdir/$short" "$type"
  done
}

checkin_dir "$tempdir" "$outdir" "pandoc to-and-from markdown"

rm -rf "$tempdir/"
mkdir -p "$tempdir"
stack exec unphone -- "$outdir" "$tempdir"

checkin_dir "$tempdir" "$outdir" unphone

rm -rf "$tempdir/"
mkdir -p "$tempdir"
stack exec rectifier -- "$outdir" "$tempdir"

checkin_dir "$tempdir" "$outdir" rectifier

if [ "$testing" ]
then
  cd "$outdir"
  git log -p | \
    sed -r -e 's/^commit \S+$/commit [hash]/' \
    -e 's/^Date:   .*/Date:   [date]/' \
    -e 's/^(    Automated Checkin: .* found at) .*/\1 [date]/' \
    -e 's/^index [^.]*..[^.]*$/index [hash]..[hash]/' \
    >git_log
  cd "$origpwd"
  rm -rf "$outdir/.git/"
fi

rm -rf "$tempdir/"
