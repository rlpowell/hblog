dir="$(dirname $0)"
cd "$dir"
./setup_links.sh

echo "Unpacking git zips."
oldpwd=$(pwd)
find tests/ -name git.zip | while read zipname
do
  cd "$(dirname $zipname)"
  find . -name .git | xargs rm -rf
  unzip "$(basename $zipname)" >/dev/null
  cd "$oldpwd"
done

rm -rf coverage/
mkdir coverage/

# Force a rebuild ; --force-dirty doesn't (as of 2017-08-07) do the
# right thing, at least by itself; see
# https://github.com/commercialhaskell/stack/issues/3306 and
# https://github.com/commercialhaskell/stack/issues/1940
for file in $(find app/ lib/ test/ -type f | grep -v '^\.')
do
  sed -i '$s/$/ /' $file
done

stack build --coverage --force-dirty hblog
stack install --coverage --force-dirty hblog
stack test --coverage --force-dirty --test-arguments "$*"

# Here we find the .tix files, sum *all* of them, put them where we
# want them, and clean up the originals.
tixes=$(find . "$(stack path --local-hpc-root)/"hblog*/ -name hpc -prune -o -type f -name '*.tix' -print | tr '\n' ' ')
if [ "$tixes" ]
then
  echo stack exec hpc -- sum --exclude=Main --union $tixes
  stack exec hpc -- sum --exclude=Main --union $tixes >coverage/all.tix

  /bin/rm -f $tixes
else
  echo "No tix files found at all; bailing."
  exit 1
fi

# Here we generate the markup.
echo stack exec hpc -- markup --hpcdir=$(stack path --dist-dir)/hpc/ --destdir=coverage/ --verbosity=2 coverage/all.tix
stack exec hpc -- markup --hpcdir=$(stack path --dist-dir)/hpc/ --destdir=coverage/ --verbosity=2 coverage/all.tix

# Cleanup
if [ -f hblog.cabal.orig ]
then
  mv hblog.cabal.orig hblog.cabal
fi

# Undo what we did to force a rebuild
for file in $(find app/ lib/ test/ -type f | grep -v '^\.')
do
  sed -i '$s/ *$//' $file
done

echo "Deleting .git directories."
find tests/ -name .git | xargs rm -rf
