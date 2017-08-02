# Make sure our non-stack script is in a place stack can find it
ln -sf /home/rlpowell/src/hblog/munge_files.sh ~/.local/bin/munge_files

find /home/rlpowell/src/hblog/tests/ /dropbox/src/hblog/tests/   -type f | xargs chmod a-x

echo "Unpacking git zips."
oldpwd=$(pwd)
find tests/ -name git.zip | while read zipname
do
  cd "$(dirname $zipname)"
  find . -name .git | xargs rm -rf
  unzip "$(basename $zipname)" >/dev/null
  cd "$oldpwd"
done

rm -rf cov/ hpc/
for name in hblog rectifier unphone tiki_to_md test lib test
do
  mkdir -p hpc/$name
done

if [ ! -f hblog.cabal.orig ]
then
  mv hblog.cabal hblog.cabal.orig
fi
sed -e '/^\s*ghc-options:/d' -e 's/-- cov: //g' hblog.cabal.orig >hblog.cabal

for file in app/* test/Spec.hs lib/HBlog/Lib.hs
do
  sed -i '$s/$/ /' $file
done

stack build hblog
stack install hblog
stack test --test-arguments "$*"
rsync -a --delete tests/ /dropbox/src/hblog/tests/

for name in $(find . -name hblog-test.tix)
do
  mv $name $(echo $name | sed 's/hblog-test/test/g')
done

for name in hblog rectifier unphone tiki_to_md test
do
  mkdir -p cov/$name
  tixes=$(find . -name hpc -prune -o -type f -name $name.tix -print | tr '\n' ' ')
  if [ "$tixes" ]
  then
    rm hpc/$name.tix
    ln -s ../lib/$(cd hpc/lib ; \ls)  hpc/$name/

    stack exec hpc sum $tixes >hpc/$name.tix

    /bin/rm -f ./.stack-work/dist/x86_64-linux/*/hpc/*
    /bin/rm -f ./.hpc/*
    /bin/rm -f $tixes
  fi
  if [ -f hpc/$name.tix ]
  then
    echo stack exec hpc -- markup --hpcdir=hpc/$name/ --destdir=cov/$name/ --verbosity=2 hpc/$name.tix
    stack exec hpc -- markup --hpcdir=hpc/$name/ --destdir=cov/$name/ --verbosity=2 hpc/$name.tix
  fi
done

find hpc -type l | xargs rm
rsync -a --delete cov/ /dropbox/src/hblog/cov/ >/dev/null 2>&1
rsync -a --delete hpc/ /dropbox/src/hblog/hpc/ >/dev/null 2>&1

# Cleanup
if [ -f hblog.cabal.orig ]
then
  mv hblog.cabal.orig hblog.cabal
fi

for file in app/* test/Spec.hs lib/HBlog/Lib.hs
do
  sed -i '$s/ *$//' $file
done

echo "Deleting .git directories."
find tests/ -name .git | xargs rm -rf
