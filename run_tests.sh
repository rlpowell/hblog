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

stack build hblog
stack install hblog
stack test --test-arguments "$*"

echo "Deleting .git directories."
find tests/ -name .git | xargs rm -rf
