dir="$(dirname $0)"
cd "$dir"
./setup_links.sh

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

rsync -a --delete tests/ /dropbox/src/hblog/tests/ >/dev/null 2>&1

./teardown_links.sh
