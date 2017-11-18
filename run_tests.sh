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

stack build hblog
stack install hblog
stack test --test-arguments "$*"

echo "Deleting .git directories."
find tests/ -name .git | xargs rm -rf
