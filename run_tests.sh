dir="$(dirname $0)"
cd "$dir"

echo "Unpacking git zips."
oldpwd=$(pwd)
find tests/ -name git.zip | while read zipname
do
  cd "$(dirname $zipname)"
  find . -name .git | xargs rm -rf
  unzip "$(basename $zipname)" >/dev/null
  cd "$oldpwd"
done

/bin/echo -e "\n\nNOTE: In cases where the primary command succeeds, the output (stdout/stderr) will not be shown from that command, only from the diff that runs after.\n\n"

stack test --test-arguments "$*"
exitval="$?"

echo "Deleting .git directories."
find tests/ -name .git | xargs rm -rf

exit "$exitval"
