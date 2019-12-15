# Note that all these link locations assume we're in a container that
# is mounting /home/rlpowell/ from the host.  /home/rlpowell/src
# gets syncthing-ed between the various hosts; see
# ~/src/containers/README.txt for further discussion.

# Setup links for our locally hacked libraries
rm -f pandoc-2.0 ; ln -sf ~/src/pandoc/ pandoc-2.0
rm -f hakyll-4.9.8.0 ; ln -sf ~/src/hakyll/ hakyll-4.9.8.0
rm -f pandoc-citeproc-0.10.5.1 ; ln -sf ~/src/pandoc-citeproc/ pandoc-citeproc-0.10.5.1

rm -f posts
if [ -e posts ]
then
  echo "Couldn't remove the posts link"
  exit 1
fi

# For our blog posts
ln -sf ~/Docs/Public/hblog_posts posts

# Make sure our non-stack script is in a place stack can find it
mkdir -p ~/.local/bin/
ln -sf /home/rlpowell/src/hblog/munge_files.sh ~/.local/bin/munge_files
