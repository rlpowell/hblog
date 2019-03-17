# Note that all these link locations assume we're in a container that
# is using syncthing to have ~/src and ~/Docs be the relevant
# Dropbox dirs, syncthing-ed from vrici.

# Setup links for our locally hacked libraries
rm -f pandoc-2.0 ; ln -sf ~/src/pandoc/ pandoc-2.0
ln -sf ~/.stack-work-pandoc ~/src/pandoc/.stack-work
rm -f hakyll-4.9.8.0 ; ln -sf ~/src/hakyll/ hakyll-4.9.8.0
ln -sf ~/.stack-work-hakyll ~/src/hakyll/.stack-work
rm -f pandoc-citeproc-0.10.5.1 ; ln -sf ~/src/pandoc-citeproc/ pandoc-citeproc-0.10.5.1
ln -sf ~/.stack-work-pandoc-citeproc ~/src/pandoc-citeproc/.stack-work

rm -f posts
if [ -e posts ]
then
  echo "Couldn't remove the posts link"
  exit 1
fi

# For our blog posts
ln -sf ~/Docs/Public/hblog_posts posts

ln -sf ~/.stack-work-hblog .stack-work

# Make sure our non-stack script is in a place stack can find it
mkdir -p ~/.local/bin/
ln -sf /home/rlpowell/src/hblog/munge_files.sh ~/.local/bin/munge_files
