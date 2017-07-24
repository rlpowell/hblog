sudo docker create -v /home/rlpowell/.stack --name volume--stack--global rlpowell/shell /bin/true
sudo docker create -v /home/rlpowell/.local --name volume--local--hblog rlpowell/shell /bin/true
sudo docker create -v /home/rlpowell/src/hblog/.stack-work --name volume--stack-work--hblog rlpowell/shell /bin/true
sudo docker create -v /home/rlpowell/src/pandoc/.stack-work --name volume--stack-work--pandoc rlpowell/shell /bin/true
sudo docker create -v /home/rlpowell/src/pandoc-citeproc/.stack-work --name volume--stack-work--pandoc-citeproc rlpowell/shell /bin/true
sudo docker create -v /home/rlpowell/src/hakyll/.stack-work --name volume--stack-work--hakyll rlpowell/shell /bin/true
