Screw makefiles.
#
#
# all:	docker_run site_copy
# 	echo done
# 
# #********************
# # Runs Outside The Container
# #********************
# 
# SLASH_DROPBOX=$(shell stat -c %F /dropbox/src/hblog 2>/dev/null)
# ifeq ($(SLASH_DROPBOX),directory)
# # Windows style; we're giving commands to the docker system we are,
# # in fact, running in; it's running on a windows box
# DOCKER_RUN_CMD=sudo docker run --name hblog -v /var/run/docker.sock:/var/run/docker.sock -v /c/Users/rlpowell/Dropbox/src/hblog:/home/rlpowell/src/hblog:z --volumes-from stackstore
# else
# # On a real system/VM/something
# DOCKER_RUN_CMD=sudo docker run --name hblog -v /var/run/docker.sock:/var/run/docker.sock -v /home/rlpowell/src/hblog:/home/rlpowell/src/hblog:z --volumes-from stackstore
# endif
# 
# docker_clean:
# 	sudo docker rmi rlpowell/hblog || true
# 
# docker_build:
# 	sudo id -a
# 	sudo docker kill hblog || true
# 	sudo docker rm hblog || true
# 	sudo docker build -t rlpowell/hblog .
# 
# docker_run_prep:		docker_build
# 	sudo id -a
# 	sudo docker kill hblog || true
# 	sudo docker rm hblog || true
# 
# docker_run_interactive:		docker_run_prep
# 	$(DOCKER_RUN_CMD) --rm -t -i rlpowell/hblog zsh
# 
# docker_run:		        docker_run_prep
# 	$(DOCKER_RUN_CMD) --rm rlpowell/hblog /usr/bin/make site
# 
# # Must be run outside the container because the docker doesn't have
# # perms on ~/public_html/
# site_copy:
# 	rsync -av --delete /home/rlpowell/src/hblog/_site/ /home/rlpowell/public_html/hblog/
# 
# #********************
# # Runs Inside The Container
# #********************
# 
# bins:	/home/rlpowell/.local/bin/hblog /home/rlpowell/.local/bin/tiki_to_md
# 
# /home/rlpowell/.local/bin/hblog: app/hblog.hs
# 	bin/run_stack build --copy-bins --test --ghc-options -Wall :hblog
# 
# /home/rlpowell/.local/bin/tiki_to_md: app/tiki_to_md.hs
# 	bin/run_stack build --copy-bins --test --ghc-options -Wall :tiki_to_md
# 
# clean:
# 	rm -rf _site/ _cache/
# 
# site_clean: clean bins
# 	~/.local/bin/hblog rebuild -v
# 
# site: bins
# 	~/.local/bin/hblog build -v
