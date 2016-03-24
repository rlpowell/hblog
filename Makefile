all:	docker_run site_copy
	echo done

#********************
# Runs Outside The Container
#********************

DOCKER_RUN_CMD=sudo docker run --name hblog -v /home/rlpowell/src/hblog:/home/hakyll/hblog:rw

docker_clean:
	sudo docker rmi rlpowell/hblog || true

docker_build:
	sudo -v
	cp /home/rlpowell/config/dotfiles/zshrc .
	cp /home/rlpowell/config/dotfiles/bothrc .
	sudo docker kill hblog || true
	sudo docker rm hblog || true
	sudo docker build --rm --force-rm -t rlpowell/hblog .
	rm zshrc bothrc

docker_run_prep:		docker_build
	sudo -v
	sudo chcon -R -t svirt_sandbox_file_t /home/rlpowell/src/hblog
	sudo docker kill hblog || true
	sudo docker rm hblog || true

docker_run_interactive:		docker_run_prep
	$(DOCKER_RUN_CMD) --rm -t -i rlpowell/hblog

docker_run:		        docker_run_prep
	$(DOCKER_RUN_CMD) --rm rlpowell/hblog /usr/bin/make site

# Must be run outside the container because the docker doesn't have
# perms on ~/public_html/
site_copy:
	rsync -av --delete /home/rlpowell/src/hblog/_site/ /home/rlpowell/public_html/hblog/

#********************
# Runs Inside The Container
#********************
dirs:
	mkdir -p build/ bin/ _site/ _cache/ dist/

bins:	bin/hblog bin/tiki_to_md dirs

bin/hblog: src/hblog.hs dirs
	ghc --make -threaded -tmpdir build/ -outputdir build/ -o bin/hblog src/hblog.hs

bin/tiki_to_md: src/tiki_to_md.hs dirs
	ghc --make -threaded -tmpdir build/ -outputdir build/ -o bin/tiki_to_md src/tiki_to_md.hs

clean:
	rm -rf build/ bin/ _site/ _cache/ dist/

site_clean: clean bins
	bin/hblog rebuild -v

site: bins
	bin/hblog build -v
