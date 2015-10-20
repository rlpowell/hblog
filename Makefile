clean:
	rm site site.o site.hi

docker_clean:
	sudo docker rmi rlpowell/hblog || true

docker_build:
	cp /home/rlpowell/config/dotfiles/zshrc .
	cp /home/rlpowell/config/dotfiles/bothrc .
	sudo docker kill hblog || true
	sudo docker rm hblog || true
	sudo docker build --rm --force-rm -t rlpowell/hblog .
	rm zshrc bothrc

docker_run:
	echo sudo docker kill mw_pather || true
	echo sudo docker rm mw_pather || true
	echo sudo docker run --name mw_pather -d -t --entrypoint="/bin/bash" -v /home/rlpowell/src/mw_pather/:/srv/haste/mw_pather/:rw -p 0.0.0.0:24601:24601 rlpowell/mw_pather -c 'cd /srv/haste/mw_pather ; make ; ./mw_pather'

docker_run_interactive:
	sudo chcon -R -t svirt_sandbox_file_t /home/rlpowell/src/hblog
	sudo docker kill hblog || true
	sudo docker rm hblog || true
	sudo docker run --name hblog -v /home/rlpowell/src/hblog:/home/hakyll/hblog:rw -t -i rlpowell/hblog

site_copy:
	rsync -av --delete /home/rlpowell/src/hblog/_site/ /home/rlpowell/public_html/hblog/

site_clean:
	ghc --make -threaded site.hs
	./site rebuild -v

site:
	./site build -v
