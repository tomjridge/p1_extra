NAME:=p1_extra

all: 
	$(MAKE) local
	$(MAKE) using_opam
# 	$(MAKE) from_github

local:
	rm -rf p1_extra
	mkdir p1_extra
	rsync --exclude='.dockerfile/*' -vaz ../ ./p1_extra  # NOTE need .git for opam pin
	docker build -t $(NAME)_local -f Dockerfile.local .

from_github:
	docker build -t $(NAME)_from_github -f Dockerfile.from_github .


using_opam:
	docker build -t $(NAME)_using_opam -f Dockerfile .

# 
# #	docker run -ti $NAME bash

clean:
	-docker image rm $(NAME)_local $(NAME)_from_github $(NAME)_using_opam
	rm -rf p1_extra
