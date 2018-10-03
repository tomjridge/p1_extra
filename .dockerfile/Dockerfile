# NOTE maybe add a "RUN echo ..." to force a rebuild, or --no-cache
FROM ocaml/opam2:ubuntu

USER root
RUN apt-get -y update
RUN apt-get install -y curl
RUN apt-get install -y gcc
RUN apt-get install -y bzip2
RUN apt-get install -y wget
RUN apt-get install -y sudo
RUN apt-get install -y unzip m4
RUN apt-get install -y time
USER opam


RUN opam list
RUN opam update

RUN eval `opam config env` && opam pin add tjr_lib_core https://github.com/tomjridge/tjr_lib.git
RUN eval `opam config env` && opam pin add tjr_lib https://github.com/tomjridge/tjr_lib.git
RUN eval `opam config env` && opam pin add p1_core https://github.com/tomjridge/p1_core.git
RUN eval `opam config env` && opam pin add p1_extra https://github.com/tomjridge/p1_extra
