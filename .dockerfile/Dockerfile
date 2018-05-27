# NOTE maybe add a "RUN echo ..." to force a rebuild, or --no-cache

# FROM ocaml/opam:ubuntu - latest doesn't work due to problems with ppx_deriving deps
# 17.04 doesn't work due to missing archives at ubuntu? or stale packages info?
FROM ocaml/opam:ubuntu-16.04_ocaml-4.04.2
# RUN apt-get -y update

RUN opam list
RUN opam update

RUN eval `opam config env` && opam pin add tjr_lib https://github.com/tomjridge/tjr_lib.git

RUN eval `opam config env` && opam pin add p1_core https://github.com/tomjridge/p1_core.git

RUN git clone https://github.com/tomjridge/p1_extra.git

# NOTE opam pin has the side effect of building p1_core; this checks the opam file
RUN eval `opam config env` && opam pin add p1_extra ./p1_extra

# NOTE we also build p1_extra directly, and run the examples
RUN eval `opam config env` && cd p1_extra && make all && make -C bin 
