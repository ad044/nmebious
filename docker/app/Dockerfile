FROM daewok/sbcl

ENV QUICKLISP_ADD_TO_INIT_FILE=true

RUN apt-get update
RUN apt-get install -y imagemagick

RUN /usr/local/bin/install-quicklisp

RUN echo "(ql:quickload :linedit) \
	  (linedit:install-repl :wrap-current t :eof-quits t)" >> ~/.sbclrc

COPY . /usr/src/app

WORKDIR /usr/src/app

CMD ["make", "run"]
