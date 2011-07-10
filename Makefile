.PHONY: all compile deps clean test devshell metisshell riakshell bundle release

BRANCH = $(shell git branch|grep '*'|cut -d' ' -f2)
DATE   = $(shell date +%d%b%y)
DIR    = ../RELEASE/
NAME   = cadam-branch-$(BRANCH)-$(DATE)



all: deps compile

deps:
	cd deps/yaws-1.90/; ./configure; make

compile:
	./rebar compile

clean:
	./rebar clean
	rm -f `find ./ -name '*~'`
	rm deps/yaws-1.90/ebin/*.beam

test: compile
	erl -sname test -setcookie aaa -pa ebin -s tests -s init stop

devshell: compile
	erl -sname dev -setcookie aaa -pa ebin -s reloader -boot start_sasl -config conf/cadam

metisshell: compile
	erl -name t -setcookie metis -pa ebin -s reloader -boot start_sasl -config conf/cadam


riakshell: compile
	erl -name cadam@127.0.0.1 -setcookie riak -pa ebin -s reloader -boot start_sasl -config conf/cadam

bundle: compile
	git bundle create $(DIR)$(NAME).bundle HEAD

release: compile
	cp conf/cadam.config conf/cadam.config.backup
	cp conf/cadam_cli.config conf/cadam_cli.config.backup

	cp conf/cadam.config.example conf/cadam.config
	cp conf/cadam_cli.config.example conf/cadam_cli.config

	git archive --format=tar --prefix=cadam/ HEAD > $(DIR)$(NAME).tar
	cd priv/www/cadamgui; ./generate.py source-all; ./generate.py build
	rm -rf priv/www/gui/
	mv priv/www/cadamgui/build priv/www/gui
	tar -rf $(DIR)$(NAME).tar ../cadam/priv/www/gui/ ../cadam/conf/cadam.config ../cadam/conf/cadam_cli.config
	gzip $(DIR)$(NAME).tar

	cp conf/cadam.config.backup conf/cadam.config
	cp conf/cadam_cli.config.backup conf/cadam_cli.config
	rm conf/*.backup
