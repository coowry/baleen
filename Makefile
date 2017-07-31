default: compile

help:
	@echo Usage: make [all] [compile] [test] [dialyze] [clean] [pull] [push] [upgrade]

all: compile dialyze test

upgrade:
	./rebar3 upgrade

compile: upgrade
	./rebar3 compile

dialyze:
	./rebar3 dialyzer

test:
	./rebar3 eunit

shell:
	./rebar3 shell

clean:
	./rebar3 clean

pull:
	git fetch --prune
	git checkout master && git merge origin/master
	git checkout develop && git merge origin/develop

push: pull
	git push --follow-tags origin master develop

.PHONY: default all help compile test dialyze shell clean pull push
