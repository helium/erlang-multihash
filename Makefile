.PHONY: compile rel cover test typecheck

REBAR=./rebar3
SHORTSHA=`git rev-parse --short HEAD`
PKG_NAME_VER=${SHORTSHA}

OS_NAME=$(shell uname -s)

ifeq (${OS_NAME},FreeBSD)
MAKE="gmake"
else
MAKE="make"
endif

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean
	$(REBAR) as prod clean

cover: test
	$(REBAR) cover --verbose

test:
	$(REBAR) as test do eunit

shell:
	$(REBAR) shell

typecheck:
	$(REBAR) dialyzer
