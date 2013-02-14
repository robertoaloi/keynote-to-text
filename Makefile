.PHONY: clean deps

REBAR=$(PWD)/rebar

all: deps compile escriptize

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

escriptize:
	$(REBAR) escriptize

clean:
	$(REBAR) clean

distclean: clean
	rm -rf deps
