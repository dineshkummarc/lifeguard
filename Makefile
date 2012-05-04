all: deps compile

compile:
	./rebar compile

clean:
	./rebar clean

deps:
	./rebar get-deps
	cd deps/erlv8 && make

devrel: rel
	rm -rf rel/lifeguard/lib/lifeguard-*/priv
	ln -sf $(abspath ./apps/lifeguard/priv) rel/lifeguard/lib/lifeguard-*

rel: compile
	./rebar generate -f

test: compile
	./rebar eunit apps=lifeguard

.PHONY: all compile clean deps
