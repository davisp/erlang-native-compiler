all:
	./bootstrap

debug:
	./bootstrap debug

xref: debug
	./xref

clean:
	@rm -rf rebar .rebar ebin/*.beam
