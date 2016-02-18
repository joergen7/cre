all: .rebar/cre_18.2.1_plt
	rebar co eu dialyze doc

clean:
	rebar clean

.rebar/cre_18.2.1_plt: deps/effi/src/effi.erl
	rebar build-plt

deps/effi/src/effi.erl:
	rebar get-deps