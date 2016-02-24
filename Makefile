all: .rebar/cre_18.2.1_plt
	rebar co eu dialyze

clean:
	rebar clean

.rebar/cre_18.2.1_plt:
	rebar update-deps build-plt

