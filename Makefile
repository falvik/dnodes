
all:
	./rebar get-deps compile

run:
	erl -name dnodes@127.0.0.1 -boot start_sasl \
		-pa ../dnodes/apps/dnodes/ebin/ \
		-pa ./deps/cowlib/ebin/ \
		-pa ./deps/ranch/ebin/ \
		-pa ./deps/cowboy/ebin/ \
		-pa ./deps/luerl/ebin/ \
		-s dnodes start -sasl errlog_type error
