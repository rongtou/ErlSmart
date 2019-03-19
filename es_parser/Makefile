
all:init erl

.PHONY:clean
clean:
	@ rm -rf ebin

.PHONY:init
init:
	@ mkdir -p ebin

.PHONY:erl
erl:$(patsubst src/%.erl, ebin/%.beam, $(wildcard src/*.erl))

Erlc = erlc -Werror -I include +debug_info -Dmaps_support -o ebin

ebin/%.beam:src/%.erl
	@ echo compile `basename $<`
	@ ${Erlc} $<

.PHONY:run
run:
	@ erl -noinput -pa ebin -s es_parser run src/test.erl -s erlang halt

.PHONY:beam_code
beam_code:
	@ erl -noinput -pa ebin -s beam_code start ebin/test.beam -s erlang halt
