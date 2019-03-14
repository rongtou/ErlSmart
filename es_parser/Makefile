
all:init erl

.PHONY:clean
clean:
	@ rm -rf ebin

.PHONY:init
init:
	@ mkdir -p ebin

.PHONY:erl
erl:$(patsubst src/%.erl, ebin/%.beam, $(wildcard src/*.erl))

Erlc = erlc -Werror -I include +debug_info -o ebin

ebin/%.beam:src/%.erl
	@ echo compile `basename $<`
	@ ${Erlc} $<
