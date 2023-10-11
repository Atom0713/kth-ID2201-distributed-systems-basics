LOCALIP=$(shell ipconfig getifaddr en0)

shell: compile
	erl

compile:
	for name in *.erl; do\
		erlc $$name; \
	done

chordy: NAME=chordy
chordy: compile
	erl -name $(NAME)@$(LOCALIP) -setcookie chordy
