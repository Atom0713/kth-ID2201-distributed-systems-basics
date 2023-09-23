LOCALIP=$(shell ipconfig getifaddr en0)

shell: 
	erl

compile:
	for name in *.erl; do\
		erlc $$name; \
	done

routy: NAME=
routy: compile
	erl -name $(NAME)@$(LOCALIP) -setcookie routy -connect_all false