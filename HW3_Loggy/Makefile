LOCALIP=$(shell ipconfig getifaddr en0)

shell: compile
	erl

compile:
	for name in *.erl; do\
		erlc $$name; \
	done

routy: compile
	erl -name $(NAME)@$(LOCALIP) -setcookie routy -connect_all false