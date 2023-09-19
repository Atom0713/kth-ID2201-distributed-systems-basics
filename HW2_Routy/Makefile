LOCALIP=$(shell ipconfig getifaddr en0)

shell: 
	erl

compile:
	for name in *.erl; do\
		erlc $$name; \
	done

routy: compile
	erl -name sweden@$(LOCALIP) -setcookie routy -connect_all false