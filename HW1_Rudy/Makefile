LOCALIP=$(shell ipconfig getifaddr en0)

shell: 
	erl

compile_all:
	for name in *.erl; do\
		erlc $$name; \
	done

rudy:
	erl -name rudy@$(LOCALIP) -setcookie secret

test:
	erl -name test@$(LOCALIP) -setcookie secret

test2:
	erl -name test2@$(LOCALIP) -setcookie secret