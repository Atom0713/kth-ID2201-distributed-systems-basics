LOCALIP=$(shell ipconfig getifaddr en0)

shell: compile
	erl

compile:
	for name in *.erl; do\
		erlc $$name; \
	done
