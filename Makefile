SCONS := scons

all:
	@$(SCONS) -Q

clean:
	@$(SCONS) -c -Q
