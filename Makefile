SCONS := scons

all:
	@$(SCONS) -Q

clean:
	@$(SCONS) -c -Q

macports:
	@$(SCONS) -Q JPEGLIB_INC_PATH=/opt/local/include \
							 JPEGLIB_LIB_PATH=/opt/local/lib
