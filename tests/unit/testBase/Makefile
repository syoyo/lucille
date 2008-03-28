SUBDIRS = `ls | grep test`

all:
	@for d in $(SUBDIRS); do	\
	  $(MAKE) -C $$d;		\
	done
