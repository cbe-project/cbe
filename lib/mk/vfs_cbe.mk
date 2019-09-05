SRC_CC = vfs.cc

LIBS += cbe sha256_4k

vpath %.cc $(REP_DIR)/src/lib/vfs/cbe

SHARED_LIB = yes
