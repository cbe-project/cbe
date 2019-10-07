SRC_CC = vfs.cc

LIBS += cbe cbe_cxx sha256_4k

vpath %.cc $(REP_DIR)/src/lib/vfs/cbe

SHARED_LIB = yes

CC_CXX_WARN_STRICT :=

LIB_ADAINIT := yes
