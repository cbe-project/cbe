SRC_CC = vfs.cc

SRC_ADS := dummy.ads

LIBS += spark cbe cbe_cxx sha256_4k external_crypto external_crypto_cxx

vpath % $(REP_DIR)/src/lib/vfs/cbe

SHARED_LIB = yes

CC_CXX_WARN_STRICT :=

LIB_ADAINIT := yes
