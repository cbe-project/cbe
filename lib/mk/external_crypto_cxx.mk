LIBS += spark aes_cbc_4k

INC_DIR += $(REP_DIR)/src/lib/cbe
INC_DIR += $(REP_DIR)/src/lib/cbe_cxx

INC_DIR += $(REP_DIR)/src/lib/external_crypto
INC_DIR += $(REP_DIR)/src/lib/external_crypto_cxx

SRC_ADB += external-crypto-cxx.adb

vpath % $(REP_DIR)/src/lib/external_crypto_cxx
