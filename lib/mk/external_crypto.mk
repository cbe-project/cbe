LIBS    += cbe spark aes_cbc_4k

INC_DIR += $(REP_DIR)/src/lib/cbe
INC_DIR += $(REP_DIR)/src/lib/external_crypto

SRC_ADB += external-crypto.adb

vpath % $(REP_DIR)/src/lib/external_crypto
