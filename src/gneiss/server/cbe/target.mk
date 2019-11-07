TARGET = cbe
SRC_ADB += component.adb conversion.adb cbe_debug.adb
LIBS += base spark gneiss libsparkcrypto cbe aes_cbc_4k sha256_4k external_crypto
INC_DIR += $(PRG_DIR)
