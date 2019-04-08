LIBS    += spark aes_cbc_4k

INC_DIR += $(REP_DIR)/src/lib/cbe

SRC_ADS += cbe.ads

SRC_ADB += cbe-cxx-cxx_crypto.adb
SRC_ADB += cbe-cxx-cxx_pool.adb
SRC_ADB += cbe-cxx-cxx_splitter.adb
SRC_ADB += cbe-cxx-cxx_primitive.adb
SRC_ADB += cbe-cxx-cxx_request.adb
SRC_ADB += cbe-cxx.adb
SRC_ADB += cbe-splitter.adb
SRC_ADB += cbe-primitive.adb
SRC_ADB += cbe-request.adb
SRC_ADB += cbe-pool.adb
SRC_ADB += cbe-crypto.adb

vpath % $(REP_DIR)/src/lib/cbe
