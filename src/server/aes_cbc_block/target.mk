TARGET  := aes_cbc_block
SRC_CC  := main.cc
INC_DIR := $(PRG_DIR)
LIBS    += base aes_cbc_4k spark

SRC_ADS := cbe.ads

SRC_ADB := cbe-cxx-cxx_pool.adb
SRC_ADB += cbe-cxx-cxx_splitter.adb
SRC_ADB += cbe-cxx-cxx_primitive.adb
SRC_ADB += cbe-cxx-cxx_request.adb
SRC_ADB += cbe-cxx.adb
SRC_ADB += cbe-splitter.adb
SRC_ADB += cbe-primitive.adb
SRC_ADB += cbe-request.adb
SRC_ADB += cbe-pool.adb
