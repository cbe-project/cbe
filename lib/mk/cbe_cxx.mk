LIBS += spark aes_cbc_4k

INC_DIR += $(REP_DIR)/src/lib/cbe
INC_DIR += $(REP_DIR)/src/lib/cbe_cxx

SRC_ADB += cbe-cxx-cxx_library.adb

vpath % $(REP_DIR)/src/lib/cbe_cxx
