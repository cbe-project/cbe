LIBS    += spark aes_cbc_4k sha256_4k

INC_DIR += $(REP_DIR)/src/lib/cbe

SRC_ADB += cbe.adb
SRC_ADB += cbe-debug.adb
SRC_ADB += cbe-library.adb
SRC_ADB += cbe-splitter.adb
SRC_ADB += cbe-primitive.adb
SRC_ADB += cbe-request.adb
SRC_ADB += cbe-pool.adb
SRC_ADB += cbe-crypto.adb
SRC_ADB += cbe-cache.adb
SRC_ADB += cbe-tree_helper.adb
SRC_ADB += cbe-translation.adb
SRC_ADB += cbe-cache_flusher.adb
SRC_ADB += cbe-sync_superblock.adb
SRC_ADB += cbe-virtual_block_device.adb
SRC_ADB += cbe-free_tree.adb
SRC_ADB += cbe-block_io.adb
SRC_ADB += cbe-write_back.adb

vpath % $(REP_DIR)/src/lib/cbe

CC_ADA_OPT += -gnatec=$(REP_DIR)/src/lib/cbe/pragmas.adc
