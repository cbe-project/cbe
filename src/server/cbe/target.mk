REQUIRES := x86_64

TARGET  := cbe
SRC_CC  := main.cc

INC_DIR := $(PRG_DIR)
LIBS    += base cbe cbe_cxx sha256_4k
