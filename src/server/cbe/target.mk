REQUIRES := x86_64

TARGET  := cbe
SRC_CC  := main.cc

# force binder for adainit
SRC_ADS := dummy.ads
LIBS    += spark

INC_DIR := $(PRG_DIR)
LIBS    += base cbe sha256_4k
