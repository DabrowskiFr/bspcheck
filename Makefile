FRAMAC_SHARE := $(shell frama-c-config -print-share-path)
FRAMAC_LIBDIR := $(shell frama-c-config -print-libpath)
PLUGIN_NAME = BSPCheck
PLUGIN_CMO = aux xCFG pidDependent bspcheck_run 
include $(FRAMAC_SHARE)/Makefile.dynamic
