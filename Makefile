SHELL = /bin/bash

all: WKB_MODEL 

WKB_MODEL: 
	make -C WKB_MODEL
	
.PHONY: WKB_MODEL 

clean:
		make -C WKB_MODEL clean
 
