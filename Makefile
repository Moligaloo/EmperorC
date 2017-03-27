.PHONY : test qinhuang

qinhuang:
	cd ./qinhuang && zip -r qinhuang.love . && mv qinhuang.love ..

test: 
	busted test/test.lua