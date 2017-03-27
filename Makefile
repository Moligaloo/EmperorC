.PHONY : test

qinhuang:
	cd ./tools/qinhuang && zip -r qinhuang.love . && mv qinhuang.love ../..

test: 
	busted test/test.lua