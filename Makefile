./build/main.js: ./src/main.hs
	hastec -Wall -fno-warn-unused-do-bind -O2 --opt-whole-program -v ./src/main.hs -isrc -o ./build/main.pre.js
	closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS --jscomp_off globalThis ./build/main.pre.js > ./build/main.js
	rm ./build/main.pre.js

clean:
	rm ./build/main.js ./src/*.o ./src/*.hi
