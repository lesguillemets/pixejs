./build/main.js: ./src/main.hs
	hastec -Wall -fno-warn-unused-do-bind -O2 --opt-all ./src/main.hs -o ./build/main.js

clean:
	rm ./build/main.js ./src/*.o ./src/*.hi
