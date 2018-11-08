test: modem_parser.o test.o
	gcc -Os -o test modem_parser.o test.o

clean:
	rm -f *.o test

modem_parser.o: modem_parser.c modem_parser.h
	gcc -Os -c -o modem_parser.o modem_parser.c

test.o: test.c modem_parser.h
	gcc -Os -c -o test.o test.c

