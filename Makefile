CC=cobc
TARGET=geekcode

geekcode2.1:
	$(CC) -x geekcode.cob 

all: ${TARGET}

clean:
	rm -f ${TARGET}

install:
	install -m 755 geekcode /usr/local/bin
	install -m 644 geekcode.6.gz /usr/share/man/man6
