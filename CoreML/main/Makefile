all-lex: lex all

lex:
	$(MAKE) -C ../lex

test:
	echo 'val x = 1;' | ./Main

clean:
	rm -f *.o Main *~

include main.mk
