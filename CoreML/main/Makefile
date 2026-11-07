all-lex: lex all

lex:
	$(MAKE) -C ../lex

test: Main
	./Main < test.sml

clean:
	rm -f *.o Main *~

include main.mk
