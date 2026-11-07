all-lex: lex all

lex:
	$(MAKE) -C ../lex

test: Main
	./Main < test.sml

main.mk:
	smlsharp -MMm Main.smi > $@

clean:
	rm -f *.o Main *~

include main.mk
