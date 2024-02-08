# file "makefile  © P. Cousot 2021"

all: clean compile examples

compile: 
	ocamllex -q lexer.mll
	ocamlc -c abstractSyntax.ml
	ocamlc -c printer.ml
	menhir --infer --explain parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc abstractSyntax.cmo printer.cmo parser.cmo lexer.cmo main.cmo -o main

.PHONY: examples
.IGNORE: examples
examples: main
	@echo "# using the abstract interpreter:"
	@echo "**************************************************"
	@echo "*** ;"
	@echo ";"| ./main
	@echo "**************************************************"
	@echo "*** x = 42;"
	@echo "x = 42;"| ./main
	@echo "**************************************************"
	@echo "*** x = 7; ; break; "
	@echo "x = 7; ; break; "| ./main
	@echo "**************************************************"
	@echo "*** {}" 
	@echo "{}" | ./main
	@echo "**************************************************"
	@echo "*** x=-10-20--40;"
	@echo "x=-10-20--40;" | ./main
	@echo "**************************************************"
	@echo "*** x=1; y=2;"
	@echo "x=1; y=2;" | ./main
	@echo "**************************************************"
	@echo "*** {x=10; ; y=20;}"
	@echo "{x=10; ; y=20;}" | ./main
	@echo "**************************************************"
	@echo "*** if (1-2<3-4-5) x=-x;"
	@echo "if (1-2<3-4-5) x=-x;" | ./main
	@echo "**************************************************"
	@echo "*** if (x<1) if (x<0) x=1; else if (x<0) { x=2; x=3; } else { x=4; x=5; x=6; }"
	@echo "if (x<1) if (x<0) x=1; else if (x<0) { x=2; x=3; } else { x=4; x=5; x=6; }" | ./main
	@echo "**************************************************"
	@echo "*** while (0<1) {}"
	@echo "while (0<1) {}" | ./main
	@echo "**************************************************"
	@echo "*** while (x<1) {}"
	@echo "while (x<1) {}" | ./main
	@echo "**************************************************"
	@echo "*** while (1<x) x=x-1;"
	@echo "while (1<x) x=x-1;" | ./main
	@echo "**************************************************"
	@echo "*** while (3<x) x=x-1;"
	@echo "while (3<x) x=x-1;" | ./main
	@echo "**************************************************"
	@echo "*** while (10<x) x=x-1;"
	@echo "while (10<x) x=x-1;" | ./main
	@echo "**************************************************"
	@echo "*** while (0<1){}"
	@echo "while (0<1){}" | ./main
	@echo "**************************************************"
	@echo "*** while (x<3) { x=x-(-1); if (1<x) break; }"
	@echo "while (x<3) { x=x-(-1); if (1<x) break; }" | ./main
	@echo "**************************************************"
	@echo "*** while (x<3) { if (1<x) break; else x=x-(-1); }"
	@echo "while (x<3) { if (1<x) break; else x=x-(-1); }" | ./main
	@echo "**************************************************"
	@echo "*** x=x-1;while (0<1){x=x-1;if(x<2)break;};"
	@echo "x=x-1;while (0<1){x=x-1;if(x<2)break;};" | ./main
	@echo "**************************************************"
	@echo "*** x=0; while (x<0) { while (x<0) x=x-1; x= 10; }; x= 100;"
	@echo "x=0; while (x<0) { while (x<0) x=x-1; x= 10; }; x= 100;" | ./main
	@echo "**************************************************"
	@echo "*** x=-10; while (x<0) if (x<0) if (0<x) x=-x;"
	@echo "x=-10; while (x<0) if (x<0) if (0<x) x=-x;" | ./main
	@echo "**************************************************"
	@echo "*** x=0; while (x<0) { while (x<0) x=x-1; break; }; x= 100;"
	@echo "x=0; while (x<0) { while (x<0) x=x-1; break; }; x= 100;" | ./main
	@echo "**************************************************"
	@echo "*** x=-10; while (x<0) { x=x-1; break; }; x= 10;"
	@echo "x=-10; while (x<0) { x=x-1; break; }; x= 10;" | ./main
	@echo "**************************************************"
	@echo "*** x=10; while (0<x) x=x-1;"
	@echo "x=10; while (0<x) x=x-1;" | ./main
	@echo "**************************************************"
	@echo "*** while (0<1){ break; x=1; }"
	@echo "while (0<1){ break; x=1; }" | ./main
	@echo "**************************************************"
	@echo "# end"

.PHONY: nonterminating
nonterminating:
	@echo "while (0<1){x=x-1;} x = 42;" | ./main

.PHONY: clean
clean: 
	@/bin/rm -f main *~ .*~ *.cmo *.cmi lexer.ml parser.ml parser.mli parser.conflicts
	@find . -lname '*.*' -delete

.PHONY: help
help:
	@echo "******************************************************************"	
	@echo "make: compile and run the examples"
	@echo "make compile: compile"
	@echo "make examples: run the examples (after compilation)"
	@echo "make nonterminating: run a nonterminating example (^C to stop)"
	@echo "make clean: delete all auxiliary files"
	@echo "make help: this help"
	@echo "******************************************************************"