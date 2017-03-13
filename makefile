OCAMLPREFIX=
OCAMLC=         $(OCAMLPREFIX)ocamlc
OCAMLOPT=       $(OCAMLPREFIX)ocamlopt.opt
OCAMLYACC=      $(OCAMLPREFIX)ocamlyacc -v
OCAMLLEX=       $(OCAMLPREFIX)ocamllex
OCAMLDEP=       $(OCAMLPREFIX)ocamldep
OCAMLINCLUDES=
OCAMLFLAGS=     -warn-error a $(OCAMLINCLUDES)
OCAMLC=         $(OCAMLPREFIX)ocamlc
OCAMLOPT=       $(OCAMLPREFIX)ocamlopt.opt
%.ml: %.mll
	$(OCAMLLEX) $*.mll
%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly
%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml
%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLFLAGS) -c $*.ml
%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $*.mli
%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $*.ml
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $*.ml
all: analyzer
AUTOGEN_ML=	java_parser.ml java_lexer.ml
AUTOGEN_MLI=    java_parser.mli
AUTOGEN= $(AUTOGEN_ML) $(AUTOGEN_MLI)
ML_FILES=	localizing.ml \
		java_syntax.ml \
		$(AUTOGEN_ML) \
		simple_java_syntax.ml \
		simple_java_translate.ml \
		simple_java_display.ml \
		simple_java_interpreter.ml \
		static_analysis_init_vars.ml \
		static_analysis_typing.ml \
		domains.ml \
		abstract_domain_with_boolean.ml \
		abstract_domain.ml \
		domain_congruences.ml \
		domain_constants.ml \
		domain_intervals.ml \
		domain_congruences_and_intervals.ml \
		static_analysis_variables.ml \
		analyzer.ml
CMO_FILES=	$(ML_FILES:%.ml=%.cmo)
CMX_FILES=      $(ML_FILES:%.ml=%.cmx)
analyzer: $(CMX_FILES) $(AUTOGEN)
	ocamlopt $(CMX_FILES) -o analyzer

depend: $(AUTOGEN_ML) $(ML_FILES)
	ocamldep $(OCAMLINCLUDES) *.mli *.ml */*.mli */*.ml > depend
clean:
	rm -f *.cmo *.cmi *.cmx */*.cmi */*.cmo */*.cmx && \
	rm -f *.o $(AUTOGEN_ML) $(AUTOGEN_MLI) analyzer depend *~ \
	rm -r *.output
include depend
