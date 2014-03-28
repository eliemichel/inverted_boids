
BIN1=display
SOURCES1=engine.ml display.ml
FLAGS1=-libs graphics,unix

BIN2=invert
SOURCES2=engine.ml invert.ml
FLAGS2=

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m
HIGHLIGHT_COLOR=\033[1m

all: $(BIN1) $(BIN2)

$(BIN1): $(SOURCES1)
	@echo "Building $(BIN1)..."
	ocamlbuild $(FLAGS1) $(BIN1).native
	unlink $(BIN1).native
	mv _build/$(BIN1).native $(BIN1)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"

# Si quelqu'un sait comment éviter cette affreuse réplication de code,
# qu'il n'hésite pas !
$(BIN2): $(SOURCES2)
	@echo "Building $(BIN2)..."
	ocamlbuild $(FLAGS1) $(BIN2).native
	unlink $(BIN2).native
	mv _build/$(BIN2).native $(BIN2)
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"


clean:
	@echo "Cleaning directory..."
	@rm -vrf _build $(BIN1) $(BIN1).native $(BIN2) $(BIN2).native *~
	@echo "$(OK_COLOR)Done.$(NO_COLOR)"

session:
	@echo "Recovering session..."
	@gedit Makefile $(SOURCES1) $(SOURCES2) 2>/dev/null &


.PHONY: all clean session



