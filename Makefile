EMACS ?= emacs
EMACS_QUICK=$(EMACS) --quick --directory .

SOURCE_FILES=*.el
BYTE_CODE=*.elc

COMPILE=--eval "(setq byte-compile-error-on-warn t)" --batch --funcall batch-byte-compile-if-not-done $(SOURCE_FILES)

all: compile

compile:
	$(EMACS_QUICK) $(COMPILE)

clean:
	rm -f $(BYTE_CODE)
