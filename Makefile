EMACS ?= emacs
SRCS = exwm-cc-lib.el exwm-cc-nm.el exwm-cc-audio.el exwm-cc-power.el \
	exwm-cc-bt.el exwm-cc-disk.el exwm-cc-display.el exwm-cc-wg.el \
	exwm-cc-polkit.el exwm-cc.el

.PHONY: compile test clean

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(SRCS) exwm-cc-tests.el

test:
	$(EMACS) -Q --batch -L . -l exwm-cc-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
