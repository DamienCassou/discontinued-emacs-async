tests:
	emacs -batch -L ${CURDIR} -l ert -l async-tests.el -f ert-run-tests-batch-and-exit
