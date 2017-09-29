trash logs
mkdir logs
stack test -v --fast --no-run-tests

TESTEXE=$(find ./.stack-work/ -type f -name jade2hdl-test)
$TESTEXE

