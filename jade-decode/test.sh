trash logs
mkdir logs
stack build -v --fast

TESTEXE=$(find ./.stack-work/ -type f -name jade-decode-test)
$TESTEXE

