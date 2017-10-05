trash logs
mkdir logs
stack test -v --fast --no-run-tests

if [ $? -eq 0 ]
then 
    TESTEXE=$(find ./.stack-work/ -type f -name jade2hdl-test)
    $TESTEXE
fi

