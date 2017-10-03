trash logs
mkdir logs

stack build -v --fast

if [ $? -eq 0 ]
then 
    TESTEXE=$(find ./.stack-work/ -type f -name jade-decode-test)
    $TESTEXE
else
    exit;
fi
    

