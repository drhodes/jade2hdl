stack clean
find . -type f -name "*.cf" -delete
find . -type f -name "*.vcd" -delete
rm -f *.prof
rm -f test-data/auto-vhdl/*
