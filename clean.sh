find . -type f -name "*.cf" -delete
find . -type f -name "*.vcd" -delete
rm -f *.prof
rm -rf test-data/auto-vhdl
find ./logs -type f -name "*.log" -delete
find ./docs/graphviz-noodling/ -name '*.png' -delete
