rm -f work*cf
ghdl -a -g --std=08 AND23.vhdl
#;ghdl -e --std=08 AND23_TB
ghdl -r --std=08  AND23_TB --vcd=AND23.vcd

