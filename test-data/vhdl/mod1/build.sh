rm -f work*cf
ghdl -a -g --std=08 *.vhdl
ghdl -r --std=08 mod_user_And41_tb --vcd=AND41.vcd

