

ghdl -a -g --std=08 *.vhdl && ghdl -r --std=08 mod_user_${1}_tb --vcd=mode_user_${1}_tb.vcd
