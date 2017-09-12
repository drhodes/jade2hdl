PRELUDE=$JADE2HDL/app-data/vhdl/prelude.vhdl
echo $PRELUDE

ghdl -a -g --std=08 $PRELUDE *.vhdl
ghdl -r --std=08 mod_user_${1}_tb --vcd=mod_user_${1}_tb.vcd
