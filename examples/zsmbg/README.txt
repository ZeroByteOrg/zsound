Placeholder

Make a program that adds a BASIC wedge to load/start/stop songs and then
return to BASIC.

Resources to learn how to do the wedge:
https://www.scottjulian.id.au/2020/05/adding-a-basic-wedge/

Initial version will simply be a PRG which loads to golden RAM and sets
an interrupt handler that calls playmusic_IRQ and then proceeds to the
Kernal IRQ handler. Control will be via SYS calls initially.


