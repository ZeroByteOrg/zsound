Placeholder

shell should be nothing but the zsmplayer routines with a simple SYS call API shell. This would be the bare
minimum to load the zsm player as a "driver" which can be called from other programs.
The current code is just a copy of the background player program. It needs to be stripped down.

Idea:
Include a helper program that adds a wedge to add BASIC commands such as MLOAD,MSTART,MSTOP,MSPEED
for instant access to player functionality from BASIC.

Resources to learn how to do the wedge:
https://www.scottjulian.id.au/2020/05/adding-a-basic-wedge/

