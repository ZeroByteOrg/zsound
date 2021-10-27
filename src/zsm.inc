.ifndef __ZSM_INC__
.define __ZSM_INC__

.struct SONGPTR
	addr		.word	1
	bank		.byte	1
.endstruct

.macro YM_BUSY_WAIT
:	bit	YM_data
	bmi :-
.endmacro

.endif
