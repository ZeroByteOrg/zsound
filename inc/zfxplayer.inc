.ifndef REV
	.define REV 39
.endif

;Macros to import the zsound version-tagged symbols as non-tagged symbols.
;Zsound contains symbols for r38 and r39 compatible builds, and exports them
;as symbol38 or symbol39. The macros "undo" that tag.
; eg: playmusic39: --> playmusic:

.ifndef __ZSOUND_TAG_SYMBOLS__
__ZSOUND_TAG_SYMBOLS__ = 1
.macro IMPORT_TAGGED symbol
	.import	.ident(.sprintf("%s%d",symbol,REV))
	.ident(.sprintf("%s",symbol))	:=	.ident(.sprintf("%s%d",symbol,REV))
.endmacro

.macro IMPORTZP_TAGGED symbol
	.importzp .ident(.sprintf("%s%d",symbol,REV))
	.ident(.sprintf("%s",symbol))	:=	.ident(.sprintf("%s%d",symbol,REV))
.endmacro
.endif

; scope causes the lines below to throw errors
; "Constant expression expected" once for each
; line of the IMPORT_TAGGED macro

;.scope zfx			
IMPORT_TAGGED "update"
IMPORT_TAGGED "init"
IMPORT_TAGGED "play"
;.endscope

; to-do: move patchym into a separate .inc file for API
IMPORT_TAGGED "patchym"

