;.scope zfx
.import update
.import init
.import play
;.endscope

; to-do: move patchym into a separate .inc file for API
.ifndef __PATCHYM__
__PATCHYM__ = 1
.import patchym
.endif
