.ifndef REV
	REV = 39
.endif

	; import symbols as symbol + REV  (e.g. helloworld38)
	; alias them with the non-rev symbol (i.e. helloworld -> helloworld38)
	
	.import	.ident(.sprintf("%s%d","helloworld",REV))
	helloworld	:=	.ident(.sprintf("%s%d","helloworld",REV))

	.import	.ident(.sprintf("%s%d","init_player",REV))
	init_player	:=	.ident(.sprintf("%s%d","init_player",REV))

	.import	.ident(.sprintf("%s%d","stepmusic",REV))
	stepmusic	:=	.ident(.sprintf("%s%d","stepmusic",REV))

	.import	.ident(.sprintf("%s%d","playmusic",REV))
	playmusic	:=	.ident(.sprintf("%s%d","playmusic",REV))

	.import	.ident(.sprintf("%s%d","startmusic",REV))
	startmusic	:=	.ident(.sprintf("%s%d","startmusic",REV))

	.import	.ident(.sprintf("%s%d","stopmusic",REV))
	stopmusic	:=	.ident(.sprintf("%s%d","stopmusic",REV))

	.import	.ident(.sprintf("%s%d","patchym",REV))
	patchym	:=	.ident(.sprintf("%s%d","patchym",REV))

; to-do: move patchym into a separate .inc file for API


