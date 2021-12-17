.ifndef REV
	REV = 39
.endif

	; import symbols as symbol + REV  (e.g. helloworld38)
	; alias them with the non-rev symbol (i.e. helloworld := helloworld38)


	.import	.ident(.sprintf("%s%d","init_pcm",REV))
	init_pcm	:=	.ident(.sprintf("%s%d","init_pcm",REV))
	.import	.ident(.sprintf("%s%d","start_digi",REV))
	start_digi	:=	.ident(.sprintf("%s%d","start_digi",REV))
	.import	.ident(.sprintf("%s%d","play_pcm",REV))
	play_pcm	:=	.ident(.sprintf("%s%d","play_pcm",REV))
	.import	.ident(.sprintf("%s%d","stop_pcm",REV))
	stop_pcm	:=	.ident(.sprintf("%s%d","stop_pcm",REV))

