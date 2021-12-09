digi_coin:
	.addr	$a000
	.byte	1
	.byte	$0f
	.word	205
	.word	2653
	.byte	0		; high byte of 24-bit size
	.byte	PCM_RATE_12207
digi_start:
	.addr	$a000 + 2653
	.byte	1
	.byte	$0f
;	.word	205
	.word	2048
	.word	51620
	.byte	0
	.byte	PCM_RATE_12207
