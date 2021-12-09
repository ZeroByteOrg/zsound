digi_coin:
	.addr	$a000
	.byte	1
	.byte	$0f
	.word	133
	.word	1739
	.byte	0		; high byte of 24-bit size
	.byte	PCM_RATE_8000
digi_start:
	.addr	$a000 + 1739
	.byte	1
	.byte	$0f
	.word	133
	.word	33830
	.byte	0
	.byte	PCM_RATE_8000
