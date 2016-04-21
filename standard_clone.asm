;
; STANDARD CLONE
;

; Code and graphics by TMR
; Music by Odie


; A quick, written-from-scratch copy of various "bog standard" C64
; demos - coded for C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Memory Map
; $0801 - $0fff		program code/data
; $1000 - $1fff		music
; $2000 - $37ff		scrolling message
; $3800 - $3fff		character set
; $4000 - $5000		sprite characters
; $5000 - $59ff		animation and logo sprites
; $5a00 - $5bff		scroller sprites
; $6000 - $8710		bitmapped image


; Select an output filename
		!to "standard_clone.prg",cbm


; Pull in the binary data
		* = $1000
music		!binary "binary\futureshock_remix.prg",,2

		* = $3800
char_data	!binary "binary\plain_font_8x8.chr"

		* = $4000
		!binary "binary\sprite_font.spr"

		* = $5000
		!binary "binary\sprites.spr"

		* = $6000
		!binary "binary\1084_monitor.kla",,2


; Raster split positions
raster_1_pos	= $00
raster_2_pos	= $1a
raster_3_pos	= $c9
raster_4_pos	= $ef

; Label assignments
raster_num	= $50
sync		= $51
msb_temp	= $52

char_count	= $53
scroll_pos	= $54		; two bytes used
char_data_pos	= $56		; two bytes used
grt_scrl_pos	= $58		; two bytes used
greets_count	= $5a
greet_col_count	= $5b
grt_col_timer	= $5c
anim_timer	= $5d
cursor_timer	= $5e

scroll_ram	= $5a09

; Add a BASIC startline
		* = $0801
		!word code_start-2
		!byte $40,$00,$9e
		!text "2066"
		!byte $00,$00,$00


; Entry point for the code
		* = $0812

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Initialise the colour for our picture
		ldx #$00
koala_init	lda $7f40,x
		sta $5c00,x
		lda $8040,x
		sta $5d00,x
		lda $8140,x
		sta $5e00,x
		lda $8228,x
		sta $5ee8,x
		lda $8328,x
		sta $d800,x
		lda $8428,x
		sta $d900,x
		lda $8528,x
		sta $da00,x
		lda $8610,x
		sta $dae8,x
		inx
		bne koala_init

; Initialise some of our own labels
		lda #$01
		sta raster_num

		lda #$00
		sta anim_timer
		sta cursor_timer
		sta char_count
		jsr scroll_reset
		jsr grt_scrl_reset

; Set up the music driver
		jsr music+$00


; Restart the interrupts
		cli

; Movement loop for the greetings
main_loop	jsr greets_fetch

		lda #$00
		sta greets_count

greets_move_1	jsr sync_wait
		jsr greets_right
		dec greets_count
		bne greets_move_1

		ldy #$32
		jsr sync_wait_long

		lda #$00
		sta greets_count

greets_move_2	jsr sync_wait
		jsr greets_left
		dec greets_count
		bne greets_move_2

		jsr greets_fetch

		lda #$00
		sta greets_count

greets_move_3	jsr sync_wait
		jsr greets_left
		dec greets_count
		bne greets_move_3

		ldy #$32
		jsr sync_wait_long

		lda #$00
		sta greets_count

greets_move_4	jsr sync_wait
		jsr greets_right
		dec greets_count
		bne greets_move_4

		jmp main_loop


; Move the greeting sprites left
greets_left	lda #$01
		sta msb_temp

		ldx #$00
gl_loop		ldy sprite_x_grt,x
		dey
		cpy #$ff
		bne gl_msb_skip
		lda sprite_x_grt+$08
		eor msb_temp
		sta sprite_x_grt+$08
gl_msb_skip	tya
		sta sprite_x_grt,x
		asl msb_temp
		inx
		cpx #$08
		bne gl_loop

		rts

; Move the greeting sprites right
greets_right	lda #$01
		sta msb_temp

		ldx #$00
gr_loop		ldy sprite_x_grt,x
		iny
		bne gr_msb_skip
		lda sprite_x_grt+$08
		eor msb_temp
		sta sprite_x_grt+$08
gr_msb_skip	tya
		sta sprite_x_grt,x
		asl msb_temp
		inx
		cpx #$08
		bne gr_loop

		rts

; Read a new line of greetings
greets_fetch	ldx #$00
		ldy #$00
grt_scrl_mread	lda (grt_scrl_pos),y
		cmp #$ff
		bne grt_scrl_okay
		jsr grt_scrl_reset
		jmp grt_scrl_mread

grt_scrl_okay	sta sprite_dp_grt,x

		inc grt_scrl_pos+$00
		bne *+$04
		inc grt_scrl_pos+$01

		inx
		cpx #$11
		bne grt_scrl_mread


; Wait for sync to be set from the interrupt
sync_wait	lda #$00
		sta sync
sw_loop		cmp sync
		beq sw_loop
		rts

; Longer sync wait
sync_wait_long	jsr sync_wait
		dey
		bne sync_wait_long
		rts

; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num
;		sta $d020

		cmp #$02
		bne *+$05
		jmp irq_rout2

		cmp #$03
		bne *+$05
		jmp irq_rout3

		cmp #$04
		bne *+$05
		jmp irq_rout4

; Raster split 1
irq_rout1

; Set up the display
		lda #$00
		sta $d020
		sta $d021
		sta $7fff

		lda #$c6
		sta $dd00

		lda #$3b
		sta $d011
		lda #$18
		sta $d016
		lda #$78
		sta $d018

; Position the sprites for the upper border
		lda #$ff
		sta $d015

		lda #$e3
		sta $d01c
		lda #$1c
		sta $d01d

		ldx #$00
		lda #$1b
sprite_y_set_1	sta $d001,x
		inx
		inx
		cpx #$10
		bne sprite_y_set_1

		ldx #$00
		ldy #$00
sprite_x_set_1	lda sprite_x_ub,x
		sta $d000,y
		iny
		iny
		inx
		cpx #$09
		bne sprite_x_set_1

		ldx #$00
sprite_dp_set_1	lda sprite_dp_ub,x
		sta $5ff8,x
		lda sprite_col_ub,x
		sta $d027,x
		inx
		cpx #$08
		bne sprite_dp_set_1

		lda #$0b
		sta $d025
		lda #$01
		sta $d026

; Set interrupt handler for split 2
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 2
irq_rout2	ldx #$0e
		dex
		bne *-$01
		nop

; Colour splitter for the upper border sprites
		ldx #$00
ub_loop		lda #$06
		ldy #$03
		sta $d025
		sty $d026

		nop
		nop
		nop
		nop
		nop
		nop

		lda #$0b
		ldy #$01
		sta $d025
		sty $d026

		inx
		cpx #$15
		bne ub_loop

; Position sprites for the monitor in the picture
		ldx #$00
		lda #$6d
sprite_set_2	lda sprite_pos_mon,x
		sta $d000,x
		inx
		cpx #$11
		bne sprite_set_2

		ldx #$00
sprite_dp_set_2	lda sprite_dp_mon,x
		sta $5ff8,x
		lda sprite_col_mon,x
		sta $d027,x
		inx
		cpx #$08
		bne sprite_dp_set_2

		lda #$00
		sta $d017
		sta $d01c
		sta $d01d

; Move the scrolling message
		ldx #$00
scroll_mover	asl scroll_ram+$1c0,x

		rol scroll_ram+$017,x
		rol scroll_ram+$016,x
		rol scroll_ram+$015,x

		rol scroll_ram+$194,x
		rol scroll_ram+$193,x
		rol scroll_ram+$192,x

		rol scroll_ram+$151,x
		rol scroll_ram+$150,x
		rol scroll_ram+$14f,x

		rol scroll_ram+$10e,x
		rol scroll_ram+$10d,x
		rol scroll_ram+$10c,x

		rol scroll_ram+$0cb,x
		rol scroll_ram+$0ca,x
		rol scroll_ram+$0c9,x

		rol scroll_ram+$088,x
		rol scroll_ram+$087,x
		rol scroll_ram+$086,x

		rol scroll_ram+$045,x
		rol scroll_ram+$044,x
		rol scroll_ram+$043,x

		rol scroll_ram+$002,x
		rol scroll_ram+$001,x
		rol scroll_ram+$000,x
		inx
		inx
		inx
		cpx #$15
		bne scroll_mover

; Do we need to copy a new character to the scroller?
		ldx char_count
		inx
		cpx #$08
		bne cc_xb

; Yes, so fetch a byte from the scroll text
		ldy #$00
scroll_mread	lda (scroll_pos),y
		bne scroll_okay
		jsr scroll_reset
		jmp scroll_mread

; Configure the character copier
scroll_okay	sta char_data_pos+$00
		lda #$00
		asl char_data_pos+$00
		rol
		asl char_data_pos+$00
		rol
		asl char_data_pos+$00
		rol
		clc
		adc #>char_data
		sta char_data_pos+$01

; And copy the definition
		ldx #$00
		ldy #$00
scroll_def_copy	lda (char_data_pos),y
		sta scroll_ram+$1c0,x
		iny
		inx
		inx
		inx
		cpx #$18
		bne scroll_def_copy

; Nudge the scroller onto the next character
		inc scroll_pos+$00
		bne *+$04
		inc scroll_pos+$01

		ldx #$00
cc_xb		stx char_count

; Update the greeting sprite colours
		ldx grt_col_timer
		inx
		cpx #$03
		bne gct_xb

		ldx #$06
gp_loop		lda sprite_col_grt+$00,x
		sta sprite_col_grt+$01,x
		dex
		bpl gp_loop

		ldx greet_col_count
		inx
		cpx #$1b
		bne *+$04
		ldx #$00
		stx greet_col_count

		cpx #$08
		bcc *+$04
		ldx #$07

		lda greet_cols,x
		sta sprite_col_grt+$00

		ldx #$00
gct_xb		stx grt_col_timer

; Play the music
		jsr music+$03

; Send a message to the runtime code
		lda #$01
		sta sync

; Set interrupt handler for split 3
		lda #$03
		sta raster_num
		lda #raster_3_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 3
irq_rout3

; Position the sprites for the greetings
		ldx #$00
		lda #$d6
sprite_y_set_3	sta $d001,x
		inx
		inx
		cpx #$10
		bne sprite_y_set_3

		ldx #$00
		ldy #$00
sprite_x_set_3	lda sprite_x_grt,x
		sta $d000,y
		iny
		iny
		inx
		cpx #$09
		bne sprite_x_set_3

		ldx #$00
sprite_dp_set_3	lda sprite_dp_grt,x
		sta $5ff8,x
		lda sprite_col_grt,x
		sta $d027,x
		inx
		cpx #$08
		bne sprite_dp_set_3

; Set interrupt handler for split 4
		lda #$04
		sta raster_num
		lda #raster_4_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


		* = ((*/$100)+1)*$100

; Raster split 4
irq_rout4

; Position the sprites for the lower border
		ldx #$00
		lda #$fc
sprite_y_set_4	sta $d001,x
		inx
		inx
		cpx #$10
		bne sprite_y_set_4

		ldx #$00
		ldy #$00
sprite_x_set_4	lda sprite_x_lb,x
		sta $d000,y
		iny
		iny
		inx
		cpx #$09
		bne sprite_x_set_4

		ldx #$00
sprite_dp_set_4	lda sprite_dp_lb,x
		sta $5ff8,x
		lda sprite_col_lb,x
		sta $d027,x
		inx
		cpx #$08
		bne sprite_dp_set_4

		lda #$0b
		sta $d025
		lda #$01
		sta $d026

		lda #$80
		sta $d01c
		lda #$7f
		sta $d01d

; Open the upper and lower borders
		lda #$f9
		cmp $d012
		bne *-$03
		lda #$34
		sta $d011

		lda #$fc
		cmp $d012
		bne *-$03
		lda #$3b
		sta $d011

; Open the side borders
		ldx #$0f
		dex
		bne *-$01

		ldx #$00
sb_loop		dec $d016
		inc $d016

		ldy spr_lb_x,x
		sty $d004
		lda spr_lb_col,x
		sta $d029

		bit $ea
		nop
		nop
		nop
		nop

		inx
		cpx #$12
		bne sb_loop

; Update sprite animations
		ldx anim_timer
		inx
		cpx #$03
		bne at_xb

		lda sprite_dp_ub+$00
		clc
		adc #$01
		and #$07
		ora #$48
		sta sprite_dp_ub+$00

		clc
		adc #$03
		and #$07
		ora #$48
		sta sprite_dp_ub+$07

		clc
		adc #$01
		and #$07
		ora #$40
		sta sprite_dp_lb+$07

		ldx #$00
at_xb		stx anim_timer

; Update cursor flash animation
		ldx cursor_timer
		inx
		cpx #$10
		bne ct_xb

		lda sprite_dp_mon
		eor #$01
		sta sprite_dp_mon

		ldx #$00
ct_xb		stx cursor_timer

; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012


; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti


; Subroutines to reset the scrolling messages
scroll_reset	lda #<scroll_text
		sta scroll_pos+$00
		lda #>scroll_text
		sta scroll_pos+$01
		rts

grt_scrl_reset	lda #<scroll_text_2
		sta grt_scrl_pos+$00
		lda #>scroll_text_2
		sta grt_scrl_pos+$01
		rts


		* = ((*/$100)+1)*$100

; Sprite positioning data for the upper border logo
sprite_x_ub	!byte $18,$7c,$94,$94,$94,$c4,$dc,$40
		!byte $80

sprite_col_ub	!byte $05,$0e,$02,$0a,$0f,$0e,$0e,$05

sprite_dp_ub	!byte $48,$50,$51,$52,$53,$54,$55,$48


; Sprite positioning data for the 1084 monitor
sprite_pos_mon	!byte $88,$6d,$a0,$6d,$b8,$6d,$d0,$6d
		!byte $73,$ae,$9b,$b0,$b3,$b0,$ec,$ac
		!byte $00

sprite_dp_mon	!byte $57,$58,$59,$5a,$5b,$5c,$5d,$5e

sprite_col_mon	!byte $0e,$0e,$0e,$0e,$00,$06,$06,$02


; Sprite positioning data for the greetings
sprite_dp_grt	!byte $00,$00,$00,$00,$00,$00,$00,$00

sprite_x_grt	!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00

sprite_col_grt	!byte $01,$0d,$01,$0d,$01,$0d,$01,$0d


; Sprite positioning data for the lower border scroller
sprite_x_lb	!byte $58,$28,$f0,$88,$b8,$e8,$18,$ac
		!byte $44

sprite_col_lb	!byte $04,$0e,$06,$03,$0f,$0a,$08,$07

sprite_dp_lb	!byte $6a,$69,$68,$6b,$6c,$6d,$6e,$40


; Table of sprite X positions and colours for recycling sprite 0
spr_lb_x	!byte $f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
		!byte $48,$48,$48,$48,$48,$48,$48,$48
		!byte $48,$48,$48,$48,$48

spr_lb_col	!byte $06,$06,$06,$06,$06,$06,$06,$06
		!byte $09,$09,$09,$09,$09,$09,$09,$09
		!byte $09,$09,$09,$09,$09


; That everso important scrolling message
		* = $2000

scroll_text	!scr "welcome to    --- standard clone ---    "
		!scr "a mash-up of inspiration from various "
		!scr $22,"bog standard",$22," demos released during the "
		!scr "1980s, in particular future shock and metal bar 2 "
		!scr "by the borderzone dezign team, scoop's real "
		!scr "sky-runner, xess 1 - rendezvous from xess and "
		!scr "tangent's airwolf demo..."
		!scr "          "

		!scr "code and graphics as always from t.m.r and musical "
		!scr "delights are provided this time by odie (a cover of "
		!scr "demon's future shock soundtrack)."
		!scr "          "

		!scr "some people are sofa sceners, but the majority of "
		!scr "this code was hammered out during a six hour coach "
		!scr "journey!    as with all previous c64cd releases there "
		!scr "isn't anything amazing going on technically, but take a "
		!scr "closer look at this scroller and count how many sprite "
		!scr "colours are used in the upper border!    if anyone is "
		!scr "wondering, the source code is available via github and "
		!scr "i'll happily answer questions as well...     just get "
		!scr "in touch through whichever means suits you best, "
		!scr "although i was never particularly good at mail trading!"
		!scr "          "

		!scr "the greetings list above is now alphabetically "
		!scr "sorted rather than in the order that the blog covered "
		!scr "people's demos.  "
		!scr "my apologies to the commandos for using the singular "
		!scr "of their name but, in keeping with how tangent's "
		!scr "airwolf demo did things, there are only eight sprites "
		!scr "in play!"
		!scr "          "

		!scr "and, since there aren't any greetings in this scroller, "
		!scr "there isn't anything else left to say so i'll finish by "
		!scr "getting in a cheap and cheerful plug for the c64cd "
		!scr "website at http://c64crapdebunk.wordpress.com/ before "
		!scr "bidding you all farewell until next time!"
		!scr "          "

		!scr "this was t.m.r for c64cd on the 21st of april 2016"
		!scr ".... ... .. .  .   ."
		!scr "                "

		!byte $00		; end of text marker

; Greetings text with sprite X co-ordinates
scroll_text_2	!scr "hello to"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "1001crew"
		!byte $50,$5e,$76,$86,$b8,$d0,$e8,$00
		!byte $7f

		!scr "ash&dave"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr " border "
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "  zone  "
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "the four"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "horsemen"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "  hdm   "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "  hcs   "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "meanteam"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr " paul,  "
		!byte $6a,$82,$9a,$b2,$ca,$e1,$f0,$00
		!byte $7f

		!scr "shandor "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "and matt"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr " pulse  "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "reset 86"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr " rob h  "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr " scoop  "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr " stoat  "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr " & tim  "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "tangent "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "thalamus"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "commando"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "the gps "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "we music"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "  xess  "
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "  yak   "
		!byte $64,$7c,$94,$ac,$c4,$dc,$f4,$00
		!byte $7f

		!scr "        "
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "  anti  "
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "greeting"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "   to   "
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "c64hater"
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!scr "        "
		!byte $58,$70,$88,$a0,$b8,$d0,$e8,$00
		!byte $7f

		!byte $ff		; end of text marker


; Colour table for the greeting sprites
greet_cols	!byte $01,$0d,$03,$05,$0e,$04,$0b,$06
