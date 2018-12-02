	device zxspectrum128
partyversion equ 1
	org	#6000
SnaStart:
	display $
	ld hl,dhlogoattr
	ld de,#5800
	ld bc,256
	ldir

	ld	hl,pizdauskas2
	ld	de,syuda
	ld	bc,256*4
	ldir
	ld	(hl),#c9

	ei
	ld hl,muza
	call pluyer+3

ZaLoop
	xor a
	out (#fe),a
	halt
;	inc a
	out (#fe),a
	call printer
	call	perebroska
	call	udalenie
	call pluyer+5
	jr ZaLoop

perebroska:
	ld	a,#ff
	inc	a
	and	#07
	ld	(perebroska+1),a
	push	af

	add	#40
	ld	(kakoj_d+1),a

	pop	af
	add	high dhlogo
	ld	(kakoj_h+1),a


	ld	l,0
	ld	e,l

kakoj_d:
	ld	d,#ff
kakoj_h:
	ld	h,#ff

	dup	256
	ldi
	edup
	ret

udalenie:
kakaya:	ld	a,#3
	inc	a
	and	%00000111
	ld	(kakaya+1),a

	ld	h,high maskotablichka
	add	a
	ld	l,a
	ld	e,(hl)
	inc	l
	ld	d,(hl)
	ld	l,0

	ld	a,d
	ld	(pizdauskas+1),a

	ld	a,#40
	add	e
	ld	h,a
	call	pizdauskas

	ret

pizdauskas:
	ld	b,#fe
pizdauskas2:
	ld	a,b
	and	(hl)
	ld	(hl),a
	inc	l
syuda:
	ds	(256*4)+2

printer:
;0 - pechataem
;1 - vivodim
;2 - zhdem anykey

	ld	a,0
	or	a
	jr	z,pechataem
	dec a
	jp	z,vivodos
	jp	zhdun
pechataem:
	ld	ixl,16
curtex01:

currenttext:
	ld	hl,samtext

curtex03:
	ld	a,(hl)
	inc	hl
	ld	(currenttext+1),hl

	cp 1
	jr nz,curtex02
	ld	a,(hl)
	ld	(currentcolor+1),a
	inc	hl
	jr	curtex03

curtex02:
	add	a,a
	ld	d,high font
	ld	e,a

currentscr:
	ld	hl,shadowscr
	push hl
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a:dec e:dec l:inc d:inc h
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a:dec e:dec l:inc d:inc h
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a:dec e:dec l:inc d:inc h
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a:dec e:dec l:inc d:inc h
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a:dec e:dec l:inc d:inc h
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a:dec e:dec l:inc d:inc h
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a:dec e:dec l:inc d:inc h
	ld a,(de) : ld (hl),a:inc e:inc l:ld a,(de):ld (hl),a
	pop hl
	ld	a,l
	add	2
	ld	l,a
	or	a
	jr	nz,curtex04
	ld	a,h
	add	8
	ld	h,a
curtex04:
	ld	(currentscr+1),hl
currentattr:
	ld	hl,shadowattr
currentcolor:
	ld	a,#47
	ld	(hl),a
	inc	l
	ld	(hl),a
	inc	hl
	ld	(currentattr+1),hl

	dec ixl
	jp nz,curtex01

strochka:
	ld	a,%00000000
	inc	a
	and	%00001111
	ld	(strochka+1),a
	or	a
	ret	nz
newpage:
	ld	a,1
	ld	(printer+1),a

	ld	hl,(currenttext+1)
	ld	a,(hl)
	or	a
	jr	nz,.l2

	if partyversion
	ld hl,partyloop
	else
	ld	hl,samtext
	endif

	ld	(currenttext+1),hl
.l2
	ld	hl,shadowscr
	ld	(currentscr+1),hl
	ld	hl,shadowattr
	ld	(currentattr+1),hl
	ret

vivodos:
	ld a,0
	ld h,0
	ld l,a
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,addrtabl
	add hl,de
	push hl
	pop ix

	ld e,(ix+00)
	ld d,(ix+01)
	ld l,(ix+02)
	ld h,(ix+03)

	dup 7

	push hl
	push de
	dup 32
	ldi
	edup
	pop de
	pop hl
	inc h
	inc d
	edup

	dup 32
	ldi
	edup

	ld e,(ix+04)
	ld d,(ix+05)
	ld l,(ix+06)
	ld h,(ix+07)
	dup 32
	ldi
	edup

	ld a,(vivodos+1)
	inc a
	and #0F
	ld (vivodos+1),a
	or a
	ret nz
	ld a,2
	ld (printer+1),a
	ret

zhdun:
vremya equ 10*50
	ld hl,vremya
	dec hl
	ld (zhdun+1),hl
	ld a,h
	or l
	ret nz
	ld hl,vremya
	ld (zhdun+1),hl
	ld a,0
	ld (printer+1),a
	ret

	align	#100
font:
	incbin "font.bin"

	align	#100
shadowscr:
	ds	#1000
shadowattr:
	ds	#200

dhlogoattr:
	incbin	"dhlogoattr.bin"

	align	#100
dhlogo:
	incbin	"dhlogo.bin"


addrtabl:
	DW	#4800+#0000+0*32,shadowscr+#0000+0*32,#5900+00*32,shadowattr+00*32 ;1
	DW	#4800+#0000+1*32,shadowscr+#0000+1*32,#5900+01*32,shadowattr+01*32 ;16
	DW	#4800+#0000+2*32,shadowscr+#0000+2*32,#5900+02*32,shadowattr+02*32 ;2
	DW	#4800+#0000+3*32,shadowscr+#0000+3*32,#5900+03*32,shadowattr+03*32 ;15
	DW	#4800+#0000+4*32,shadowscr+#0000+4*32,#5900+04*32,shadowattr+04*32 ;3
	DW	#4800+#0000+5*32,shadowscr+#0000+5*32,#5900+05*32,shadowattr+05*32 ;14
	DW	#4800+#0000+6*32,shadowscr+#0000+6*32,#5900+06*32,shadowattr+06*32 ;4
	DW	#4800+#0000+7*32,shadowscr+#0000+7*32,#5900+07*32,shadowattr+07*32 ;13
	DW	#4800+#0800+0*32,shadowscr+#0800+0*32,#5900+08*32,shadowattr+08*32 ;5
	DW	#4800+#0800+1*32,shadowscr+#0800+1*32,#5900+09*32,shadowattr+09*32 ;12
	DW	#4800+#0800+2*32,shadowscr+#0800+2*32,#5900+10*32,shadowattr+10*32 ;6
	DW	#4800+#0800+3*32,shadowscr+#0800+3*32,#5900+11*32,shadowattr+11*32 ;11
	DW	#4800+#0800+4*32,shadowscr+#0800+4*32,#5900+12*32,shadowattr+12*32 ;7
	DW	#4800+#0800+5*32,shadowscr+#0800+5*32,#5900+13*32,shadowattr+13*32 ;10
	DW	#4800+#0800+6*32,shadowscr+#0800+6*32,#5900+14*32,shadowattr+14*32 ;8
	DW	#4800+#0800+7*32,shadowscr+#0800+7*32,#5900+15*32,shadowattr+15*32 ;9

	align #100
maskotablichka:
	db	0,%10101010
	db	1,%01010101
	db	2,%10101010
	db	3,%01010101
	db	4,%10101010
	db	5,%01010101
	db	6,%10101010
	db	7,%01010101

	db	0,%01010101
	db	1,%10101010
	db	2,%01010101
	db	3,%10101010
	db	4,%01010101
	db	5,%10101010
	db	6,%01010101
	db	7,%10101010

pluyer:
	include "pt3xplayer.asm"
	display "poslepluera = ",$
muza:
	incbin "2017.pt3"
	display "tuta = ",$

samtext:
	db	1,#47,"                "
	db	"                "
	db	"                "
	db	"                "
	db	"   HELLO ",1,%11010111,"CC17",1,#47,"   "
	db	"                "
	DB	"    VISITORS    "
	db	"                "
	db	"   AND  OTHER   "
	db	"                "
	db	"EIGHTBIT PEOPLES"
	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"                "

	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	" DRUNKEN CLOWNS "
	db	"                "
	db	"  FROM  ",1,%01111001,"Q-BONE",1,#47,"  "
	db	"                "
	db	"  VERY PROUDLY  "
	db	"                "
	db	"    PRESENT     "
	db	"                "
	db	"                "
	db	"                "
	db	"                "







	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"  ",1,%01111000,"D",1,%01111010,"I",1,%01111000,"HALT  2018",1,#47,"  "
	db	"                "
	db	" ",1,%01111000,"WINTER EDITION",1,#47," "
	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"                "
	db	"                "

	db	"                "
	db	"PLACE:          "
	db	1,%01111000,"NIZHNY NOVGOROD",1,#47," "
	db	"                "
	db	"DATE:           "
	db	1,%01111000,"4-6 JANUARY 2018"
	db	1,#43,"                "
	db	"VODKA,          "
	db	"BEER,           "
	db	"SHASHLYK,       "
	db	"SAMOGON         "
	db	"I PROCHIE       "
	db	"NISHTYAKI       "
	db	"                "
	db	"                "
	db	"                "

	db	"                "
	db	1,%01010000,"    ACHTUNG     "
	db	1,#42,"                "
	db	"                "
	db	"                "
	db	"NO COSPLAY      "
	db	"                "
	db	"NO SOCIAL       "
	db	"    RESTRICTIONS"
	db	"                "
	db	"PURE DEMOSCENE  "
	db	"            ONLY"
	db	"                "
	db	"                "
	db	"                "
	db	"                "


	db	"                "
	db	1,%01010000,"    ACHTUNG     "
	db	1,#42,"                "
	db	"                "
	db	"ALCOHOL  BROUGHT"
	db	"WITH YOURSELF IS"
	db	"ALLOWED         "
	db	"                "
	db	"YOU  CAN   DRINK"
	db	"    ANYTHING,   "
	DB	"    ANYWHERE    "
	db	"AT ANY  TIME AND"
	db	"AS  MUCH AS  YOU"
	db	"CAN             "
	db	"                "
	db	"                "

	DB	"                "
	DB	1,%01001111," Q-BONE GREETS  "
	DB	1,#00,"                "
	DB	1,#47,"CONSCIOUSNESS ",1,#45,"ZS"
	DB	"                "
	DB	1,#45,"4D  ",1,#47,"HOOY-PROGRAM"
	DB	"                "
	DB	1,#47,"KAKOS NONOS ",1,#45,"NYUK"
	DB	"                "
	DB	1,#45,"ARTX  ",1,#47,"TRIEBKRAFT"
	DB	"                "
	DB	1,#47,"MMCM ",1,#45,"NEDOPC  ",1,#47,"VBI"
	DB	"                "
	DB	1,#45,"GEMBABOYS  ",1,#47,"SKRJU"
	DB	"                "
	DB	1,%01001111," Q-BONE GREETS  "

	DB	1,#00,"ORGANISM - EBLAN"
	DB	1,%01001111," Q-BONE GREETS  "
	DB	1,#00,"                "
	DB	1,#45,"CPU   ",1,#47,"KABARDCOMP"
	DB	"                "
	DB	1,#47,"GOBLINISH ",1,#45,"KPACKU"
	DB	"                "
	DB	1,#45,"SHURAN  ",1,#47,"THESUPER"
	DB	"                "
	DB	1,#47,"DEMARCHE  ",1,#45,"TSLABS"
	DB	"                "
	DB	1,#45,"DEBRIS   ",1,#47,"SIBCREW"
	DB	"                "
	DB	1,#47,"OUTSIDERS  ",1,#45,"QUITE"
	DB	"                "
	DB	1,%01001111," Q-BONE GREETS  "

	DB	1,#00,"                "
	DB	1,%01001111," Q-BONE GREETS  "
	DB	1,#00,"                "
	DB	1,#47,"MOROZ  ",1,#45,"SPECCY.PL"
	DB	"                "
	DB	1,#45,"VINNNY ",1,#47,"OUTSIDERS"
	DB	"                "
	DB	1,#47,"ZEROTEAM ",1,#45,"B-STATE"
	DB	"                "
	DB	1,#45,"TECHNO LAB  ",1,#47,"NUTS"
	DB	"                "
	DB	1,#47,"NOTSOFT   ",1,#45,"EXCESS"
	DB	"                "
	DB	1,#45,"HYPE HYDRA  ",1,#47,"BFOX"
	DB	"                "
	DB	1,%01001111," Q-BONE GREETS  "


	DB	1,#47,"                "
	DB	1,%01010111,"PERSONAL  INVITE"
	DB	1,#00,"                "
	DB	"                "
	DB	1,#47,"   SERZHSOFT    "
	DB	"    ANDY FER    "
	DB	"     KOTSOFT    "
	DB	"      G0BLIN    "
	DB	"       N1K-0    "
	DB	"       BUYAN    "
	DB	"       QUIET    "
	DB	"        MMCM    "
	DB	"        BFOX    "
	DB	"                "
	DB	"     ",1,%11000111,"AND YOU",1,#47,"    "
	DB	"                "
partyloop
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	1,%01001111,"  MORE INFO AT  "
	DB	1,#00,"                "
	DB	1,%01001111," DIHALT.ORG.RU  "
	DB	1,#00,"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "

	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "
	DB	1,%01010111,"  CODE - RASMER "
	DB	1,#00,"                "
	DB	1,%01010111,"  MUSIC - SCL   "
	DB	1,#00,"                "
	DB	1,%01010111,"  Q-BONE 2017   "
	DB	1,#00,"                "
	DB	"SHRIFT SPIZDIL!!"
	DB	"                "
	DB	"                "
	DB	"                "
	DB	"                "


endsamtext
	DB	0
	savebin "invitro.bin",#6000,$-#6000
	savesna "invitro.sna",SnaStart