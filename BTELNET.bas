  10 REM > BTELNET
  20 :
  30 REM See if it's a Master series machine
  40 A%=0:X%=1:id%=((USR&FFF4)AND&FF00)DIV256
  50 REM A%=0:X%=1:id%=FNfx(0,1)
  60 IFid%=3 OR id%=4 OR id%=5 THEN bbc%=TRUE ELSE END
  70 :
  80 REM Setup Local and Gobal ERROR Handling
  90 PROCerr_local(1) : REM Activate LOCAL ERROR HANDLING
 100 ON ERROR PROCmyerror(ERR)
 110 :
 120 REM **********************
 130 REM ***** VARIABES *****
 140 REM **********************
 150 :
 160 REM TELNET Protocol Constants
 170 DO=&FD : REM 253
 180 WONT=&FC :
 190 WILL=&FB : REM 251   will ; do ; dont ; will
 200 DONT=&FE : REM 254
 210 IAC=&FF : REM Command
 220 ECHO=1
 230 NAWS=31 : REM Window Size
 240 SB=&FA : REM 250
 250 SE=&F0 : REM 240
 260 GA=249 : REM Go Ahead
 270 SGA=3
 280 TERM=24 : REM Terminal-Type
 290 SEND=1
 300 IS=0
 310 NOPx = 241  : REM No Operation
 320 DM  = 242  : REM Data Mark
 330 BRKx = 243  : REM Break
 340 IP  = 244  : REM Interrupt process
 350 AO  = 245  : REM Abort output
 360 AYT = 246  : REM Are You There
 370 EC  = 247  : REM Erase Character
 380 EL  = 248  : REM Erase Line
 390 GA  = 249  : REM Go Ahead
 400 xEOR = 25 : REM end or record
 410 REM ABORT= : REM
 420 REM SUSP= : REM
 430 REM EOFx= : REM
 440 :
 450 REM TCP/IP Socket Constants
 460 AF_INET%=2
 470 SOCK_STREAM%=1
 480 MSG_DONTWAIT = 8
 490 :
 500 REM General Constants
 510 tcpbuffersize%=32 :REM Buffer for request
 520 sendbufsize%=240
 530 IACpipesize% = 12
 540 cmd_total% = 18
 550 element_total% = 3
 560 version$ = "0.1"
 570 firsttime_sendbuffer = 0
 580 load_send = 0
 590 :
 600 REM Runtime generated Constants
 610 myterm$ = FNget__termtype
 620 :
 630 REM General Variable
 640 local_echo=TRUE
 650 send_sga=TRUE
 660 screenX = 0
 670 screenY = 0
 680 debug%=0
 690 size%=0
 700 sock%=-1 :REM The listening socket
 710 req% =-1 :REM The request socket
 720 brokenseq% = 0 :REM control sequence split across buffers ?
 730 check_linefeed% = 0
 740 IACpipecounter = 0
 750 IACpipehead = 0
 760 :
 770 REM Array Dimensions - Indirect - For Blocks for addresses, conversions, and OSWord calls
 780 DIM tcpbufferblk tcpbuffersize%
 790 DIM tcpsendblk sendbufsize%
 800 DIM ipaddrblk 16,ipaddrlen 4
 810 DIM tempblk 10
 820 DIM negotiateblk 10
 830 DIM wordblk tcpbuffersize%
 840 DIM nameblk 256
 850 DIM smallblk 2
 860 :
 870 REM Assembly Mem Allocation
 880 DIM fastprint% 45
 890 DIM fastinit% 45
 900 zeropage_ptr=&80
 910 :
 920 REM Array Dimensions - Normal
 930 DIM IACpipe(IACpipesize%)
 940 DIM cmd$(cmd_total%)
 950 DIM cmdproc$(cmd_total%)
 960 DIM cmdhelp$(cmd_total%)
 970 DIM cmdline_array$(element_total%)
 980 :
 990 REM Process command line parameters
1000 REM Set Dynamic Runtime Parameters
1010 A$=FNOS_GetEnv+" "
1020 PROCget__cmdline
1030 save_scrmode%=FNget__mode("SHADOW") : IF save_scrmode% <> 131 THEN MODE 131
1040 :
1050 REM ***************************
1060 REM ***** END OF VARIABES *****
1070 REM ***************************
1080 :
1090 REM ******************************
1100 REM ***** Assemble 6502 Code *****
1110 REM ******************************
1120 PROCasm_fastprint
1130 PROCasm__fastinit
1140 REM ****************************
1150 REM ***** END OF 6502 Code *****
1160 REM ****************************
1170 :
1180 REM **********************
1190 REM *****  MAIN CODE *****
1200 REM **********************
1210 PROCBFont_Load("IBM",128)
1220 :
1230 REM If command line parameters didnt set server$ then enter telnet CLI
1240 REPEAT : IF server$ = "" THEN PROCrun__cmdline ELSE PROCopen_connection : UNTIL sock% > -1
1250
1260 :
1270 :
1280 PROCfast_flushbuf(wordblk,tcpbuffersize%) : PROCfast_flushbuf(tcpbufferblk,tcpbuffersize%)
1290 REM ON ERROR PRINT "ESCAPE": PROCmyerror(ERR) :RUN
1300 :
1310 REM ***************************
1320 REM ******** MAIN LOOP ********
1330 REM ***************************
1340 REPEAT
1350 	charcounter%=0
1360 	key$=INKEY$(0) : IF NOT (key$ = "") THEN PROCprocess_keybuffer(key$)
1370 	size%=FNrecv(sock%, tcpbufferblk, tcpbuffersize%, MSG_DONTWAIT) :
1380 	IF size%=0 THEN PRINT "connect failed. Error":PROCmyerror(ERR):END
1390 IF size%>20 : IF brokenseq%=0 THEN charcounter%=FNfast_recvparser(size%) : IF size%=charcounter% THEN PROCfast_flushbuf(tcpbufferblk,LEN$tcpbufferblk) : PROCfast_flushbuf(wordblk,LEN$wordblk)
1400 	REM IF size%=tcpbuffersize% THEN charcounter%=FNfast_recvparser(size%) : IF size%=charcounter% THEN PROCfast_flushbuf(tcpbufferblk,LEN$tcpbufferblk) : PROCfast_flushbuf(wordblk,LEN$wordblk)
1410 	IF size%>0 : IF size% >= charcounter%+1 THEN PROCrecvparser(size%,charcounter%) : PROCfast_flushbuf(tcpbufferblk,LEN$tcpbufferblk) : PROCfast_flushbuf(wordblk,LEN$wordblk)
1420 UNTIL FALSE
1430 REM ****************************
1440 REM ***** END OF MAIN LOOP *****
1450 REM ****************************
1460 :
1470 REM CLOSE CONNECTION
1480 DEF PROCexit__program
1490 	PROCerr_local(0)
1500 	IF save_scrmode% <> 131 THEN MODE save_scrmode%
1510 	PROCclose(sock%)
1520 REM *******************************
1530 REM ***** END OF MAIN PROGRAM *****
1540 REM *******************************
1550 	END
1560 ENDPROC
1570 :
1580 REM ****************************
1590 REM * BTELNET Program FN/Procs *
1600 REM * Elminster 2017           *
1610 REM ****************************
1620 :
1630 DEF PROCprocess_keybuffer(key$)
1640 	IF key$ = CHR$(0) THEN PROCdebug__toggle("DEBUG") : ENDPROC
1650 	IF key$ = CHR$(31) THEN PROCdebug__toggle("STEM") : ENDPROC
1660 	IF key$ = CHR$(29) THEN PROCrun__cmdline : ENDPROC
1670 	IF local_echo=FALSE THEN x%=FNsendkey(key$) : ENDPROC
1680 	IF local_echo=TRUE THEN x%=FNsendbuffer(key$)
1690 ENDPROC
1700 :
1710 DEF PROCrecvparser(totalsize%,charcounter%)
1720 	tcpbufferblk?totalsize%=13
1730 	REPEAT
1740 		charcounter%=FNprocess__character(charcounter%)
1750 	UNTIL charcounter% >= totalsize%
1760 ENDPROC
1770 :
1780 DEF FNfast_recvparser(totalsize%)
1790 	tcpbufferblk?totalsize%=13
1800 	Y%=totalsize%
1810 	CALL fastprint%
1820 =X%
1830 :
1840 DEF FNprocess__character(charcounter%)
1850 		IF tcpbufferblk?charcounter% = IAC OR brokenseq%=1 THEN charcounter%=FNnegotiate(totalsize%,charcounter%) : =charcounter%
1860 		IF tcpbufferblk?charcounter% >= 32 AND tcpbufferblk?charcounter% <= 126 THEN PRINT CHR$(tcpbufferblk?charcounter%); : =charcounter%+1
1870 		IF tcpbufferblk?charcounter% >= 128 AND tcpbufferblk?charcounter% <= 254 THEN PRINT CHR$(tcpbufferblk?charcounter%); : =charcounter%+1
1880 		IF tcpbufferblk?charcounter% = 27 THEN PRINT CHR$(tcpbufferblk?charcounter%); : =charcounter%+1
1890 		IF tcpbufferblk?charcounter% >= 7 AND tcpbufferblk?charcounter% <= 13 THEN PROCnvtprocess(tcpbufferblk?charcounter%)
1900 =charcounter%+1 : REM ignore any other character and go to the next one
1910 :
1920 DEF PROCmyerror(myerrnum)
1930 	PRINT "GLOBAL ERROR"
1940 	REPORT:PRINT ERL:PROCexit__program
1950 ENDPROC
1960 :
1970 DEF PROCopen_connection
1980 	PRINT "Trying ";server$;"..."
1990 	REM REPEAT
2000 			host%=FNgethost(server$+CHR$0)
2010 	REM UNTIL host%<>0
2020 	REM Just take the first alias
2030 	server%=FNreadword(FNreadword(host%!16))
2040 	PRINT "Resolved to "FNshowaddr(server%)
2050 	sock%=FNcreat(AF_INET%,SOCK_STREAM%,0)
2060 	IF sock%<0 THEN PRINT "Could not get a socket":PROCexit__program : END
2070 	IF FNopen__telnet <0 THEN PRINT "connect failed. Error": PROCexit__program : END
2080 	PRINT "Connected to " ; server$;". Port " ; port%
2090 	PRINT "Escape character is '^]'."
2100 ENDPROC
2110 :
2120 DEF FNopen__telnet
2130 	ipaddrblk?0=16
2140 	ipaddrblk?1=AF_INET%
2150 	ipaddrblk!2=FNendianswap16(port%)
2160 	ipaddrblk!4=server%
2170 	ipaddrblk!8=0
2180 	ipaddrblk!12=0
2190 	connect%=FNconnect(sock%,ipaddrblk,16)
2200 =connect%
2210 :
2220 DEF FNsendbuffer(bufferchar$)
2230 	LOCAL bufmax_flag% : bufmax_flag% = 0
2240 	IF firsttime_sendbuffer = 0 THEN firsttime_sendbuffer = 1 : PROCfast_flushbuf(tcpsendblk,sendbufsize%)
2250 	IF FNchecklinelen > 0 THEN =1
2260 	IF LEN$tcpsendblk >= sendbufsize% - 2 : IF bufferchar$ <> CHR$(13): IF bufferchar$ <> CHR$(127) THEN VDU 7 : =1
2270 	IF ASC(bufferchar$) <> 127 THEN $tcpsendblk = $tcpsendblk + bufferchar$
2280 	IF ASC(bufferchar$) = 127 : IF LEN$tcpsendblk = 0 THEN =1
2290 	IF ASC(bufferchar$) = 127 : IF LEN$tcpsendblk > 0 THEN $tcpsendblk = LEFT$($tcpsendblk,LEN($tcpsendblk)-1)
2300 	IF bufferchar$ = CHR$(13) PROCsendbuffer__chr13 ELSE PRINT bufferchar$;
2310 =1
2320 :
2330 DEF PROCsendbuffer__chr13
2340 	LOCAL x%
2350 	$tcpsendblk = $tcpsendblk + CHR$(10)
2360 	x%=FNsend(sock%, tcpsendblk, LEN$tcpsendblk, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCexit__program:END
2370 	PRINT bufferchar$
2380 	PROCfast_flushbuf(tcpsendblk,LEN$tcpsendblk)
2390 ENDPROC
2400 :
2410 DEF FNsendkey(bufferchar$)
2420 $smallblk = bufferchar$
2430 IF bufferchar$ = CHR$(13) THEN $smallblk = $smallblk + CHR$(10)
2440 x%=FNsend(sock%, smallblk, LEN$smallblk, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCexit__program:END
2450 =1
2460 :
2470 DEF FNchecklinelen
2480 	IF LEN$tcpsendblk < sendbufsize% THEN =0
2490 	PRINT CHR$(127)+CHR$(7)
2500 =-1
2510 DEF PROCnvtprocess(bufchar%)
2520 	LOCAL bufchar$ : REM Do something with NVT Control Codes
2530 	IF bufchar%=13 THEN PRINT : check_linefeed% = 1 : ENDPROC
2540 	IF bufchar%=10 AND check_linefeed% = 0 THEN PRINT : ENDPROC
2550 	IF bufchar%=10 THEN check_linefeed% = 0 : ENDPROC
2560 	IF bufchar%=8 THEN VDU 8 : ENDPROC : REM Backspace
2570 	IF bufchar%=7 THEN VDU 7 : ENDPROC : REM Bell
2580 	IF bufchar%=12 THEN VDU 12 : ENDPROC : REM CLS/Form Feed
2590 	IF bufchar%=9 THEN PRINT CHR$(9); :ENDPROC :REM Horizontal TAB
2600 	IF bufchar%=11 : FOR vtab = 1 TO 6 : PRINT : NEXT vtab: ENDPROC : REM Vertical TAB
2610 	message$ = "<NVT Cntrl Char>"+STR$(bufchar%) : PROCprint__debug(message$,"")
2620 ENDPROC
2630 :
2640 :
2650 DEF PROCflushbuf(thebuffer,bufsize%)
2660 	LOCAL count : count=0
2670 	REPEAT
2680 		thebuffer?count=13
2690 		count=count+1
2700 	UNTIL count >= bufsize%
2710 ENDPROC
2720 :
2730 DEF PROCfast_flushbuf(thebuffer%,bufsize%)
2740 	X%=bufsize%
2750 	A%=thebuffer%MOD256 : Y%=thebuffer%DIV256 : REM PRINT thebuffer%,~thebuffer%,~A%,~Y%
2760 	CALL fastinit%
2770 ENDPROC
2780 :
2790 DEF FNnegotiate(totalsize%,charcounter%)
2800 	LOCAL elseif : elseif=FALSE
2810 	LOCAL fixed : fixed=0
2820 	IF brokenseq%=1 THEN fixed%=FNfixbrokenseq(charcounter%)
2830 	IF fixed=-1 THEN =charcounter%+1
2840 	IF fixed > 0 THEN charcounter%=fixed% : elseif=TRUE
2850 	IF elseif=FALSE AND tcpbufferblk?charcounter% = IAC AND tcpbufferblk?(charcounter%+1) = 13 THEN brokenseq%=1 : =charcounter%+1
2860 	IF elseif=FALSE AND tcpbufferblk?charcounter% = IAC AND tcpbufferblk?(charcounter%+1) <> SB THEN charcounter%=FNmainneg(charcounter%,totalsize%) : elseif=TRUE
2870 	IF elseif=FALSE AND tcpbufferblk?charcounter% = IAC AND tcpbufferblk?(charcounter%+1) = SB THEN charcounter%=FNsubneg(charcounter%,totalsize%)
2880 	IF brokenseq%=0 THEN PROCprocess__negotiate
2890 =charcounter%
2900 :
2910 DEF FNfixbrokenseq(charcounter%)
2920 	IF IACpipecounter = 0 : brokenseq=0
2930 	IF IACpipe[0] <> IAC THEN PROCflush(IACpipe) : brokenseq=0
2940 	IF brokenseq=0 AND tcpbufferblk?charcounter%  <> IAC THEN =-1
2950 	IF brokenseq=0 AND tcpbufferblk?charcounter% = IAC THEN =0
2960 	IF IACpipecounter = 2 AND IACpipe[1] <> SB THEN FNenqueue(IACpipe,(tcpbufferblk?charcounter%)) : =charcount%+1
2970 	IF IACpipecounter = 3 AND IACpipe[1] <> SB THEN PROCflush(IACpipe) : brokenseq=0
2980 	IF brokenseq=0 AND tcpbufferblk?charcounter%  <> IAC THEN =-1
2990 	IF brokenseq=0 AND tcpbufferblk?charcounter% = IAC THEN =0
3000 	charcounter%=FNsubneg(charcounter%,totalsize%)
3010 =charcount%
3020 :
3030 DEF FNmainneg(charcounter%,totalsize%)
3040 	LOCAL X
3050 	IF charcounter%+3 <= totalsize% THEN X=FNenqueue(IACpipe,tcpbufferblk?charcounter%): X=FNenqueue(IACpipe,tcpbufferblk?(charcounter%+1)) : X=FNenqueue(IACpipe,tcpbufferblk?(charcounter%+2)) : =charcounter%+3
3060 	IF charcounter%+2 <= totalsize% THEN X=FNenqueue(IACpipe,tcpbufferblk?charcounter%): X=FNenqueue(IACpipe,tcpbufferblk?(charcounter%+1)) : brokenseq%=1 : =charcounter%+2
3070 	X=FNenqueue(IACpipe,(tcpbufferblk?charcounter%))
3080 =charcounter%+1
3090 :
3100 DEF FNsubneg(charcounter%,totalsize%)
3110 	LOCAL X
3120 	REM IAC SB ABC <parameters> IAC SE
3130 	findSE=INSTR($tcpbufferblk,CHR$(SE),charcounter%)
3140 	IF findSE = 0 THEN readuntil = totalsize% : brokenseq%=1 ELSE readuntil = findSE
3150 	FOR readiterator = charcounter% TO findSE
3160 		X=FNenqueue(IACpipe,tcpbufferblk?charcounter%)
3170 		charcounter%=charcounter%+1
3180 	NEXT readiterator
3190 =charcounter%
3200 :
3210 DEF FNenqueue(IACpipe,bufferchar)
3220 	IF IACpipecounter >= IACpipesize% THEN =-1
3230 	IACpipe(IACpipecounter)=bufferchar
3240 	IACpipecounter=IACpipecounter+1
3250 =0
3260 :
3270 DEF FNdequeue(IACpipe)
3280 	IF IACpipecounter <= 0 THEN =-1
3290 	deqchar%=IACpipe(IACpipehead)
3300 	IACpipehead=IACpipehead+1
3310 	IF IACpipehead = IACpipecounter THEN IACpipehead = 0 : IACpipecounter = 0
3320 =deqchar%
3330 :
3340 DEF PROCflush(IACPIPE)
3350 	IACpipecounter=0
3360 ENDPROC
3370 :
3380 DEF PROCprint__debug(message$,newline$)
3390 	IF debug% = 0 THEN ENDPROC
3400 	IF newline$ ="N" OR newline$ ="n" THEN PRINT message$; : ENDPROC
3410 	PRINT message$
3420 ENDPROC
3430 :
3440 DEF PROCdebug__toggle(opt$)
3450 	IF opt$="STEM" AND myterm$="UNKNOWN" THEN PRINT "UNKNOWN" : ENDPROC
3460 	IF opt$="STEM" AND myterm$ <> "UNKNOWN" THEN PRINT "STEM ON" : ENDPROC
3470 	IF opt$="DEBUG" AND debug%=0 THEN debug%=1 : PRINT "<Debug ON>" : ENDPROC
3480 	IF opt$="DEBUG" AND debug%=1 THEN debug%=0  : PRINT "<Debug OFF>" : ENDPROC
3490 	PRINT "Option Not Known"
3500 ENDPROC
3510 :
3520 DEF PROCprocess__negotiate
3530 	tempblk?IACpipecounter=13
3540 	FOR emptyqueue% = 0 TO IACpipecounter-1
3550 		tempblk?emptyqueue% = FNdequeue(IACpipe)
3560 	NEXT emptyqueue%
3570 	IF tempblk?0 <> IAC THEN ENDPROC
3580 	IF tempblk?2 = NAWS THEN PROCwindowsize : ENDPROC
3590 	IF tempblk?2 = TERM THEN PROCtermtype(tempblk) : ENDPROC
3600 	IF tempblk?2 = ECHO THEN PROCecho(tempblk) : ENDPROC
3610 	IF tempblk?2 = SGA THEN PROCsga(tempblk) : ENDPROC
3620
3630 	IF tempblk?1 = DO THEN tempblk?1 = WONT
3640 	IF tempblk?1 = WILL THEN tempblk?1 = DONT
3650
3660 	IF tempblk?1 <> SB THEN x%=FNsend(sock%, tempblk, 3, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
3670 	IF tempblk?1 <> SB THEN debug_message$="Unknown IAC:"+"     "+ STR$(tempblk?2) : PROCprint__debug(debug_message$,"") : ENDPROC
3680 	PRINT "Negotiating Unknown", tempblk?0, tempblk?1, tempblk?2 , tempblk?3, tempblk?4, tempblk?5
3690 ENDPROC
3700 :
3710 :
3720 DEF PROCscreensize : REM not checking for DO etc.
3730 	REM LOCAL A%
3740 	REMA%=135 : bbcmode%=(((USR&FFF4)AND&FFFF00)DIV256)DIV256 : REM Dont care if shadow Mode so no check for 128
3750 	bbcmode%=FNget__mode("NOSHADOW")
3760 	PROCprint__debug("Mode ","N") : PROCprint__debug(STR$(bbcmode%),"")
3770 	IF bbcmode% = 3 THEN screenX=80 : screenY=25 : ENDPROC
3780 	IF bbcmode% = 0 THEN screenX=80 : screenY=32 : ENDPROC
3790 	IF bbcmode% = 1 THEN screenX=40 : screenY=32 : ENDPROC
3800 	IF bbcmode% = 2 THEN screenX=20 : screenY=32 : ENDPROC
3810 	IF bbcmode% = 4 THEN screenX=20 : screenY=32 : ENDPROC
3820 	IF bbcmode% = 5 THEN screenX=20 : screenY=32 : ENDPROC
3830 	IF bbcmode% = 6 THEN screenX=40 : screenY=25 : ENDPROC
3840 	IF bbcmode% = 7 THEN screenX=40 : screenY=25 : ENDPROC
3850 ENDPROC
3860 :
3870 DEF FNget__mode(mode$)
3880 	IF mode$ = "SHADOW" THEN  mode%=(FNfx(&87,0)DIV256) OR ((FNfx(&75,0)AND&10)*8)
3890 	ELSE  mode%=(FNfx(&87,0)DIV256)
3900 =mode%
3910 :
3920 DEF FNfx(A%,X%):LOCAL Y%:Y%=X%DIV256:=((USR&FFF4)AND&FFFF00)DIV256
3930 :
3940 DEF PROCwindowsize
3950 	LOCAL ret%
3960 	PROCscreensize
3970 	PROCprint__debug("Windows size:","N")
3980 	tempblk?0=IAC : tempblk?1=WILL : tempblk?2=NAWS
3990 	ret%=FNsend(sock%, tempblk, 3, 0)
4000 	IF ret%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4010 	tempblk?0=IAC : tempblk?1=SB : tempblk?2=NAWS : tempblk?3=0 : tempblk?4=screenX : tempblk?5=0 : tempblk?6=screenY : tempblk?7=IAC : tempblk?8=SE
4020 	ret%=FNsend(sock%, tempblk, 9, 0) : IF ret%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4030 	debug_message$=STR$(tempblk?1) + "   "+ STR$(tempblk?2)+ "   "+ STR$(tempblk?3)+ "   "+ STR$(tempblk?4)+ "   "+ STR$(tempblk?5)+ "   "+ STR$(tempblk?6)+ "   "+ STR$(tempblk?7)+ "   "+ STR$(tempblk?8)
4040 	PROCprint__debug(debug_message$,"")
4050 ENDPROC
4060 :
4070 DEF PROCtermtype(tempblk) : REM Need to support multiple term type abilities
4080 	PROCprint__debug("Term Type","")
4090 	LOCAL x% : x%=0
4100 	IF tempblk?1 = DO THEN tcpsendblk?0=IAC : tcpsendblk?1=WILL : tcpsendblk?2=TERM : x%=FNsend(sock%, tcpsendblk, 3, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4110 	IF x% > 0 THEN ENDPROC
4120 	IF tempblk?1 <> SB OR tempblk?3 <> SEND OR tempblk?4 <> IAC OR tempblk?5 <> SE THEN ENDPROC
4130 	negotiateblk?0=IAC : negotiateblk?1=SB : negotiateblk?2=TERM :negotiateblk?3=IS
4140 	x%=FNsend(sock%, negotiateblk, 4, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4150 	$nameblk=myterm$
4160 	x%=FNsend(sock%, nameblk, LEN$nameblk, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4170 	negotiateblk?0=IAC : negotiateblk?1=SE
4180 	x%=FNsend(sock%, negotiateblk, 2, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4190 ENDPROC
4200 :
4210 DEF PROCsga(tempblk)
4220 	PROCprint__debug("SGA"+STR$(tempblk?1),"")
4230 	IF tempblk?1 = DO THEN tcpsendblk?0=IAC : tcpsendblk?1=WILL : tcpsendblk?2=SGA : x%=FNsend(sock%, tcpsendblk, 3, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4240 	IF tempblk?1 = WILL THEN tcpsendblk?0=IAC : tcpsendblk?1=DO : tcpsendblk?2=SGA : x%=FNsend(sock%, tcpsendblk, 3, 0) : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4250 ENDPROC
4260 :
4270 DEF PROCecho(tempblk)
4280 	PROCprint__debug("Echo:"+STR$(tempblk?1),"")
4290 	 IF tempblk?1 = WILL THEN tcpsendblk?0=IAC : tcpsendblk?1=DO : tcpsendblk?2=ECHO : x%=FNsend(sock%, tcpsendblk, 3, 0):local_echo=FALSE:PROCprint__debug("Echo Off",""):IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4300 	 IF tempblk?1 = DO THEN tcpsendblk?0=IAC : tcpsendblk?1=WONT : tcpsendblk?2=ECHO : x%=FNsend(sock%, tcpsendblk, 3, 0)  : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4310 	 IF tempblk?1 = DONT THEN tcpsendblk?0=IAC : tcpsendblk?1=WONT : tcpsendblk?2=ECHO : x%=FNsend(sock%, tcpsendblk, 3, 0)  : IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4320 	 IF tempblk?1 = WONT THEN tcpsendblk?0=IAC : tcpsendblk?1=DONT : tcpsendblk?2=ECHO : x%=FNsend(sock%, tcpsendblk, 3, 0):local_echo=TRUE:PROCprint__debug("Echo On",""):IF x%=0 THEN PRINT "Connection failed. Error":PROCmyerror(ERR):END
4330 ENDPROC
4340 :
4350 DEF FNget__termtype
4360 	LOCAL ERROR:ON ERROR LOCAL ="UNKNOWN"
4370 	vtok%=TRUE
4380 	IF vtok%:*VT102 ON
4390 	IF vtok%=TRUE THEN="vt100"
4400 ="UNKNOWN"
4410 :
4420 DEF PROCrun__cmdline
4430 	LOCAL cmdproc% :  LOCAL elements%
4440 	IF cmd$(0) = "" : FOR cmd_num%=0 TO cmd_total% : READ cmd$(cmd_num%) : READ cmdproc$(cmd_num%) : READ cmdhelp$(cmd_num%) : NEXT cmd_num%
4450 	REPEAT
4460 		cmdproc% = -1
4470 		PRINT TAB(0) "telnet> ";
4480 		INPUT ""cmdline$ : elements%=FNstrip__cmdline(cmdline$," ")
4490 		TEMP$ = cmdline_array$(0) : cmdline_array$(0)=FNlc(TEMP$)
4500 		FOR cmd_num%=0 TO cmd_total%
4510 			IF cmdline_array$(0) = cmd$(cmd_num%) THEN cmdproc%=EVAL("FNrun__"+cmdproc$(cmd_num%)+"("+STR$(elements%)+")") : cmd_num%=cmd_total%
4520 		NEXT cmd_num%
4530 		IF cmdproc%=-1 : IF cmdline$ <> "" THEN PRINT "?Invalid command"
4540 	UNTIL cmdproc% = 1 OR ( sock%>0 AND cmdline$ = "" )
4550 ENDPROC
4560 :
4570 DEF FNrun__help(elements%)
4580 IF elements%=0 THEN PROCprint_fullhelp : =0
4590 		FOR cmd_num%=0 TO cmd_total%
4600 			IF cmdline_array$(1) = cmd$(cmd_num%) THEN PRINT cmdhelp$(cmd_num%) : cmd_num%=cmd_total%
4610 		NEXT cmd_num%
4620 =0
4630 :
4640 DEF PROCprint_fullhelp
4650 	PRINT "Commands may be abbreviated.  Commands are:" : PRINT
4660 	PRINT "close   	close current connection"
4670 	PRINT "logout  	forcibly logout remote user and close the connection"
4680 	PRINT "display 	display operating parameters"
4690 	PRINT "mode    	try to enter line or character mode ('mode ?' for more)"
4700 	PRINT "telnet  	connect to a site"
4710 	PRINT "open    	connect to a site"
4720 	PRINT "quit    	exit telnet"
4730 	PRINT "send    	transmit special characters ('send ?' for more)"
4740 	PRINT "set     	set operating parameters ('set ?' for more)"
4750 	PRINT "unset   	unset operating parameters ('unset ?' for more)"
4760 	PRINT "status  	print status information"
4770 	PRINT "toggle  	toggle operating parameters ('toggle ?' for more)"
4780 	PRINT "slc     	change state of special charaters ('slc ?' for more)"
4790 	PRINT "auth    	turn on (off) authentication ('auth ?' for more)"
4800 	PRINT "z       	suspend telnet"
4810 	PRINT "!       	invoke a subshell"
4820 	PRINT "environ 	change environment variables ('environ ?' for more)"
4830 	PRINT "?       	print help information"
4840 ENDPROC
4850 :
4860 DEF FNstrip__cmdline(string$,delimiter$)
4870 	start%=1
4880 	counter%=0
4890 	REPEAT
4900 		end%=INSTR(string$,delimiter$,start%)
4910 		cmdline_array$(counter%)=MID$(string$,start%,end%-start%)
4920 		start%=end%+1
4930 		counter%=counter%+1
4940 	UNTIL end%=0
4950 =counter%-1
4960 :
4970 DEF FNrun__close(elements%)
4980 			PROCexit__program
4990 =0
5000 :
5010 DEF PROCload__dim(array_name$,rows%,columns%)
5020 LOCAL row_counter% : LOCAL column_counter%
5025 DIM send_list$(15,7)
5030 FOR row_counter% = 0 TO rows%-1
5040 	FOR column_counter% = 0 TO columns%-1
5050 		EVAL("READ "+array_name$+"("+STR$(row_counter%)+","+STR$(column_counter%)+")")
5060 	NEXT column_counter%
5070 NEXT row_counter%
5080 ENDPROC
:
5090 DEF FNrun__open(elements%)
5100 	IF sock%>0 THEN PRINT "?Already connected to ";server$ : =0
5110 	IF elements% = 0 THEN INPUT "(To) "A$ : PROCprocess__open
5120 	IF elements% > 0 THEN A$=MID$(cmdline$,INSTR(cmdline$," "))+" " : PROCprocess__open
5130 	IF server$ = "" THEN PRINT "usage: open [-l user] host-name [port]" : =0
5140 	IF port% < 0 OR port% > 65536 THEN PRINT "Error: Port must be between 0 and 65536" : =0
5150 	PROCopen_connection
5160 =1
5170 :
5180 DEF PROCprocess__open
5190 	user$=FNcl("-l",1)  : debug_message$="user: "+user$ : PROCprint__debug(debug_message$,"")
5200 	server$=FNcl("",0) : debug_message$="server: "+server$ : PROCprint__debug(debug_message$,"")
5210 	A$=A$+" ":port$=FNcl("",0) : IF port$ ="" THEN port%=23 ELSE port%=(VAL(port$))
5220 ENDPROC
5230 :
5240 DEF PROCprocess__send
5250 	LOCAL row% : LOCAL temp$ : LOCAL counter% : counter% = 0
5260 	IF load_send = 0 THEN load_send = 1 : PROCload__dim("send_list$",15,7)
5270 		arg1$=FNcl("",0) : debug_message$="server: "+server$ : PROCprint__debug(debug_message$,"")
5280 		IF elements% > 1 THEN arg2$=FNcl("",0) : debug_message$="server: "+server$ : PROCprint__debug(debug_message$,"")
5290 		TEMP$ = cmdline_array$(1) : cmdline_array$(1)=FNlc(TEMP$)
5300 		FOR counter% = 0 TO 14
5310 			cmdline_array$(1) = send_list$(row%,0) THEN PROCsend_char(row%): counter% = 14
5320 			row%=row% + 1
5330 		NEXT
5340 ENDPROC
5350 :
5360 DEF FNrun__environ(elements%)
5370 	PRINT "Under Construction"
5380 	IF FNcl("-?",0) THEN PROCprint__filehelp("environ") : =0
5390 =0
5400 DEF FNrun__mode(elements%)
5410 	PRINT "Under Construction"
5420 	IF FNcl("-?",0) THEN PROCprint__filehelp("mode"): =0
5430 =0
5440 :
5450 DEF FNrun__display(elements%)
5460 	PRINT "Under Construction"
5470 =0
5480 :
5490 DEF FNrun__send(elements%)
5510 	IF elements% = 0 THEN PRINT "need at least one argument for 'send' command"+CHR(13)+"'send -?' for help" : =0
5520 	IF elements% > 0 THEN A$=MID$(cmdline$,INSTR(cmdline$," "))+" "
5525    IF FNcl("-?",0) THEN PROCprint__filehelp("send"): =0
5526 	PROCprocess__send
5530 =0
5540 :
5550 DEF FNrun__set(elements%)
5560 	PRINT "Under Construction"
5570 IF FNcl("-?",0) THEN PROCprint__filehelp("set"): =0
5580 =0
5590 :
5600 DEF FNrun__unset(elements%)
5610 	PRINT "Under Construction"
5620 IF FNcl("-?",0) THEN PROCprint__filehelp("unset"): =0
5630 =0
5640 :
5650 DEF FNrun__status(elements%)
5660 	PRINT "Under Construction"
5670 =0
5680 :
5690 DEF FNrun__toggle(elements%)
5700 	PRINT "Under Construction"
5710 IF FNcl("-?",0) THEN PROCprint__filehelp("toggle"): =0
5720 =0
5730 :
5740 DEF FNrun__slc(elements%)
5750 	PRINT "Under Construction"
5760 IF FNcl("-?",0) THEN PROCprint__filehelp("slc"): =0
5770 =0
5780 :
5790 DEF FNrun__auth(elements%)
5800 	PRINT "Under Construction"
5810 IF FNcl("-?",0) THEN PROCprint__filehelp("auth"): =0
5820 =0
5830 :
5840 DEF FNrun__subshell(elements%)
5850 	PRINT "Under Construction"
5860 =0
5870 :
5880 DEF FNrun__suspend
5890 	PRINT "Under Construction"
5900 =0
5910 :
5920 DEF PROCget__cmdline
5930 	IF FNcl("-?",0):PRINT "usage: BTELNET [-?] [-v] [-l user] host-name [port]" : END
5940 	IF FNcl("-v ",0) OR FNcl("-V ",0):PRINT "version: BTELNET ";version$ : END
5950 	debug%=FNcl("-debug",0) : debug_message$="debug: "+STR$(debug%) : PROCprint__debug(debug_message$,"")
5960 	user$=FNcl("-l",1)  : debug_message$="user: "+user$ : PROCprint__debug(debug_message$,"")
5970 	server$=FNcl("",0) : debug_message$="server: "+server$ : PROCprint__debug(debug_message$,"")
5980 	A$=A$+" ":port$=FNcl("",0) : IF port$ ="" THEN port%=23 ELSE port%=(VAL(port$)) : IF port% <0 OR port%>65536 THEN PRINT "Error: Port must be between 0 and 65536" : END
5990 debug_message$="port: "+STR$(port%) : PROCprint__debug(debug_message$,"")
6000 ENDPROC
6010 :
6020 DEF PROCprint__filehelp(filename$)
6030 	LOCAL file%
6040 	LOCAL ERROR:ON ERROR PRINT "Help file Not Found" : ENDPROC
6050 	file%=OPENIN filename$
6060 	REPEAT
6070 		INPUT# file% , fileline$
6080 		PRINT fileline$
6090 	UNTIL EOF#
6100 	CLOSE# file%
6110 ENDPROC
6120 :
6130 DEF PROCload__array(rows%,columns%)
6140
6150 ENDPROC
6160 DEF PROCasm_fastprint
6170 	FOR P=0 TO 2 STEP 2
6180 	P%=fastprint%
6190 		[OPT P
6200 			LDX #0
6210 		.fp_loop
6220 			LDA tcpbufferblk,X
6230 			CMP #7
6240 			BCC fp_fail
6250 			CMP #10
6260 			BEQ fp_fail
6270 			CMP #13+1
6280 			BCC fp_pass
6290 			CMP #27
6300 			BEQ fp_pass
6310 			CMP #32
6320 			BCC fp_fail
6330 			CMP #254+1
6340 			BCS fp_iac
6350 			CMP #127
6360 			BEQ fp_fail
6370 		.fp_pass JSR &FFE3
6380 		.fp_fail INX
6390 			DEY
6400 			BEQ fp_exit
6410 			JMP fp_loop
6420 		.fp_iac
6430 		.fp_exit RTS
6440 		]
6450 	NEXT P
6460 ENDPROC
6470 DEF PROCasm__fastinit
6480 	FOR P=0 TO 2 STEP 2
6490 	P%=fastinit%
6500 		[OPT P
6510 			STA zeropage_ptr
6520 	 		STY zeropage_ptr+1
6530 			LDY #0
6540 		.fp_loop2
6550 			LDA #13
6560 			STA (zeropage_ptr),Y
6570 			CPX #0
6580 			BEQ fp_exit2
6590 			INY
6600 			DEX
6610 			BEQ fp_exit2
6620 			JMP fp_loop2
6630 		.fp_exit2 RTS
6640 		]
6650 	NEXT P
6660 ENDPROC
6670 :
6680 REM *****************
6690 REM Network functions
6700 REM (C)2010 SPROW
6710 REM *****************
6720 :
6730 DEF FNgethost(name$)
6740 wordblk?0=8:REM Parameters in
6750 wordblk?1=24:REM Parameters out
6760 wordblk?2=&41:REM Resolver_GetHost
6770 wordblk?3=0:REM No error on entry
6780 wordblk!4=nameblk
6790 $nameblk=name$
6800 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
6810 IF wordblk?3<>0 THEN=0
6820 =wordblk+4:REM Address not value
6830 :
6840 DEF FNcreat(pf%,type%,prot%)
6850 wordblk?0=16:REM Parameters in
6860 wordblk?1=8:REM Parameters out
6870 wordblk?2=&00:REM Socket_Creat
6880 wordblk?3=0:REM No error on entry
6890 wordblk!4=pf%
6900 wordblk!8=type%
6910 wordblk!12=prot%
6920 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
6930 IF wordblk?3<>0 THEN=-1
6940 =wordblk!4
6950 :
6960 DEF FNbind(handle%,addr%,addrlen%)
6970 wordblk?0=16:REM Parameters in
6980 wordblk?1=8:REM Parameters out
6990 wordblk?2=&01:REM Socket_Bind
7000 wordblk?3=0:REM No error on entry
7010 wordblk!4=handle%
7020 wordblk!8=addr%
7030 wordblk!12=addrlen%
7040 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7050 IF wordblk?3<>0 THEN=-1
7060 =wordblk!4
7070 :
7080 DEF FNlisten(handle%,count%)
7090 wordblk?0=12:REM Parameters in
7100 wordblk?1=8:REM Parameters out
7110 wordblk?2=&02:REM Socket_Listen
7120 wordblk?3=0:REM No error on entry
7130 wordblk!4=handle%
7140 wordblk!8=count%
7150 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7160 IF wordblk?3<>0 THEN=-1
7170 =wordblk!4
7180 :
7190 DEF FNaccept(handle%,addr%,addrlenblk%)
7200 wordblk?0=16:REM Parameters in
7210 wordblk?1=8:REM Parameters out
7220 wordblk?2=&03:REM Socket_Accept
7230 wordblk?3=0:REM No error on entry
7240 wordblk!4=handle%
7250 wordblk!8=addr%
7260 wordblk!12=addrlenblk%
7270 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7280 IF wordblk?3<>0 THEN=-1
7290 =wordblk!4
7300 :
7310 DEF FNconnect(handle%,addr%,addrlen%)
7320 wordblk?0=16:REM Parameters in
7330 wordblk?1=8:REM Parameters out
7340 wordblk?2=&04:REM Socket_Connect
7350 wordblk?3=0:REM No error on entry
7360 wordblk!4=handle%
7370 wordblk!8=addr%
7380 wordblk!12=addrlen%
7390 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7400 IF wordblk?3<>0 THEN=-1
7410 =wordblk!4
7420 :
7430 DEF FNrecv(handle%,data%,len%,opts%)
7440 wordblk?0=20:REM Parameters in
7450 wordblk?1=8:REM Parameters out
7460 wordblk?2=&05:REM Socket_Recv
7470 wordblk?3=0:REM No error on entry
7480 wordblk!4=handle%
7490 wordblk!8=data%
7500 wordblk!12=len%
7510 wordblk!16=opts%
7520 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7530 IF wordblk?3<>0 THEN=-1
7540 =wordblk!4
7550 :
7560 DEF FNsend(handle%,data%,len%,opts%)
7570 wordblk?0=20:REM Parameters in
7580 wordblk?1=8:REM Parameters out
7590 wordblk?2=&08:REM Socket_Send
7600 wordblk?3=0:REM No error on entry
7610 wordblk!4=handle%
7620 wordblk!8=data%
7630 wordblk!12=len%
7640 wordblk!16=opts%
7650 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7660 IF wordblk?3<>0 THEN=-1
7670 =wordblk!4
7680 :
7690 DEF PROCshutdown(handle%,type%)
7700 wordblk?0=12:REM Parameters in
7710 wordblk?1=4:REM Parameters out
7720 wordblk?2=&0B:REM Socket_Shutdown
7730 wordblk?3=0:REM No error on entry
7740 wordblk!4=handle%
7750 wordblk!8=type%
7760 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7770 ENDPROC
7780 :
7790 DEF PROCclose(handle%)
7800 wordblk?0=8:REM Parameters in
7810 wordblk?1=4:REM Parameters out
7820 wordblk?2=&10:REM Socket_Close
7830 wordblk?3=0:REM No error on entry
7840 wordblk!4=handle%
7850 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7860 ENDPROC
7870 DEF FNgethost(name$)
7880 wordblk?0=8:REM Parameters in
7890 wordblk?1=24:REM Parameters out
7900 wordblk?2=&41:REM Resolver_GetHost
7910 wordblk?3=0:REM No error on entry
7920 wordblk!4=nameblk
7930 $nameblk=name$
7940 A%=192:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
7950 IF wordblk?3<>0 THEN=0
7960 =wordblk+4:REM Address not value
7970 :
7980 DEF FNreadword(addr%)
7990 LOCALtemp%,data%
8000 IF(addr% AND&FFFF0000)<>&FFFF0000 THEN=!addr%:REM Not IO memory
8010 FOR temp%=3TO0STEP-1
8020 !wordblk=addr%+temp%:A%=5:X%=wordblk:Y%=wordblk DIV256:CALL&FFF1
8030 tempblk?temp%=wordblk?4
8040 NEXT
8050 =!tempblk
8060 :
8070 DEF FNshowaddr(addr%)
8080 !tempblk=addr%
8090 =STR$(tempblk?0)+"."+STR$(tempblk?1)+"."+STR$(tempblk?2)+"."+STR$(tempblk?3)
8100 :
8110 DEF FNendianswap16(data%)
8120 LOCALtemp%
8130 !tempblk=data% AND&FFFF
8140 temp%=tempblk?0:tempblk?0=tempblk?1:tempblk?1=temp%
8150 =!tempblk
8160 :
8170 REM ***************************
8180 REM MDFS BASIC Library Routines
8190 REM (C)J.G.Harston jgh@mdfs.net
8200 REM ***************************
8210 :
8220 REM > BLib.BFont 1.00 23-Feb-1991
8230 :
8240 REM Bitmap font routines
8250 REM ====================
8260 REM This version uses serial BGET/BPUT
8270 :
8280 :
8290 REM BFont_Load - Load bitmap font
8300 REM -----------------------------
8310 :
8320 DEFPROCBFont_Load(A$,S%):LOCALin%,A%:in%=OPENIN(A$):IFin%=0:ENDPROC
8330 PRINT "Loading Font..."
8340 REPEAT:IFA%=0:IFS%:VDU23,S%:S%=S%+1
8350 VDUBGET#in%:A%=(A%+1)AND7:UNTILEOF#in%:CLOSE#in%:ENDPROC
8360 :
8370 :
8380 REM BFont_Save - Save bitmap font
8390 REM -----------------------------
8400 REM A$ - filename to save to
8410 REM S% - start character to save
8420 REM E% - end character to save
8430 REM R% - save as raw bitmap
8440 :
8450 DEFPROCBFont_Save(A$,S%,E%,R%):LOCALout%,L%,X%,Y%,A%:out%=OPENOUT(A$):IFout%=0:ENDPROC
8460 DIMX%-1:Y%=X%DIV256:A%=10:REPEAT:?X%=S%:CALL&FFF1:IFR%=0:BPUT#out%,23:BPUT#out%,S%
8470 L%=1:REPEAT:BPUT#out%,X%?L%:L%=L%+1:UNTILL%=8:NEXT:S%=S%+1:UNTILS%>E%:CLOSE#out%:ENDPROC
8480 :
8490 REM > BLib.CmdLine 1.10 27Jul2009
8500 REM v1.10 Parses "quoted" options
8510 :
8520 REM Command Line Parsing
8530 REM ~~~~~~~~~~~~~~~~~~~~
8540 :
8550 REM FNcl() - parse command line for switches, options and parameters
8560 REM ----------------------------------------------------------------
8570 REM FNcl("",0) - return next parameter
8580 REM FNcl(switch$,0) - return TRUE/FALSE if switch$ present
8590 REM FNcl(option$,1) - return option string if present or ""
8600 :
8610 DEFFNcl(l$,n%):IFl$="":A$=FNs(A$):IFASCA$=34:A%=INSTR(A$+" "" ",""" ",2):l$=MID$(A$,2,A%-2):A$=FNs(MID$(A$,A%+1)):=l$
8620 IFl$="":A%=INSTR(A$+" "," "):l$=LEFT$(A$,A%-1):A$=FNs(MID$(A$,A%+1)):=l$
8630 IFn%=0:IFl$<>"":A%=INSTR(A$,l$):IFA%:A$=FNs(LEFT$(A$,A%-1)+MID$(A$,INSTR(A$," ",A%)+1))+" ":=TRUE
8640 IFn%=0:IFl$<>"":=FALSE
8650 A%=INSTR(LEFT$(" ",ASCl$=32)+A$,l$):IFA%=0:=""
8660 A$=LEFT$(A$,A%-1)+FNs(MID$(A$,INSTR(A$," ",A%)+1))
8670 IFASCl$=32:l$=MID$(A$,A%):A$=LEFT$(A$,A%-1):=MID$(l$,1-(ASCl$=34),LENl$+2*(ASCl$=34))
8680 IFASCMID$(A$,A%,1)<>34:l$=MID$(A$,A%,INSTR(A$+" "," ",A%)-A%):A$=LEFT$(A$,A%-1)+MID$(A$,A%+LENl$+1):=l$
8690 l$=MID$(A$,A%+1,INSTR(A$+""" ",""" ",A%+1)-A%-1):A$=LEFT$(A$,A%-1)+MID$(A$,A%+LENl$+3):=l$
8700 DEFFNs(A$):IFLEFT$(A$,1)=" ":REPEATA$=MID$(A$,2):UNTILLEFT$(A$,1)<>" "
8710 IFRIGHT$(A$,1)=" ":REPEATA$=LEFT$(A$,LENA$-1):UNTILRIGHT$(A$,1)<>" "
8720 =A$
8730 :
8740 REM > BLib.ProgEnv 1.05 09Jan2007
8750 :
8760 REM Program Environment Functions
8770 REM =============================
8780 :
8790 REM Return command line tail, sets run$ to execution filename
8800 REM Works with BASIC on I/O,T6502,Arc,TZ80BBC,PC,Win,TARM
8810 REM Needs: 270 free bytes above end of heap for temp. w/s
8820 REM ---------------------------------------------------------
8830 DEFFNOS_GetEnv:LOCALA$,A%,X%,Y%:X%=1:os%=((USR&FFF4)AND&FF00)DIV256
8840 IFos%>31:IFPAGE>&FFFF:DIMX%LOCAL256:LOADATN"GetModuleFileName",0,X%,255:run$=$$X%:=@cmd$
8850 IFos%>31:A$=$&100
8860 IFLENA$=0:IFHIMEM>&FFFF:run$=$(PAGE-&E00):LOADATN16TOA$,,A%:LOADATN72,"",A%:A$=MID$(A$,INSTR(A$," ",(ASCMID$(A$,7,1)=45)AND7)+1):IFINSTR(A$," ")=0:A$=run$
8870 IFLENA$=0:IF?(TOP-3):A$=$&600 ELSE IFLENA$=0:A$=$(PAGE-&300)
8880 FORY%=-1TO0:A$=" "+A$:REPEATA$=MID$(A$,2):UNTILASCA$<>32
8890 IFY%:IFASCA$=34:A%=INSTR(A$,"""",2)+1 ELSE IFY%:A%=INSTR(A$+" "," ")
8900 IFY%:run$=LEFT$(A$,A%-1):IFrun$<>"":A$=MID$(A$,A%+1)
8910 NEXT:=A$
8920 :
8930 REM Run a program, passing it a command line
8940 REM If program is *Command, called with OSCLI, else CHAINed
8950 REM -------------------------------------------------------
8960 DEFPROCos(A$):IFASCA$=42:OSCLIA$ ELSE IFA$<>"":CHAINA$
8970 ENDPROC
8980 :
8990 REM Exit program, setting return value
9000 REM ----------------------------------
9010 DEFPROCexit(A%):OSCLI"FX1,"+STR$A%:quit$=quit$:A$=quit$:quit$="":PROCos(A$)
9020 IFos%>31:LOADASN A%
9030 IFos%<6:END ELSE *Quit
9040 ENDPROC
9050 :
9060 REM > LocalError 0.20 15-Oct-2016 J.G.Harston
9070 REM Implement LOCAL ERROR and ON ERROR LOCAL for 6502 BASIC
9080 :
9090 REM PROCerr_local(1) to enable local error handler
9100 REM PROCerr_local(0) to restore default error handler
9110 REM Usage: LOCAL ERROR:ON ERROR LOCAL local error handler
9120 :
9130 DEFPROCerr_local(A%):IF HIMEM>&FFFF:ENDPROC
9140 DIM P%-1:[OPT 0:NOP:]:IFP%?-1<>&EA:ENDPROC
9150 IF A%=0:?&202=err_0%:?&203=err_0%DIV256:ENDPROC
9160 A%=!&202:REPEATA%=A%+1:UNTIL(!A%AND&8000FF)=&80004C
9170 DIM err_% 119:FOR B%=0 TO 1:P%=err_%:[OPT 0
9180 LDY #0:LDA (&FD),Y:CMP #16:BNE E%
9190 LDY &0A:DEY:LDA (&0B),Y:CMP #&EA:BNE E%:.C%
9200 INY:LDA (&0B),Y:CMP #32:BEQ C%:CMP #&85:BNE E%
9210 INY:STY &0A:INC &1FB:LDA 4:SBC #8:STA 4:LDA 5
9220 SBC #0:STA 5:LDY #7:LDA &1F9:STA (&04),Y:DEY:.D%
9230 LDA &12,Y:STA (&04),Y:DEY:CPY #3:BNE D%
9240 LDA #0:STA (&04),Y:DEY:LDA #4:STA (&04),Y:DEY
9250 LDA #0:STA (&04),Y:DEY:LDA #&16:STA (&04),Y:BNE G%:.E%
9260 LDY #0:LDA (&16),Y:CMP #&EA:BEQ P%+5:.F%:JMP !&202
9270 LDA &16:ADC #0:STA &0B:LDA &17:ADC #0:STA &0C
9280 LDA #0:STA &0A:.G%:LDX #&F5:TXS:JMP A%!1:]:NEXT
9290 ?&202=err_%:?&203=err_%/256:err_0%=F%!1
9300 ENDPROC
9310 :
9320 REM > BLib.String 1.00 09Aug1998
9330 :
9340 REM String Manipulation Functions
9350 REM =============================
9360 :
9370 REM FNs() - strip spaces from start and end of string
9380 REM -------------------------------------------------
9390 DEFFNs(A$):IFLEFT$(A$,1)=" ":REPEATA$=MID$(A$,2):UNTILLEFT$(A$,1)<>" "
9400 IFRIGHT$(A$,1)=" ":REPEATA$=LEFT$(A$,LENA$-1):UNTILRIGHT$(A$,1)<>" "
9410 =A$
9420 :
9430 REM FNuc() - convert string to upper case
9440 REM -------------------------------------
9450 DEFFNuc(A$):LOCAL B$:IFA$="":=""
9460 REPEATB$=B$+CHR$(ASCA$AND((A$<"@")OR&DF)):A$=MID$(A$,2):UNTILA$="":=B$
9470 :
9480 REM FNlc() - convert string to lower case
9490 REM -------------------------------------
9500 DEFFNlc(A$):LOCAL B$:IFA$="":=""
9510 REPEATB$=B$+CHR$(ASCA$OR((A$<"_")AND&20)):A$=MID$(A$,2):UNTILA$="":=B$
9520 :
9530 REM *********************
9540 REM *** DIM READ DATA ***
9550 REM *********************
9560 DATA "send","send","transmit special characters ('send ?' for more)","set","set","set operating parameters ('set ?' for more)"
9570 DATA "unset","unset","unset operating parameters ('unset ?' for more)","status","status","print status information"
9580 DATA "toggle","toggle","toggle operating parameters ('toggle ?' for more)","slc","slc","change state of special charaters ('slc ?' for more)"
9590 DATA "auth","auth","turn on (off) authentication ('auth ?' for more)","z","subshell","suspend telnet"
9600 DATA "!","suspend","invoke a subshell","environ","environ","change environment variables ('environ ?' for more)"
9610 DATA "help","help","print help information","?","help","print help information","telnet","open","connect to a site"
9620 DATA "open","open","connect to a site","quit","close","exit telnet"
9630 DATA "mode","mode","try to enter line or character mode ('mode ?' for more)","display","display","display operating parameters"
9640 DATA "logout","close","forcibly logout remote user and close the connection","close","close","close current connection"
9650 REM sendlist - name, help, needconnect, no. args, fn/proc, no. bytes, special Char
9660 DATA "ao", "", 1, 0, 0, 2, AO
9670 DATA  "ayt", "", 1, 0, 0, 2, AYT
9680 DATA  "brk", "", 1, 0, 0, 2, BRKx
9690 DATA  "ec", "", 1, 0, 0, 2, EC
9700 DATA  "el", "", 1, 0, 0, 2, EL
9710 DATA  "escape", "", 1, 0, send_esc, 1, 0
9720 DATA  "ga", "", 1, 0, 0, 2, GA
9730 DATA  "ip", "", 1, 0, 0, 2, IP
9740 DATA  "nop", "", 1, 0, 0, 2, NOPx
9750 DATA  "eor", "", 1, 0, 0, 2, EORx
9760 DATA  "abort", "", 1, 0, 0, 2, ABORT
9770 DATA "susp", "", 1, 0, 0, 2, SUSP
9780 DATA  "eof", "", 1, 0, 0, 2, EOFx
9790 DATA  "synch", "", 1, 0, dosynch, 2, 0
9800 DATA  "getstatus", "", 1, 0, get_status, 6, 0
9810 :
9820 REM **************************
9830 REM ***** END of Program *****
9840 REM **************************
