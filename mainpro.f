***************************************************
*******    PROGRAM MAINPRO(INPUT,OUTPUT)   ********
***************************************************

      PROGRAM SETLLING

      implicit real*8(a-h,o-z)                         !óLå¯êîéö15åÖ ()ì‡ÇÕìKópïœêîÇÃêÊì™ï∂éö

      PARAMETER(LX=10+8,LY=4+8,LZ=10+8,LN=10000)       !ÉÅÉbÉVÉÖêîÅAó±éqêî

      dimension
     & U(LX,LY,LZ),V(LX,LY,LZ),W(LX,LY,LZ),            !ó¨ë¨
     & P(LX,LY,LZ),                                    !à≥óÕ
     & VOID(LX,LY,LZ),                                 !ãÛåÑó¶
     & RR(LX,LY,LZ),RRM(LX,LY,LZ),                     !à≥óÕÇÃé˚éx
     & FU(LX,LY,LZ),FV(LX,LY,LZ),FW(LX,LY,LZ),         !äµê´çÄ
     & VISX(LX,LY,LZ),VISY(LX,LY,LZ),VISZ(LX,LY,LZ),   !îSê´çÄ
     & UB(LX,LY,LZ),VB1(LX,LY,LZ),WB(LX,LY,LZ),        !1Ç¬ëOÇÃÉTÉCÉNÉãÇÃó¨ë¨
     & VOIDB(LX,LY,LZ),                                !1Ç¬ëOÇÃÉTÉCÉNÉãÇÃãÛåÑó¶
     & SPX(LX,LY,LZ),SPY(LX,LY,LZ),SPZ(LX,LY,LZ),	 !çRóÕ
     & VOLP(LX,LY,LZ)				       	 !ÉÅÉbÉVÉÖíÜÇÃó±éqÇÃëÃêœ

      dimension
     & UP(LN),  VP(LN),  WP(LN),		       	 !ó±éqë¨ìx
     & XP(LN),  YP(LN),  ZP(LN), 		       	 !ó±éqà íu
     & DP(LN)  					       	 !ó±åa

      INTEGER TNOA,TNOAK

      INTEGER PARTATL(LX-8,LY-8,LZ-8),                 !ÉÅÉbÉVÉÖÉpÅ[ÉgÉeÅ[ÉuÉã
     &        PARTA(LX-8,LY-8,LZ-8,256)

      real*8 MUF                                       !êÖÇÃîSìx

      CHARACTER kota*7,kota1*4,kota2*5,kota3*6,kota4*7      

*************************
****  ó±éqÇÃïœêîêÈåæ ****
*************************

      include 'dem.h'

*************************
********  íËêî **********
*************************

C      ich = 3543                                      !ÉfÅ[É^É`ÉFÉbÉNÇ∑ÇÈó±éq
C      open( unit = 77, err = 9906, status='new',
C     &file = 'pcheck.dat' )


      CYCLE =1                                         !åvéZäJénÉTÉCÉNÉã

      SU0 = 0.131                                      !ë„ï\ë¨ìx(èIññíæç~ë¨ìx) Uo(cm/s)
      SL0 = 0.12                                       !ë„ï\í∑Ç≥ Lo(cm)

      UW0 = SU0*6.0                                    !è„ï«ïîÇÃë¨ìx UW0(cm/s)

      dt = 1.0e-6                                      !óLéüå≥éûä‘çèÇ›(sec)

      DELT = dt * SU0 / SL0                            !ñ≥éüå≥éûä‘çèÇ›

      TWFIN = DELT*5000001.0                           !åvéZèIóπéûä‘
      NCYCLE = 10000                                   !ÉfÅ[É^èëÇ´èoÇµä‘äu

      muf = 0.01002                                    !ó¨ëÃîSìx
      RE = SL0 * SU0 / 0.01                            !ÉåÉCÉmÉãÉYêî

      DELX = 1.0/10.0                                  !ñ≥éüå≥ÉÅÉbÉVÉÖïù
      DELY = 1.0/10.0
      DELZ = 1.0/10.0

      CZ = 1.0                                         !âìêSóÕÇ…ÇÊÇÈî{ó¶å

      EPSI = 1.0                                       !èâä˙ÇÃé˚ë©îªíËåWêî
      OMG = 1.0                                        !â¡ë¨ìxåWêî

      RADJ = 0.0015                                    !ëÂè¨ó±éqîªíË

      PI=4.0*ATAN(1.0)                                 !ÉŒ

******************************
**** ÉfÅ[É^ÇÃì«Ç›çûÇ›ê›íË ****
******************************

      SW01 = 0.0                                       !0.0ÇÃèÍçáÇÕÉfÅ[É^ì«Ç›çûÇ›ÇµÇ»Ç¢

******************************

      IBAR = LX-8                                      !ÉÅÉbÉVÉÖì‡ì_êî
      JBAR = LY-8
      KBAR = LZ-8

      UI=0.0                                           !èâä˙0ê›íËÇ…ópÇ¢ÇÈílÇ
      VI=0.0
      WI=0.0

      DPARTAX=1.0
      DPARTAY=1.0
      DPARTAZ=1.0

      VOLC=DELX*DELY*DELZ                              !ÉÅÉbÉVÉÖëÃêœ

      IMAX=IBAR+8
      JMAX=JBAR+8
      KMAX=KBAR+8
      IM1=IMAX-1
      JM1=JMAX-1
      KM1=KMAX-1
      IM2=IMAX-2
      JM2=JMAX-2
      KM2=KMAX-2
      IM3=IMAX-3
      JM3=JMAX-3
      KM3=KMAX-3
      IM4=IMAX-4
      JM4=JMAX-4
      KM4=KMAX-4
      IM5=IMAX-5
      JM5=JMAX-5
      KM5=KMAX-5
      IM6=IMAX-6
      JM6=JMAX-6
      KM6=KMAX-6
      IM7=IMAX-7
      JM7=JMAX-7
      KM7=KMAX-7

      RDX=1.0/DELX                                      !ãtêî
      RDY=1.0/DELY
      RDZ=1.0/DELZ

      NPARTAX=IBAR/DPARTAX
      NPARTAY=JBAR/DPARTAY
      NPARTAZ=KBAR/DPARTAZ

************************
**** ó¨ëÃÇÃèâä˙ê›íË ****                                !ó¨ëÃÇÃèâä˙ê›íË
************************

      DO 560 K=1,KMAX
      DO 560 J=1,JMAX
      DO 560 I=1,IMAX
        U(I,J,K)= UI
        V(I,J,K)= VI
        W(I,J,K)= WI
        UB(I,J,K)= UI
        VB1(I,J,K)= VI
        WB(I,J,K)= WI
        SPX(I,J,K)= 0.0
        SPY(I,J,K)= 0.0
        SPZ(I,J,K)= 0.0
        VOLP(I,J,K)= 0.0
        VOID(I,J,K)= 1.0
        VOIDB(I,J,K)= 1.0
        P(I,J,K)= 0.0 
  560 CONTINUE 

********************************
***** ó±éqÉfÅ[É^ÇÃì«Ç›çûÇ› *****
********************************

      CALL PRE

********************************

      DO 275 I=1,IP
        DP(I) = 2.0*R(I)/SL0                            !ó±éqÇÃñ≥éüå≥íºåa
 275  CONTINUE

*******************************

      IF(SW01.NE.0.0) GO TO 750                         !1ÉTÉCÉNÉãñ⁄ÇÃèÍçáÇÃê›íËÉRÉRÇ©ÇÁ

********************************
****** ó±éqà íuÇÃñ≥éüå≥âª ******
********************************

      DO 7141 N=1,IP
        XP(N)=x(N)/SL0
        YP(N)=y(N)/SL0
        ZP(N)=z(N)/SL0
 7141 CONTINUE

************************************
****** ÉpÅ[ÉeÉBÉVÉáÉìÇÃèâä˙âª ******
************************************

      DO 7140 IP1=1,NPARTAX
      DO 7140 JP1=1,NPARTAY
      DO 7140 KP1=1,NPARTAZ
          PARTATL(IP1,JP1,KP1)=0
        DO 7140 NPP1=1,256
          PARTA(IP1,JP1,KP1,NPP1)=0
 7140 CONTINUE

********************************************
****** ÉÅÉbÉVÉÖíÜÇÃó±éqå¬êîÇÃÉJÉEÉìÉg ******
********************************************

      DO 7103 N=1,IP
        XPP=XP(N)
        YPP=YP(N)
        ZPP=ZP(N)
        II=INT(XPP/(DELX*DPARTAX))+1
        JJ=INT(YPP/(DELY*DPARTAY))+1
        KK=INT(ZPP/(DELZ*DPARTAZ))+1
        TNOA=PARTATL(II,JJ,KK)+1
        IF(TNOA.GT.256) GO TO 7103
        PARTATL(II,JJ,KK)=TNOA
        PARTA(II,JJ,KK,TNOA)=N 
 7103 CONTINUE

**************************
****** ãÛåÑó¶ÇÃåvéZ ******
**************************

      DO 7125 N=1,IP
        X1=XP(N)-DELX*INT(XP(N)/DELX)
        X2=DELX*(INT(XP(N)/DELX)+1)-XP(N)
        Y1=YP(N)-DELY*INT(YP(N)/DELY)
        Y2=DELY*(INT(YP(N)/DELY)+1)-YP(N)
        Z1=ZP(N)-DELZ*INT(ZP(N)/DELZ)
        Z2=DELZ*(INT(ZP(N)/DELZ)+1)-ZP(N)
        IX=INT(XP(N)/DELX)+5
        JY=INT(YP(N)/DELY)+5
        KZ=INT(ZP(N)/DELZ)+5
        XX1=DP(N)/2-X1
        XX2=DP(N)/2-X2
        YY1=DP(N)/2-Y1
        YY2=DP(N)/2-Y2
        ZZ1=DP(N)/2-Z1
        ZZ2=DP(N)/2-Z2
        XX1=MAX(XX1,0.0)
        XX2=MAX(XX2,0.0)
        YY1=MAX(YY1,0.0)
        YY2=MAX(YY2,0.0)
        ZZ1=MAX(ZZ1,0.0)
        ZZ2=MAX(ZZ2,0.0)
        VOLPX1=PI*XX1**2*(DP(N)+X1)/3.0
        VOLPX2=PI*XX2**2*(DP(N)+X2)/3.0
        VOLPY1=PI*YY1**2*(DP(N)+Y1)/3.0
        VOLPY2=PI*YY2**2*(DP(N)+Y2)/3.0
        VOLPZ1=PI*ZZ1**2*(DP(N)+Z1)/3.0
        VOLPZ2=PI*ZZ2**2*(DP(N)+Z2)/3.0
        VOLP(IX-1,JY,KZ)=VOLP(IX-1,JY,KZ)+VOLPX1
        VOLP(IX,JY-1,KZ)=VOLP(IX,JY-1,KZ)+VOLPY1
        VOLP(IX,JY,KZ-1)=VOLP(IX,JY,KZ-1)+VOLPZ1
        VOLP(IX,JY,KZ)=VOLP(IX,JY,KZ)+(PI*DP(N)**3)/6.0
     &  -(VOLPX1+VOLPY1+VOLPZ1+VOLPX2+VOLPY2+VOLPZ2)
        VOLP(IX+1,JY,KZ)=VOLP(IX+1,JY,KZ)+VOLPX2
        VOLP(IX,JY+1,KZ)=VOLP(IX,JY+1,KZ)+VOLPY2
        VOLP(IX,JY,KZ+1)=VOLP(IX,JY,KZ+1)+VOLPZ2
 7125 CONTINUE

      DO 7130 K=5,KM4
      DO 7130 J=5,JM4
      DO 7130 I=5,IM4
        VOIDD =(VOLC-VOLP(I,J,K))/VOLC
        VOIDB(I,J,K)=VOIDD
        VOID(I,J,K)=VOIDD
 7130 CONTINUE

**************************
**** ãÛåÑó¶ÇÃã´äEèåè ****
**************************

**** íÍïî ****
      DO 7113 J=5,JM4
      DO 7113 I=5,IM4
        VOIDB(I,J,4)=VOIDB(I,J,5)
        VOID(I,J,4)=VOID(I,J,5)
        VOIDB(I,J,3)=VOIDB(I,J,6)
        VOID(I,J,3)=VOID(I,J,6)
        VOIDB(I,J,2)=VOIDB(I,J,7)
        VOID(I,J,2)=VOID(I,J,7)
        VOIDB(I,J,1)=VOIDB(I,J,8)
        VOID(I,J,1)=VOID(I,J,8)
 7113 CONTINUE

**** ç∂ë§ ****
      DO 7132 K=5,KM4
      DO 7132 J=5,JM4
        VOIDB(1,J,K)=VOIDB(IM7,J,K)
        VOID(1,J,K)=VOID(IM7,J,K)
        VOIDB(2,J,K)=VOIDB(IM6,J,K)
        VOID(2,J,K)=VOID(IM6,J,K)
        VOIDB(3,J,K)=VOIDB(IM5,J,K)
        VOID(3,J,K)=VOID(IM5,J,K)
        VOIDB(4,J,K)=VOIDB(IM4,J,K)
        VOID(4,J,K)=VOID(IM4,J,K)
 7132 CONTINUE

**** âEë§ ****
      DO 7133 K=5,KM4
      DO 7133 J=5,JM4
        VOIDB(IMAX,J,K)=VOIDB(8,J,K)
        VOID(IMAX,J,K)=VOID(8,J,K)
        VOIDB(IM1,J,K)=VOIDB(7,J,K)
        VOID(IM1,J,K)=VOID(7,J,K)
        VOIDB(IM2,J,K)=VOIDB(6,J,K)
        VOID(IM2,J,K)=VOID(6,J,K)
        VOIDB(IM3,J,K)=VOIDB(5,J,K)
        VOID(IM3,J,K)=VOID(5,J,K)
 7133 CONTINUE

**** è„ïî ****
      DO 7134 J=5,JM4
      DO 7134 I=5,IM4
        VOIDB(I,J,KMAX)=VOIDB(I,J,KM7)
        VOID(I,J,KMAX)=VOID(I,J,KM7)
        VOIDB(I,J,KM1)=VOIDB(I,J,KM6)
        VOID(I,J,KM1)=VOID(I,J,KM6)
        VOIDB(I,J,KM2)=VOIDB(I,J,KM5)
        VOID(I,J,KM2)=VOID(I,J,KM5)
        VOIDB(I,J,KM3)=VOIDB(I,J,KM4)
        VOID(I,J,KM3)=VOID(I,J,KM4)
 7134 CONTINUE

**** éËëOë§ ****
      DO 7117 K=5,KM4
      DO 7117 I=5,IM4
        VOIDB(I,1,K)=VOIDB(I,8,K)
        VOID(I,1,K)=VOID(I,8,K)
        VOIDB(I,2,K)=VOIDB(I,7,K)
        VOID(I,2,K)=VOID(I,7,K)
        VOIDB(I,3,K)=VOIDB(I,6,K)
        VOID(I,3,K)=VOID(I,6,K)
        VOIDB(I,4,K)=VOIDB(I,5,K)
        VOID(I,4,K)=VOID(I,5,K)
 7117 CONTINUE

**** âúë§ ****
      DO 7127 K=5,KM4
      DO 7127 I=5,IM4
        VOIDB(I,JMAX,K)=VOIDB(I,JM7,K)
        VOID(I,JMAX,K)=VOID(I,JM7,K)
        VOIDB(I,JM1,K)=VOIDB(I,JM6,K)
        VOID(I,JM1,K)=VOID(I,JM6,K)
        VOIDB(I,JM2,K)=VOIDB(I,JM5,K)
        VOID(I,JM2,K)=VOID(I,JM5,K)
        VOIDB(I,JM3,K)=VOIDB(I,JM4,K)
        VOID(I,JM3,K)=VOID(I,JM4,K)
 7127 CONTINUE

******************************
****** ÇπÇÒífó¨ÇÍÇÃê›íË ******
******************************

      DELUW = (UW0/SU0)/FLOAT(KBAR)                    !ÇπÇÒífó¶

      DO 563 K=5,KM4
      DO 563 J=5,JM4
      DO 563 I=5,IM4
        U(I,J,K) = (0.5*DELUW+DELUW*FLOAT(K-5))/VOID(I,J,K)
        UB(I,J,K)= (0.5*DELUW+DELUW*FLOAT(K-5))/VOID(I,J,K)
 563  CONTINUE

**************************************
****** ó¨ëÃë¨ìxÇÃã´äEèåèÇÃê›íË ******
**************************************

**** è„ïî ****
      DO 6915 I=5,IM4
      DO 6915 J=5,JM4
        U(I,J,KM3)=-U(I,J,KM4)+2.0*UW0/SU0
        U(I,J,KM2)=-U(I,J,KM5)+2.0*UW0/SU0
        U(I,J,KM1)=-U(I,J,KM6)+2.0*UW0/SU0
        U(I,J,KMAX)=-U(I,J,KM7)+2.0*UW0/SU0
        V(I,J,KM3)=-V(I,J,KM4)
        V(I,J,KM2)=-V(I,J,KM5)
        V(I,J,KM1)=-V(I,J,KM6)
        V(I,J,KMAX)=-V(I,J,KM7)
        W(I,J,KM4)=0.0
        W(I,J,KM3)=W(I,J,KM5)
        W(I,J,KM2)=W(I,J,KM6)
        W(I,J,KM1)=W(I,J,KM7)
        W(I,J,KMAX)=W(I,J,KM7-1)
 6915 CONTINUE

**** íÍïî ****
      DO 6918 I=5,IM4
      DO 6918 J=5,JM4
        U(I,J,4)=-U(I,J,5)
        U(I,J,3)=-U(I,J,6)
        U(I,J,2)=-U(I,J,7)
        U(I,J,1)=-U(I,J,8)
        V(I,J,4)=-V(I,J,5)
        V(I,J,3)=-V(I,J,6)
        V(I,J,2)=-V(I,J,7)
        V(I,J,1)=-V(I,J,8)
        W(I,J,4)= 0.0
        W(I,J,3)= W(I,J,5)
        W(I,J,2)= W(I,J,6)
        W(I,J,1)= W(I,J,7)
 6918 CONTINUE

**** ç∂ë§ ****
      DO 6300 J=5,JM4
      DO 6300 K=5,KM4
        U(4,J,K)=U(IM4,J,K)
        U(3,J,K)=U(IM5,J,K)
        U(2,J,K)=U(IM6,J,K)
        U(1,J,K)=U(IM7,J,K)
        V(4,J,K)=V(IM4,J,K)
        V(3,J,K)=V(IM5,J,K)
        V(2,J,K)=V(IM6,J,K)
        V(1,J,K)=V(IM7,J,K)
        W(4,J,K)=W(IM4,J,K)
        W(3,J,K)=W(IM5,J,K)
        W(2,J,K)=W(IM6,J,K)
        W(1,J,K)=W(IM7,J,K)
 6300 CONTINUE

**** âEë§ ****
      DO 6302 J=5,JM4
      DO 6302 K=5,KM4
        U(IM3,J,K)=U(5,J,K)
        U(IM2,J,K)=U(6,J,K)
        U(IM1,J,K)=U(7,J,K)
        U(IMAX,J,K)=U(8,J,K)
        V(IM3,J,K)=V(5,J,K)
        V(IM2,J,K)=V(6,J,K)
        V(IM1,J,K)=V(7,J,K)
        V(IMAX,J,K)=V(8,J,K)
        W(IM3,J,K)=W(5,J,K)
        W(IM2,J,K)=W(6,J,K)
        W(IM1,J,K)=W(7,J,K)
        W(IMAX,J,K)=W(8,J,K)
 6302 CONTINUE

**** éËëOë§ ****
      DO 6911 I=5,IM4
      DO 6911 K=5,KM4
        U(I,4,K)=-U(I,5,K)
        U(I,3,K)=-U(I,6,K)
        U(I,2,K)=-U(I,7,K)
        U(I,1,K)=-U(I,8,K)
        V(I,4,K)= 0.0
        V(I,3,K)= V(I,5,K)
        V(I,2,K)= V(I,6,K)
        V(I,1,K)= V(I,7,K)
        W(I,4,K)=-W(I,5,K)
        W(I,3,K)=-W(I,6,K)
        W(I,2,K)=-W(I,7,K)
        W(I,1,K)=-W(I,8,K)
 6911 CONTINUE

**** âúë§ ****
      DO 6912 I=5,IM4
      DO 6912 K=5,KM4
        U(I,JM3,K)=-U(I,JM4,K)
        U(I,JM2,K)=-U(I,JM5,K)
        U(I,JM1,K)=-U(I,JM6,K)
        U(I,JMAX,K)=-U(I,JM7,K)
        V(I,JM4,K)= 0.0
        V(I,JM3,K)= V(I,JM5,K)
        V(I,JM2,K)= V(I,JM6,K)
        V(I,JM1,K)= V(I,JM7,K)
        V(I,JMAX,K)= V(I,JM7-1,K)
        W(I,JM3,K)=-W(I,JM4,K)
        W(I,JM2,K)=-W(I,JM5,K)
        W(I,JM1,K)=-W(I,JM6,K)
        W(I,JMAX,K)=-W(I,JM7,K)
 6912 CONTINUE

      GOTO 920                                          !1ÉTÉCÉNÉãñ⁄èÍçáÇÃê›íËÉRÉRÇ‹Ç≈

  750 CONTINUE

*****************************************
***** ó¨ëÃà¯åpÇ¨ÉfÅ[É^ÇÃì«Ç›çûÇ› ********               !à¯åpÇ¨ÉfÅ[É^ì«Ç›çûÇ›
*****************************************

      open( unit = 1, err = 9906, status='old',
     &file = 'airinfile.dat' )

      READ(1,*) I

      DO 900 I=1,IMAX
      DO 900 J=1,JMAX
      DO 900 K=1,KMAX
        READ(1,*) U(I,J,K),V(I,J,K),W(I,J,K),P(I,J,K)
        READ(1,*) UB(I,J,K),VB1(I,J,K),WB(I,J,K)
        READ(1,*) VOID(I,J,K),VOIDB(I,J,K)
  900 CONTINUE

      DO 905 I=1,IMAX
      DO 905 J=1,JMAX
      DO 905 K=1,KMAX
        READ(1,*) SPX(I,J,K),SPY(I,J,K),SPZ(I,J,K)
  905 CONTINUE

      close( 1 )

      goto 9907

 9906 write(9,*) 'open error airinfile.dat (unit=1)'

 9907 continue

**************************************************

  920 CONTINUE                                         !Ç±Ç±Ç‹Ç≈èâä˙ê›íË

****************************
***** ÉTÉCÉNÉãÉXÉ^Å[Ég *****
****************************

 1000 CONTINUE

      ITER=0
      FLG=1.0
      ASSIGN 3200 TO KRET

      DO 3300 K=4,KM3
      DO 3300 J=4,JM3
      DO 3300 I=4,IM3

****************************
***** äµê´çÄÇÃç∑ï™åvéZ *****
****************************

      VU1=(V(I,J+2,K)+V(I+1,J+2,K)+V(I,J+1,K)+V(I+1,J+1,K))/4.
      VU2=(V(I,J+1,K)+V(I+1,J+1,K)+V(I,J,K)+V(I+1,J,K))/4.
      VU3=(V(I,J,K)+V(I+1,J,K)+V(I,J-1,K)+V(I+1,J-1,K))/4.
      VU4=(V(I,J-1,K)+V(I+1,J-1,K)+V(I,J-2,K)+V(I+1,J-2,K))/4.
      VU5=(V(I,J-2,K)+V(I+1,J-2,K)+V(I,J-3,K)+V(I+1,J-3,K))/4.

      WU1=(W(I,J,K+2)+W(I+1,J,K+2)+W(I,J,K+1)+W(I+1,J,K+1))/4.
      WU2=(W(I,J,K+1)+W(I+1,J,K+1)+W(I,J,K)+W(I+1,J,K))/4.
      WU3=(W(I,J,K)+W(I+1,J,K)+W(I,J,K-1)+W(I+1,J,K-1))/4.
      WU4=(W(I,J,K-1)+W(I+1,J,K-1)+W(I,J,K-2)+W(I+1,J,K-2))/4.
      WU5=(W(I,J,K-2)+W(I+1,J,K-2)+W(I,J,K-3)+W(I+1,J,K-3))/4.

      UV1=(U(I+2,J,K)+U(I+2,J+1,K)+U(I+1,J,K)+U(I+1,J+1,K))/4.
      UV2=(U(I+1,J,K)+U(I+1,J+1,K)+U(I,J,K)+U(I,J+1,K))/4.
      UV3=(U(I,J,K)+U(I,J+1,K)+U(I-1,J,K)+U(I-1,J+1,K))/4.
      UV4=(U(I-1,J,K)+U(I-1,J+1,K)+U(I-2,J,K)+U(I-2,J+1,K))/4.
      UV5=(U(I-2,J,K)+U(I-2,J+1,K)+U(I-3,J,K)+U(I-3,J+1,K))/4.

      WV1=(W(I,J,K+2)+W(I,J+1,K+2)+W(I,J,K+1)+W(I,J+1,K+1))/4.
      WV2=(W(I,J,K+1)+W(I,J+1,K+1)+W(I,J,K)+W(I,J+1,K))/4.
      WV3=(W(I,J,K)+W(I,J+1,K)+W(I,J,K-1)+W(I,J+1,K-1))/4.
      WV4=(W(I,J,K-1)+W(I,J+1,K-1)+W(I,J,K-2)+W(I,J+1,K-2))/4.
      WV5=(W(I,J,K-2)+W(I,J+1,K-2)+W(I,J,K-3)+W(I,J+1,K-3))/4.

      UW1=(U(I+2,J,K)+U(I+2,J,K+1)+U(I+1,J,K)+U(I+1,J,K+1))/4.
      UW2=(U(I+1,J,K)+U(I+1,J,K+1)+U(I,J,K)+U(I,J,K+1))/4.
      UW3=(U(I,J,K)+U(I,J,K+1)+U(I-1,J,K)+U(I-1,J,K+1))/4.
      UW4=(U(I-1,J,K)+U(I-1,J,K+1)+U(I-2,J,K)+U(I-2,J,K+1))/4.
      UW5=(U(I-2,J,K)+U(I-2,J,K+1)+U(I-3,J,K)+U(I-3,J,K+1))/4.

      VW1=(V(I,J+2,K)+V(I,J+2,K+1)+V(I,J+1,K)+V(I,J+1,K+1))/4.
      VW2=(V(I,J+1,K)+V(I,J+1,K+1)+V(I,J,K)+V(I,J,K+1))/4.
      VW3=(V(I,J,K)+V(I,J,K+1)+V(I,J-1,K)+V(I,J-1,K+1))/4.
      VW4=(V(I,J-1,K)+V(I,J-1,K+1)+V(I,J-2,K)+V(I,J-2,K+1))/4.
      VW5=(V(I,J-2,K)+V(I,J-2,K+1)+V(I,J-3,K)+V(I,J-3,K+1))/4.

      UU1=U(I+2,J,K)*(VOID(I+3,J,K)+VOID(I+2,J,K))/2.
      UU2=U(I+1,J,K)*(VOID(I+2,J,K)+VOID(I+1,J,K))/2.
      UU3=U(I,J,K)*(VOID(I+1,J,K)+VOID(I,J,K))/2.
      UU4=U(I-1,J,K)*(VOID(I,J,K)+VOID(I-1,J,K))/2.
      UU5=U(I-2,J,K)*(VOID(I-1,J,K)+VOID(I-2,J,K))/2.
      UU6=U(I,J+2,K)*(VOID(I,J+2,K)+VOID(I+1,J+2,K))/2.
      UU7=U(I,J+1,K)*(VOID(I,J+1,K)+VOID(I+1,J+1,K))/2.
      UU8=U(I,J-1,K)*(VOID(I,J-1,K)+VOID(I+1,J-1,K))/2.
      UU9=U(I,J-2,K)*(VOID(I,J-2,K)+VOID(I+1,J-2,K))/2.
      UU10=U(I,J,K+2)*(VOID(I,J,K+2)+VOID(I+1,J,K+2))/2.
      UU11=U(I,J,K+1)*(VOID(I,J,K+1)+VOID(I+1,J,K+1))/2.
      UU12=U(I,J,K-1)*(VOID(I,J,K-1)+VOID(I+1,J,K-1))/2.
      UU13=U(I,J,K-2)*(VOID(I,J,K-2)+VOID(I+1,J,K-2))/2.

      VV1=V(I,J+2,K)*(VOID(I,J+2,K)+VOID(I,J+2,K))/2.
      VV2=V(I,J+1,K)*(VOID(I,J+2,K)+VOID(I,J+1,K))/2.
      VV3=V(I,J,K)*(VOID(I,J+1,K)+VOID(I,J,K))/2.
      VV4=V(I,J-1,K)*(VOID(I,J,K)+VOID(I,J-1,K))/2.
      VV5=V(I,J-2,K)*(VOID(I,J-1,K)+VOID(I,J-2,K))/2.
      VV6=V(I+2,J,K)*(VOID(I+2,J,K)+VOID(I+2,J+1,K))/2.
      VV7=V(I+1,J,K)*(VOID(I+1,J,K)+VOID(I+1,J+1,K))/2.
      VV8=V(I-1,J,K)*(VOID(I-1,J,K)+VOID(I-1,J+1,K))/2.
      VV9=V(I-2,J,K)*(VOID(I-2,J,K)+VOID(I-2,J+1,K))/2.
      VV10=V(I,J,K+2)*(VOID(I,J,K+2)+VOID(I,J+1,K+2))/2.
      VV11=V(I,J,K+1)*(VOID(I,J,K+1)+VOID(I,J+1,K+1))/2.
      VV12=V(I,J,K-1)*(VOID(I,J,K-1)+VOID(I,J+1,K-1))/2.
      VV13=V(I,J,K-2)*(VOID(I,J,K-2)+VOID(I,J+1,K-2))/2.

      WW1=W(I,J,K+2)*(VOID(I,J,K+2)+VOID(I,J,K+2))/2.
      WW2=W(I,J,K+1)*(VOID(I,J,K+2)+VOID(I,J,K+1))/2.
      WW3=W(I,J,K)*(VOID(I,J,K+1)+VOID(I,J,K))/2.
      WW4=W(I,J,K-1)*(VOID(I,J,K)+VOID(I,J,K-1))/2.
      WW5=W(I,J,K-2)*(VOID(I,J,K-1)+VOID(I,J,K-2))/2.
      WW6=W(I+2,J,K)*(VOID(I+2,J,K)+VOID(I+2,J,K+1))/2.
      WW7=W(I+1,J,K)*(VOID(I+1,J,K)+VOID(I+1,J,K+1))/2.
      WW8=W(I-1,J,K)*(VOID(I-1,J,K)+VOID(I-1,J,K+1))/2.
      WW9=W(I-2,J,K)*(VOID(I-2,J,K)+VOID(I-2,J,K+1))/2.
      WW10=W(I,J+2,K)*(VOID(I,J+2,K)+VOID(I,J+2,K+1))/2.
      WW11=W(I,J+1,K)*(VOID(I,J+1,K)+VOID(I,J+1,K+1))/2.
      WW12=W(I,J-1,K)*(VOID(I,J-1,K)+VOID(I,J-1,K+1))/2.
      WW13=W(I,J-2,K)*(VOID(I,J-2,K)+VOID(I,J-2,K+1))/2.

      VU1=VU1*(VOID(I,J+2,K)+VOID(I+1,J+2,K))/2.
      VU2=VU2*(VOID(I,J+1,K)+VOID(I+1,J+1,K))/2.
      VU3=VU3*(VOID(I,J,K)+VOID(I+1,J,K))/2.
      VU4=VU4*(VOID(I,J-1,K)+VOID(I+1,J-1,K))/2.
      VU5=VU5*(VOID(I,J-2,K)+VOID(I+1,J-2,K))/2.

      WU1=WU1*(VOID(I,J,K+2)+VOID(I+1,J,K+2))/2.
      WU2=WU2*(VOID(I,J,K+1)+VOID(I+1,J,K+1))/2.
      WU3=WU3*(VOID(I,J,K)+VOID(I+1,J,K))/2.
      WU4=WU4*(VOID(I,J,K-1)+VOID(I+1,J,K-1))/2.
      WU5=WU5*(VOID(I,J,K-2)+VOID(I+1,J,K-2))/2.

      UV1=UV1*(VOID(I+2,J,K)+VOID(I+2,J+1,K))/2.
      UV2=UV2*(VOID(I+1,J,K)+VOID(I+1,J+1,K))/2.
      UV3=UV3*(VOID(I,J,K)+VOID(I,J+1,K))/2.
      UV4=UV4*(VOID(I-1,J,K)+VOID(I-1,J+1,K))/2.
      UV5=UV5*(VOID(I-2,J,K)+VOID(I-2,J+1,K))/2.

      WV1=WV1*(VOID(I,J,K+2)+VOID(I,J+1,K+2))/2.
      WV2=WV2*(VOID(I,J,K+1)+VOID(I,J+1,K+1))/2.
      WV3=WV3*(VOID(I,J,K)+VOID(I,J+1,K))/2.
      WV4=WV4*(VOID(I,J,K-1)+VOID(I,J+1,K-1))/2.
      WV5=WV5*(VOID(I,J,K-2)+VOID(I,J+1,K-2))/2.

      UW1=UW1*(VOID(I+2,J,K)+VOID(I+2,J,K+1))/2.
      UW2=UW2*(VOID(I+1,J,K)+VOID(I+1,J,K+1))/2.
      UW3=UW3*(VOID(I,J,K)+VOID(I,J,K+1))/2.
      UW4=UW4*(VOID(I-1,J,K)+VOID(I-1,J,K+1))/2.
      UW5=UW5*(VOID(I-2,J,K)+VOID(I-2,J,K+1))/2.

      VW1=VW1*(VOID(I,J+2,K)+VOID(I,J+2,K+1))/2.
      VW2=VW2*(VOID(I,J+1,K)+VOID(I,J+1,K+1))/2.
      VW3=VW3*(VOID(I,J,K)+VOID(I,J,K+1))/2.
      VW4=VW4*(VOID(I,J-1,K)+VOID(I,J-1,K+1))/2.
      VW5=VW5*(VOID(I,J-2,K)+VOID(I,J-2,K+1))/2.

***** éléüÇÃíÜâõç∑ï™ñ@ *****

      fux1=uu3     /(12.*delx)
     &    *(-u(i+2,j,k)+8.*u(i+1,j,k)+u(i-2,j,k)-8.*u(i-1,j,k))
      fuy1=vu3     /(12.*dely)
     &    *(-u(i,j+2,k)+8.*u(i,j+1,k)+u(i,j-2,k)-8.*u(i,j-1,k))
      fuz1=wu3     /(12.*delz)
     &    *(-u(i,j,k+2)+8.*u(i,j,k+1)+u(i,j,k-2)-8.*u(i,j,k-1))
      fvx1=uv3     /(12.*delx)
     &    *(-v(i+2,j,k)+8.*v(i+1,j,k)+v(i-2,j,k)-8.*v(i-1,j,k))
      fvy1=vv3     /(12.*dely)
     &    *(-v(i,j+2,k)+8.*v(i,j+1,k)+v(i,j-2,k)-8.*v(i,j-1,k))
      fvz1=wv3     /(12.*delz)
     &    *(-v(i,j,k+2)+8.*v(i,j,k+1)+v(i,j,k-2)-8.*v(i,j,k-1))
      fwx1=uw3     /(12.*delx)
     &    *(-w(i+2,j,k)+8.*w(i+1,j,k)+w(i-2,j,k)-8.*w(i-1,j,k))
      fwy1=vw3     /(12.*dely)
     &    *(-w(i,j+2,k)+8.*w(i,j+1,k)+w(i,j-2,k)-8.*w(i,j-1,k))
      fwz1=ww3     /(12.*delz)
     &    *(-w(i,j,k+2)+8.*w(i,j,k+1)+w(i,j,k-2)-8.*w(i,j,k-1))

      fux2=1.0/(12.*delx)
     &    *(-uu1**2+8.*uu2**2+uu5**2-8.*uu4**2)
      fuy2=1.0/(12.*dely)
     &    *(-uu6*vu1+8.*uu7*vu2+uu9*vu5-8.*uu8*vu4)
      fuz2=1.0/(12.*delz)
     &    *(-uu10*wu1+8.*uu11*wu2+uu13*wu5-8.*uu12*wu4)
      fvx2=1.0/(12.*delx)
     &    *(-vv6*uv1+8.*vv7*uv2+vv9*uv5-8.*vv8*uv4)
      fvy2=1.0/(12.*dely)
     &    *(-vv1**2+8.*vv2**2+vv5**2-8.*vv4**2)
      fvz2=1.0/(12.*delz)
     &    *(-vv10*wv1+8.*vv11*wv2+vv13*wv5-8.*vv12*wv4)
      fwx2=1.0/(12.*delx)
     &    *(-ww6*uw1+8.*ww7*uw2+ww9*uw5-8.*ww8*uw4)
      fwy2=1.0/(12.*dely)
     &    *(-ww10*vw1+8.*ww11*vw2+ww13*vw5-8.*ww12*vw4)
      fwz2=1.0/(12.*delz)
     &    *(-ww1**2+8.*ww2**2+ww5**2-8.*ww4**2)

      FU(I,J,K)=(FUX1+FUY1+FUZ1+FUX2+FUY2+FUZ2)/2.
      FV(I,J,K)=(FVX1+FVY1+FVZ1+FVX2+FVY2+FVZ2)/2.
      FW(I,J,K)=(FWX1+FWY1+FWZ1+FWX2+FWY2+FWZ2)/2.

****************************
***** îSê´çÄÇÃç∑ï™åvéZ *****
****************************

      VU2=(V(I,J+1,K)+V(I+1,J+1,K)+V(I,J,K)+V(I+1,J,K))/4.
      VU4=(V(I,J-1,K)+V(I+1,J-1,K)+V(I,J-2,K)+V(I+1,J-2,K))/4.
      WU2=(W(I,J,K+1)+W(I+1,J,K+1)+W(I,J,K)+W(I+1,J,K))/4.
      WU4=(W(I,J,K-1)+W(I+1,J,K-1)+W(I,J,K-2)+W(I+1,J,K-2))/4.
      UV2=(U(I+1,J,K)+U(I+1,J+1,K)+U(I,J,K)+U(I,J+1,K))/4.
      UV4=(U(I-1,J,K)+U(I-1,J+1,K)+U(I-2,J,K)+U(I-2,J+1,K))/4.
      WV2=(W(I,J,K+1)+W(I,J+1,K+1)+W(I,J,K)+W(I,J+1,K))/4.
      WV4=(W(I,J,K-1)+W(I,J+1,K-1)+W(I,J,K-2)+W(I,J+1,K-2))/4.
      UW2=(U(I+1,J,K)+U(I+1,J,K+1)+U(I,J,K)+U(I,J,K+1))/4.
      UW4=(U(I-1,J,K)+U(I-1,J,K+1)+U(I-2,J,K)+U(I-2,J,K+1))/4.
      VW2=(V(I,J+1,K)+V(I,J+1,K+1)+V(I,J,K)+V(I,J,K+1))/4.
      VW4=(V(I,J-1,K)+V(I,J-1,K+1)+V(I,J-2,K)+V(I,J-2,K+1))/4.
      VISX1=((U(I+1,J,K)-2.*U(I,J,K)+U(I-1,J,K))/DELX**2
     &+(U(I,J+1,K)-2.*U(I,J,K)+U(I,J-1,K))/DELY**2
     &+(U(I,J,K+1)-2.*U(I,J,K)+U(I,J,K-1))/DELZ**2)
     &*(VOID(I+1,J,K)+VOID(I,J,K))/2.
      VISY1=((V(I+1,J,K)-2.*V(I,J,K)+V(I-1,J,K))/DELX**2
     &+(V(I,J+1,K)-2.*V(I,J,K)+V(I,J-1,K))/DELY**2
     &+(V(I,J,K+1)-2.*V(I,J,K)+V(I,J,K-1))/DELZ**2)
     &*(VOID(I,J+1,K)+VOID(I,J,K))/2.
      VISZ1=((W(I+1,J,K)-2.*W(I,J,K)+W(I-1,J,K))/DELX**2
     &+(W(I,J+1,K)-2.*W(I,J,K)+W(I,J-1,K))/DELY**2
     &+(W(I,J,K+1)-2.*W(I,J,K)+W(I,J,K-1))/DELZ**2)
     &*(VOID(I,J,K+1)+VOID(I,J,K))/2.
      VISX2=((U(I+1,J,K)-2.*U(I,J,K)+U(I-1,J,K))/(DELX**2)
     &+(V(I+1,J,K)-V(I+1,J-1,K)-V(I,J,K)+V(I,J-1,K))/(DELY*DELX)
     &+(W(I+1,J,K)-W(I+1,J,K-1)-W(I,J,K)+W(I,J,K-1))/(DELZ*DELX))
     &*(VOID(I+1,J,K)+VOID(I,J,K))/2.
      VISY2=((U(I,J+1,K)-U(I-1,J+1,K)-U(I,J,K)+U(I-1,J,K))/(DELX*DELY)
     &+(V(I,J+1,K)-2*V(I,J,K)+V(I,J-1,K))/(DELY**2)
     &+(W(I,J+1,K)-W(I,J+1,K-1)-W(I,J,K)+W(I,J,K-1))/(DELZ*DELY))
     &*(VOID(I,J+1,K)+VOID(I,J,K))/2.
      VISZ2=((U(I,J,K+1)-U(I-1,J,K+1)-U(I,J,K)+U(I-1,J,K))/(DELX*DELZ)
     &+(V(I,J,K+1)-V(I,J-1,K+1)-V(I,J,K)+V(I,J-1,K))/(DELY*DELZ)
     &+(W(I,J,K+1)-2*W(I,J,K)+W(I,J,K-1))/(DELZ**2))
     &*(VOID(I,J,K+1)+VOID(I,J,K))/2.
      VISX3X=((VOID(I+1,J,K)-VOID(I,J,K))/(3.*DELX))*(2.*(U(I+1,J,K)
     &-U(I-1,J,K))/DELX-((VU2-VU4)/DELY+(WU2-WU4)/DELZ))
      VISX3Y=(((VOID(I,J+1,K)+VOID(I+1,J+1,K))-(VOID(I,J-1,K)
     &+VOID(I+1,J-1,K)))/(4.*DELY))*((U(I,J+1,K)-U(I,J-1,K))/(2.*DELY)
     &+((V(I+1,J-1,K)+V(I+1,J,K))-(V(I,J-1,K)+V(I,J,K)))/(2.*DELX))
      VISX3Z=(((VOID(I,J,K+1)+VOID(I+1,J,K+1))-(VOID(I,J,K-1)
     &+VOID(I+1,J,K-1)))/(4.*DELZ))*((U(I,J,K+1)-U(I,J,K-1))/(2.*DELZ)
     &+((W(I+1,J,K-1)+W(I+1,J,K))-(W(I,J,K-1)+W(I,J,K)))/(2.*DELX))
      VISY3X=(((VOID(I+1,J,K)+VOID(I+1,J+1,K))-(VOID(I-1,J,K)
     &+VOID(I-1,J+1,K)))/(4.*DELX))*((V(I+1,J,K)-V(I-1,J,K))/(2.*DELX)
     &+((U(I-1,J+1,K)+U(I,J+1,K))-(U(I-1,J,K)+U(I,J,K)))/(2.*DELY))
      VISY3Y=((VOID(I,J+1,K)-VOID(I,J,K))/(3.*DELY))*(2.*(V(I,J+1,K)
     &-V(I,J-1,K))/DELY-((UV2-UV4)/DELX+(WV2-WV4)/DELZ))
      VISY3Z=(((VOID(I,J,K+1)+VOID(I,J+1,K+1))-(VOID(I,J,K-1)
     &+VOID(I,J+1,K-1)))/(4.*DELZ))*((V(I,J,K+1)-V(I,J,K-1))/(2.*DELZ) 
     &+((W(I,J+1,K-1)+W(I,J+1,K))-(W(I,J,K-1)+W(I,J,K)))/(2.*DELY))
      VISZ3X=(((VOID(I+1,J,K)+VOID(I+1,J,K+1))-(VOID(I-1,J,K)
     &+VOID(I-1,J,K+1)))/(4.*DELX))*((W(I+1,J,K)-W(I-1,J,K))/(2.*DELX)
     &+((U(I-1,J,K+1)+U(I,J,K+1))-(U(I-1,J,K)+U(I,J,K)))/(2.*DELZ))
      VISZ3Y=(((VOID(I,J+1,K)+VOID(I,J+1,K+1))-(VOID(I,J-1,K)
     &+VOID(I,J-1,K+1)))/(4.*DELY))*((W(I,J+1,K)-W(I,J-1,K))/(2.*DELY)
     &+((V(I,J-1,K+1)+V(I,J,K+1))-(V(I,J-1,K)+V(I,J,K)))/(2.*DELZ))
      VISZ3Z=((VOID(I,J,K+1)-VOID(I,J,K))/(3.*DELZ))*(2.*(W(I,J,K+1)
     &-W(I,J,K-1))/DELZ-((UW2-UW4)/DELX+(VW2-VW4)/DELY))

      VISX(I,J,K)=(VISX1+VISX2/3.+VISX3X+VISX3Y+VISX3Z)/RE
      VISY(I,J,K)=(VISY1+VISY2/3.+VISY3X+VISY3Y+VISY3Z)/RE
      VISZ(I,J,K)=(VISZ1+VISZ2/3.+VISZ3X+VISZ3Y+VISZ3Z)/RE

 3300 CONTINUE

*****************************************************
*************** É|ÉAÉ\Éìï˚íˆéÆÇÃåvéZ ****************
*****************************************************

*********** à≥óÕÇÃã´äEèåèÇÃê›íË ************
**** íÍïî ****
      DO 2100 J=5,JM4
      DO 2100 I=5,IM4
              K=4
        P(I,J,K)=(P(I,J,K+1)*VOID(I,J,K+1)
     &    +DELZ*(FW(I,J,K)-VISZ(I,J,K)+SPZ(I,J,K)))
     &    /VOID(I,J,K)
 2100    CONTINUE

**** ç∂ë§ ****
      DO 2300 K=5,KM4
      DO 2300 J=5,JM4
              I=4
        P(I,J,K)=P(IM4,J,K)
 2300 CONTINUE

**** éËëOë§ *****
      DO 2500 K=5,KM4
      DO 2500 I=5,IM4
              J=4
        P(I,J,K)=(P(I,J+1,K)*VOID(I,J+1,K)
     &    +DELY*(FV(I,J,K)-VISY(I,J,K)+SPY(I,J,K)))
     &    /VOID(I,J,K)
 2500  CONTINUE

**** è„ïî ****
      DO 2400 J=5,JM4
      DO 2400 I=5,IM4
              K=KM3
        P(I,J,K)=(P(I,J,K-1)*VOID(I,J,K-1)
     &    -DELZ*(FW(I,J,K-1)-VISZ(I,J,K-1)+SPZ(I,J,K-1)))
     &    /VOID(I,J,K)
 2400 CONTINUE

**** âEë§ ****
      DO 2600 K=5,KM4
      DO 2600 J=5,JM4
              I=IM3
        P(I,J,K)=P(5,J,K)
 2600 CONTINUE

**** âúë§ ****
      DO 2700 K=5,KM4
      DO 2700 I=5,IM4
              J=JM3
        P(I,J,K)=(P(I,J-1,K)*VOID(I,J-1,K)
     &    -DELY*(FV(I,J-1,K)-VISY(I,J-1,K)+SPY(I,J-1,K)))
     &    /VOID(I,J,K)
 2700 CONTINUE

*********************************

 2800 CONTINUE

      IF(FLG.EQ.0.0) GO TO 4000
      ITER=ITER+1
      IF(ITER.LT.10000) GO TO 3050
      TA=1.0E+10
      GO TO 5060
 3050 FLG=0.0
      GO TO KRET
 3200 CONTINUE

*************************
***** écç∑RRMÇÃåvéZ *****
*************************

      DO 3500 K=5,KM4
      DO 3500 J=5,JM4
      DO 3500 I=5,IM4
      F1=FU(I,J,K)-FU(I-1,J,K)
      F2=FV(I,J,K)-FV(I,J-1,K)
      F3=FW(I,J,K)-FW(I,J,K-1)
      VIS1=VISX(I,J,K)-VISX(I-1,J,K)
      VIS2=VISY(I,J,K)-VISY(I,J-1,K)
      VIS3=VISZ(I,J,K)-VISZ(I,J,K-1)
      SP1=SPX(I,J,K)-SPX(I-1,J,K)
      SP2=SPY(I,J,K)-SPY(I,J-1,K)
      SP3=SPZ(I,J,K)-SPZ(I,J,K-1)
      Q=F1/DELX+F2/DELY+F3/DELZ
     &-VIS1/DELX-VIS2/DELY-VIS3/DELZ
     &+SP1/DELX+SP2/DELY+SP3/DELZ
      VOIDBX1=(VOIDB(I+1,J,K)+VOIDB(I,J,K))/2.
      VOIDBX2=(VOIDB(I,J,K)+VOIDB(I-1,J,K))/2.
      VOIDBY1=(VOIDB(I,J+1,K)+VOIDB(I,J,K))/2.
      VOIDBY2=(VOIDB(I,J,K)+VOIDB(I,J-1,K))/2.
      VOIDBZ1=(VOIDB(I,J,K+1)+VOIDB(I,J,K))/2.
      VOIDBZ2=(VOIDB(I,J,K)+VOIDB(I,J,K-1))/2.
      DN01=((VOIDBX1*UB(I,J,K)-VOIDBX2*UB(I-1,J,K))*RDX+(VOIDBY1*
     &VB1(I,J,K)-VOIDBY2*VB1(I,J-1,K))*RDY+(VOIDBZ1*WB(I,J,K)-VOIDBZ2*
     &WB(I,J,K-1))*RDZ)+(VOID(I,J,K)-VOIDB(I,J,K))/(2.*DELT)
      RR(I,J,K)=Q-DN01/(2.*DELT)

 3500 CONTINUE

**** ë¨ìxÉfÅ[É^ÇÃíuÇ´ä∑Ç¶ ****
      DO 4900 I=1,IMAX
      DO 4900 J=1,JMAX
      DO 4900 K=1,KMAX
        UB(I,J,K)=U(I,J,K)
        VB1(I,J,K)=V(I,J,K)
        WB(I,J,K)=W(I,J,K)
 4900 CONTINUE

 3600 CONTINUE

********************************
***** É|ÉAÉ\Éìï˚íˆéÆÇÃåvéZ *****
********************************

      DO 3700 K=5,KM4
      DO 3700 J=5,JM4
      DO 3700 I=5,IM4

      IF (CYCLE.EQ.1) THEN                             !íçà”10/22
        RRM(I,J,K)=0
      ELSE
        RRM(I,J,K)
     &  =((P(I+1,J,K)*VOID(I+1,J,K)+P(I-1,J,K)*VOID(I-1,J,K))
     &   *RDX**2
     &   +(P(I,J+1,K)*VOID(I,J+1,K)+P(I,J-1,K)*VOID(I,J-1,K))
     &   *RDY**2
     &   +(P(I,J,K+1)*VOID(I,J,K+1)+P(I,J,K-1)*VOID(I,J,K-1))
     &   *RDZ**2
     &   +RR(I,J,K))
     &   /(2.*(RDX**2+RDY**2+RDZ**2)*VOID(I,J,K))-P(I,J,K)
      ENDIF

 3700 CONTINUE

      RRMAX=0.0

******************
**** é˚ë©îªíË ****
******************

      IF (CYCLE.GT.1) THEN
      PMAX = 0.0
      DO 2755 K=4,KM3
      DO 2755 J=4,JM3
      DO 2755 I=4,IM3
      IF(ABS(P(I,J,K)).LE.PMAX) GOTO 2755
      PMAX = ABS(P(I,J,K))
 2755 CONTINUE
      EPSI=PMAX/5000.0                                 !é˚ë©îªíËåWêîåvéZ
      ENDIF

      DO 3800 K=5,KM4
      DO 3800 J=5,JM4
      DO 3800 I=5,IM4
      IF(ABS(RRM(I,J,K)).GE.EPSI) FLG=1.0
      P(I,J,K)=P(I,J,K)+OMG*RRM(I,J,K)
      IF(ABS(RRM(I,J,K)).LE.RRMAX) GO TO 3800
      RRMAX=ABS(RRM(I,J,K))
 3800 CONTINUE

**** ç∂ë§ ****
      DO 3802 K=5,KM4
      DO 3802 J=5,JM4
      P(4,J,K)=P(IM4,J,K)
 3802 CONTINUE

**** âEë§ ****
      DO 3804 K=5,KM4
      DO 3804 J=5,JM4
      P(IM3,J,K)=P(5,J,K)
 3804 CONTINUE

      ASSIGN 3600 TO KRET
      GO TO 2800

 4000 CONTINUE

******************************
***** ó¨ëÃÇÃêVë¨ìxÇÃåvéZ *****                         !êVë¨ìxåvéZ
******************************

      DO 3900 K=5,KM4
      DO 3900 J=5,JM4
      DO 3900 I=5,IM4
          PXYZ=P(I,J,K)*VOID(I,J,K)
          PXX=P(I+1,J,K)*VOID(I+1,J,K)
          PYY=P(I,J+1,K)*VOID(I,J+1,K)
          PZZ=P(I,J,K+1)*VOID(I,J,K+1)
          VOIDX=(VOID(I,J,K)+VOID(I+1,J,K))/2.0
          VOIDY=(VOID(I,J,K)+VOID(I,J+1,K))/2.0
          VOIDZ=(VOID(I,J,K)+VOID(I,J,K+1))/2.0
          U(I,J,K)=U(I,J,K)+DELT*((PXYZ-PXX)/DELX
     &      -FU(I,J,K)+VISX(I,J,K)-SPX(I,J,K))/VOIDX
          V(I,J,K)=V(I,J,K)+DELT*((PXYZ-PYY)/DELY
     &      -FV(I,J,K)+VISY(I,J,K)-SPY(I,J,K))/VOIDY
          W(I,J,K)=W(I,J,K)+DELT*((PXYZ-PZZ)/DELZ
     &      -FW(I,J,K)+VISZ(I,J,K)-SPZ(I,J,K))/VOIDZ
 3900 CONTINUE

**************************************
****** ó¨ëÃë¨ìxÇÃã´äEèåèÇÃê›íË ******                     !ã´äEèåè
**************************************

**** è„ïî ****
      DO 4915 I=5,IM4
      DO 4915 J=5,JM4
        U(I,J,KM3)= -U(I,J,KM4)+2.0*UW0/SU0
        U(I,J,KM2)= -U(I,J,KM5)+2.0*UW0/SU0
        U(I,J,KM1)= -U(I,J,KM6)+2.0*UW0/SU0
        U(I,J,KMAX)=-U(I,J,KM7)+2.0*UW0/SU0
        V(I,J,KM3)= -V(I,J,KM4)
        V(I,J,KM2)= -V(I,J,KM5)
        V(I,J,KM1)= -V(I,J,KM6)
        V(I,J,KMAX)=-V(I,J,KM7)
        W(I,J,KM4)= 0.0
        W(I,J,KM3)= W(I,J,KM5)
        W(I,J,KM2)= W(I,J,KM6)
        W(I,J,KM1)= W(I,J,KM7)
        W(I,J,KMAX)=W(I,J,KM7-1)
 4915 CONTINUE

**** íÍïî ****
      DO 4918 I=5,IM4
      DO 4918 J=5,JM4
        U(I,J,4)=-U(I,J,5)
        U(I,J,3)=-U(I,J,6)
        U(I,J,2)=-U(I,J,7)
        U(I,J,1)=-U(I,J,8)
        V(I,J,4)=-V(I,J,5)
        V(I,J,3)=-V(I,J,6)
        V(I,J,2)=-V(I,J,7)
        V(I,J,1)=-V(I,J,8)
        W(I,J,4)= 0.0
        W(I,J,3)= W(I,J,5)
        W(I,J,2)= W(I,J,6)
        W(I,J,1)= W(I,J,7)
 4918 CONTINUE

**** ç∂ë§ ****
      DO 4300 J=5,JM4
      DO 4300 K=5,KM4
        U(4,J,K)=U(IM4,J,K)
        U(3,J,K)=U(IM5,J,K)
        U(2,J,K)=U(IM6,J,K)
        U(1,J,K)=U(IM7,J,K)
        V(4,J,K)=V(IM4,J,K)
        V(3,J,K)=V(IM5,J,K)
        V(2,J,K)=V(IM6,J,K)
        V(1,J,K)=V(IM7,J,K)
        W(4,J,K)=W(IM4,J,K)
        W(3,J,K)=W(IM5,J,K)
        W(2,J,K)=W(IM6,J,K)
        W(1,J,K)=W(IM7,J,K)
 4300 CONTINUE

**** âEë§ ****
      DO 4302 J=5,JM4
      DO 4302 K=5,KM4
        U(IM3,J,K)= U(5,J,K)
        U(IM2,J,K)= U(6,J,K)
        U(IM1,J,K)= U(7,J,K)
        U(IMAX,J,K)=U(8,J,K)
        V(IM3,J,K)= V(5,J,K)
        V(IM2,J,K)= V(6,J,K)
        V(IM1,J,K)= V(7,J,K)
        V(IMAX,J,K)=V(8,J,K)
        W(IM3,J,K)= W(5,J,K)
        W(IM2,J,K)= W(6,J,K)
        W(IM1,J,K)= W(7,J,K)
        W(IMAX,J,K)=W(8,J,K)
 4302 CONTINUE

**** éËëOë§ ****
      DO 4911 I=5,IM4
      DO 4911 K=5,KM4
        U(I,4,K)=-U(I,5,K)
        U(I,3,K)=-U(I,6,K)
        U(I,2,K)=-U(I,7,K)
        U(I,1,K)=-U(I,8,K)
        V(I,4,K)= V(I,5,K)
        V(I,3,K)= V(I,6,K)
        V(I,2,K)= V(I,7,K)
        V(I,1,K)= V(I,8,K)
        W(I,4,K)=-W(I,5,K)
        W(I,3,K)=-W(I,6,K)
        W(I,2,K)=-W(I,7,K)
        W(I,1,K)=-W(I,8,K)
 4911 CONTINUE

**** âúë§ ****
      DO 4912 I=5,IM4
      DO 4912 K=5,KM4
        U(I,JM3,K)= -U(I,JM4,K)
        U(I,JM2,K)= -U(I,JM5,K)
        U(I,JM1,K)= -U(I,JM6,K)
        U(I,JMAX,K)=-U(I,JM7,K)
        V(I,JM4,K)=  0.0
        V(I,JM3,K)=  V(I,JM5,K)
        V(I,JM2,K)=  V(I,JM6,K)
        V(I,JM1,K)=  V(I,JM7,K)
        V(I,JMAX,K)= V(I,JM7-1,K)
        W(I,JM3,K)= -W(I,JM4,K)
        W(I,JM2,K)= -W(I,JM5,K)
        W(I,JM1,K)= -W(I,JM6,K)
        W(I,JMAX,K)=-W(I,JM7,K)
 4912 CONTINUE

********************************
***** DEMÇ…ÇÊÇÈó±éqÇÃåvéZ  *****                       !ó±éqÇÃåvéZ
********************************

      outcnt = 0

**** CLEAR ALL PARTICLE PARAMETERS ****

c      DO 1507 I=1,IP
c          NF(I)=I
c          MASSF(I)=MASS(I)
c        IF(R(I).GT.RADJ) THEN
c          nfl(I) = 1
c          nfs(I) = 0
c        ELSE
c          nfl(I) = 0
c          nfs(I) = 1
c        ENDIF
c 1507 CONTINUE

********************************

**************************************
***** ó±éqï˚íˆéÆíÜÇÃçRóÕçÄÇÃåvéZ *****
**************************************

      DO 20 iq = 1, IP

        ISUP=INT(X(iq)/DELX/SL0)+5
        JSUP=INT(Y(iq)/DELY/SL0)+5
        KSUP=INT(Z(iq)/DELZ/SL0)+5
        VOIDD=VOID(ISUP,JSUP,KSUP)

C        faip=3.757-5.376/VOIDD+2.619/(VOIDD**2)
        faip=1.0

        usp=(U(ISUP,JSUP,KSUP)+U(ISUP-1,JSUP,KSUP))/2.0
        vsp=(V(ISUP,JSUP,KSUP)+V(ISUP,JSUP-1,KSUP))/2.0
        wsp=(W(ISUP,JSUP,KSUP)+W(ISUP,JSUP,KSUP-1))/2.0

        usbp=(UB(ISUP,JSUP,KSUP)+UB(ISUP-1,JSUP,KSUP))/2.0
        vsbp=(VB1(ISUP,JSUP,KSUP)+VB1(ISUP,JSUP-1,KSUP))/2.0
        wsbp=(WB(ISUP,JSUP,KSUP)+WB(ISUP,JSUP,KSUP-1))/2.0

        upxp=vx(iq)-usp*SU0
        vpyp=vy(iq)-vsp*SU0
        wpzp=vz(iq)-wsp*SU0

******** ãÖëäìñíºåa ********

        rrr=(3.0*massf(nf(iq))/(4.0*pi*rhop))**0.3333333

        if(r(iq).gt.radj) then
          rpp=2.0*rrr/(2.0*float(nfl(nf(iq)))+float(nfs(nf(iq))))
        else
          rpp=    rrr/(2.0*float(nfl(nf(iq)))+float(nfs(nf(iq))))
        endif

        dpp=2.0*rpp

****************************

        FX(iq) = -3.0*PI*muf*dpp*upxp*faip

        FY(iq) = -3.0*PI*muf*dpp*vpyp*faip

        FZ(iq) = -3.0*PI*muf*dpp*wpzp*faip
     &           -980.7*CZ*mass(iq)*(rhop-rhof)/rhop


 20   CONTINUE


**** ãÖëäìñåaåvéZópÉfÅ[É^ÇÃèâä˙âª ****

      DO 507 I=1,IP
          NF(I)=I
          MASSF(I)=MASS(I)
        IF(R(I).GT.RADJ) THEN
          nfl(I) = 1
          nfs(I) = 0
        ELSE
          nfl(I) = 0
          nfs(I) = 1
        ENDIF
  507 CONTINUE

**************************
***** ó±éqÇÃê⁄êGåvéZ *****                             !ã´äEèåèÇÃçƒê›íË
**************************

      CALL PARTITION

      CALL CALCULATION

      CALL MOVE

C      WRITE(77,*)
C      WRITE(77,*) 'ê⁄êGåvéZå„',' FZ(çáåv) =',FZ(ich)
C      WRITE(77,*)


**************************************
***** ó¨ëÃï˚íˆéÆíÜÇÃçRóÕçÄÇÃåvéZ *****
**************************************

**** ó±éqà íuÇÃñ≥éüå≥âª ****

      DO 777 N=1,IP
        XP(N)= x(N)/SL0
        YP(N)= y(N)/SL0
        ZP(N)= z(N)/SL0
        UP(N)=vx(N)/SU0
        VP(N)=vy(N)/SU0
        WP(N)=vz(N)/SU0
 777  CONTINUE

      DO 6100 IP1=1,NPARTAX
      DO 6100 JP1=1,NPARTAY
      DO 6100 KP1=1,NPARTAZ
        PARTATL(IP1,JP1,KP1)=0
        DO 6100 NPP1=1,256
          PARTA(IP1,JP1,KP1,NPP1)=0
 6100 CONTINUE

***** ÉÅÉbÉVÉÖì‡ÇÃó±éqêîÇÃÉJÉEÉìÉg *****

      DO 6151 N=1,IP
        XPP=XP(N)
        YPP=YP(N)
        ZPP=ZP(N)
        II=INT(XPP/(DELX*DPARTAX))+1
        JJ=INT(YPP/(DELY*DPARTAY))+1
        KK=INT(ZPP/(DELZ*DPARTAZ))+1
        TNOA=PARTATL(II,JJ,KK)+1
        IF(TNOA.GT.256) GO TO 6151
        PARTATL(II,JJ,KK)=TNOA
        PARTA(II,JJ,KK,TNOA)=N 
 6151 CONTINUE

      DO 6153 K=1,KMAX
      DO 6153 J=1,JMAX
      DO 6153 I=1,IMAX
        VOLP(I,J,K)=0.0
 6153 CONTINUE

***** ãÛåÑó¶ÇÃåvéZ *****

      DO 6700 N=1,IP
        X1=XP(N)-DELX*INT(XP(N)/DELX)
        X2=DELX*(INT(XP(N)/DELX)+1)-XP(N)
        Y1=YP(N)-DELY*INT(YP(N)/DELY)
        Y2=DELY*(INT(YP(N)/DELY)+1)-YP(N)
        Z1=ZP(N)-DELZ*INT(ZP(N)/DELZ)
        Z2=DELZ*(INT(ZP(N)/DELZ)+1)-ZP(N)
        IX=INT(XP(N)/DELX)+5
        JY=INT(YP(N)/DELY)+5
        KZ=INT(ZP(N)/DELZ)+5
        XX1=DP(N)/2-X1
        XX2=DP(N)/2-X2
        YY1=DP(N)/2-Y1
        YY2=DP(N)/2-Y2
        ZZ1=DP(N)/2-Z1
        ZZ2=DP(N)/2-Z2
        XX1=MAX(XX1,0.0)
        XX2=MAX(XX2,0.0)
        YY1=MAX(YY1,0.0)
        YY2=MAX(YY2,0.0)
        ZZ1=MAX(ZZ1,0.0)
        ZZ2=MAX(ZZ2,0.0)
        VOLPX1=PI*XX1**2*(DP(N)+X1)/3.0
        VOLPX2=PI*XX2**2*(DP(N)+X2)/3.0
        VOLPY1=PI*YY1**2*(DP(N)+Y1)/3.0
        VOLPY2=PI*YY2**2*(DP(N)+Y2)/3.0
        VOLPZ1=PI*ZZ1**2*(DP(N)+Z1)/3.0
        VOLPZ2=PI*ZZ2**2*(DP(N)+Z2)/3.0
        VOLP(IX-1,JY,KZ)=VOLP(IX-1,JY,KZ)+VOLPX1
        VOLP(IX,JY-1,KZ)=VOLP(IX,JY-1,KZ)+VOLPY1
        VOLP(IX,JY,KZ-1)=VOLP(IX,JY,KZ-1)+VOLPZ1
        VOLP(IX,JY,KZ)=VOLP(IX,JY,KZ)+(PI*DP(N)**3)/6.0
     &    -(VOLPX1+VOLPY1+VOLPZ1+VOLPX2+VOLPY2+VOLPZ2)
        VOLP(IX+1,JY,KZ)=VOLP(IX+1,JY,KZ)+VOLPX2
        VOLP(IX,JY+1,KZ)=VOLP(IX,JY+1,KZ)+VOLPY2
        VOLP(IX,JY,KZ+1)=VOLP(IX,JY,KZ+1)+VOLPZ2
 6700 CONTINUE

      DO 6800 K=5,KM4
      DO 6800 J=5,JM4
      DO 6800 I=5,IM4
        VOIDD =(VOLC-VOLP(I,J,K))/VOLC
        VOIDB(I,J,K)=VOID(I,J,K)
        VOID(I,J,K)=VOIDD
 6800 CONTINUE

***** ãÛåÑó¶ÇÃã´äEèåèê›íË *****

**** íÍïî ****
      DO 7181 J=5,JM4
      DO 7181 I=5,IM4
        VOIDB(I,J,4)=VOIDB(I,J,5)
        VOID(I,J,4)=VOID(I,J,5)
        VOIDB(I,J,3)=VOIDB(I,J,6)
        VOID(I,J,3)=VOID(I,J,6)
        VOIDB(I,J,2)=VOIDB(I,J,7)
        VOID(I,J,2)=VOID(I,J,7)
        VOIDB(I,J,1)=VOIDB(I,J,8)
        VOID(I,J,1)=VOID(I,J,8)
 7181 CONTINUE

**** ç∂ë§ ****
      DO 7182 K=5,KM4
      DO 7182 J=5,JM4
        VOIDB(1,J,K)=VOIDB(IM7,J,K)
        VOID(1,J,K)=VOID(IM7,J,K)
        VOIDB(2,J,K)=VOIDB(IM6,J,K)
        VOID(2,J,K)=VOID(IM6,J,K)
        VOIDB(3,J,K)=VOIDB(IM5,J,K)
        VOID(3,J,K)=VOID(IM5,J,K)
        VOIDB(4,J,K)=VOIDB(IM4,J,K)
        VOID(4,J,K)=VOID(IM4,J,K)
 7182 CONTINUE

**** âEë§ ****
      DO 7183 K=5,KM4
      DO 7183 J=5,JM4
        VOIDB(IMAX,J,K)=VOIDB(8,J,K)
        VOID(IMAX,J,K)=VOID(8,J,K)
        VOIDB(IM1,J,K)=VOIDB(7,J,K)
        VOID(IM1,J,K)=VOID(7,J,K)
        VOIDB(IM2,J,K)=VOIDB(6,J,K)
        VOID(IM2,J,K)=VOID(6,J,K)
        VOIDB(IM3,J,K)=VOIDB(5,J,K)
        VOID(IM3,J,K)=VOID(5,J,K)
 7183 CONTINUE

**** è„ïî ****
      DO 7184 J=5,JM4
      DO 7184 I=5,IM4
        VOIDB(I,J,KMAX)=VOIDB(I,J,KM7)
        VOID(I,J,KMAX)=VOID(I,J,KM7)
        VOIDB(I,J,KM1)=VOIDB(I,J,KM6)
        VOID(I,J,KM1)=VOID(I,J,KM6)
        VOIDB(I,J,KM2)=VOIDB(I,J,KM5)
        VOID(I,J,KM2)=VOID(I,J,KM5)
        VOIDB(I,J,KM3)=VOIDB(I,J,KM4)
        VOID(I,J,KM3)=VOID(I,J,KM4)
 7184 CONTINUE

**** éËëOë§ ****
      DO 7185 K=5,KM4
      DO 7185 I=5,IM4
        VOIDB(I,1,K)=VOIDB(I,8,K)
        VOID(I,1,K)=VOID(I,8,K)
        VOIDB(I,2,K)=VOIDB(I,7,K)
        VOID(I,2,K)=VOID(I,7,K)
        VOIDB(I,3,K)=VOIDB(I,6,K)
        VOID(I,3,K)=VOID(I,6,K)
        VOIDB(I,4,K)=VOIDB(I,5,K)
        VOID(I,4,K)=VOID(I,5,K)
 7185 CONTINUE

**** âúë§ ****
      DO 7186 K=5,KM4
      DO 7186 I=5,IM4
        VOIDB(I,JMAX,K)=VOIDB(I,JM7,K)
        VOID(I,JMAX,K)=VOID(I,JM7,K)
        VOIDB(I,JM1,K)=VOIDB(I,JM6,K)
        VOID(I,JM1,K)=VOID(I,JM6,K)
        VOIDB(I,JM2,K)=VOIDB(I,JM5,K)
        VOID(I,JM2,K)=VOID(I,JM5,K)
        VOIDB(I,JM3,K)=VOIDB(I,JM4,K)
        VOID(I,JM3,K)=VOID(I,JM4,K)
 7186 CONTINUE

*************************************
**** ó¨ëÃÇÃï˚íˆéÆÇÃçRóÕçÄÇÃåvéZ *****                  !ó±éqÇ∆ó¨ëÃÇÃçRóÕ
*************************************

      DO 7209 I=1,IMAX
      DO 7209 J=1,JMAX
      DO 7209 K=1,KMAX
        SPX(I,J,K)=0.0
        SPY(I,J,K)=0.0
        SPZ(I,J,K)=0.0
 7209 CONTINUE

      DO 7201 II=1,NPARTAX
      DO 7201 JJ=1,NPARTAY
      DO 7201 KK=1,NPARTAZ
        TNOAK=PARTATL(II,JJ,KK)
      DO 7202 NO1=1,TNOAK
        N=PARTA(II,JJ,KK,NO1)
        IS1=INT(XP(N)/DELX)+5
        JS1=INT(YP(N)/DELY)+5
        KS1=INT(ZP(N)/DELZ)+5
        VOIDD=VOID(IS1,JS1,KS1)
        FAI=3.757-5.376/VOIDD+2.619/(VOIDD**2)
        US=(U(IS1,JS1,KS1)+U(IS1-1,JS1,KS1))/2.0
        VS=(V(IS1,JS1,KS1)+V(IS1,JS1-1,KS1))/2.0
        WS=(W(IS1,JS1,KS1)+W(IS1,JS1,KS1-1))/2.0
        UPXA=UP(N)-US
        VPYA=VP(N)-VS
        WPZA=WP(N)-WS
C        REP=RE*DP(N)*ABS(SQRT(UPXA**2+VPYA**2+WPZA**2))
        SPX(IS1,JS1,KS1)=SPX(IS1,JS1,KS1)+3.0*PI*DP(N)
C     &                   *(1+0.15*REP**0.687)
     &                   *(US-UP(N))*FAI/RE *0.0
        SPY(IS1,JS1,KS1)=SPY(IS1,JS1,KS1)+3.0*PI*DP(N)
C     &                   *(1+0.15*REP**0.687)
     &                   *(VS-VP(N))*FAI/RE *0.0
        SPZ(IS1,JS1,KS1)=SPZ(IS1,JS1,KS1)+3.0*PI*DP(N)
C     &                   *(1+0.15*REP**0.687)
     &                   *(WS-WP(N))*FAI/RE *0.0
 7202 CONTINUE
 7201 CONTINUE

***** SPçÄÇÃã´äEèåèê›íË *****
****  ç∂ë§ãyÇ—âEë§  ****
      DO 7210 K=5,KM4
      DO 7210 J=5,JM4
        SPX(4,J,K)=SPX(IM4,J,K)
        SPX(IM3,J,K)=SPX(5,J,K)
        SPY(4,J,K)=SPY(IM4,J,K)
        SPY(IM3,J,K)=SPY(5,J,K)
        SPZ(4,J,K)=SPZ(IM4,J,K)
        SPZ(IM3,J,K)=SPZ(5,J,K)
 7210 CONTINUE

****  éËëOë§ãyÇ—âúë§  ****
      DO 7220 K=5,KM4
      DO 7220 I=5,IM4
        SPX(I,4,K)=SPX(I,5,K)
        SPY(I,4,K)=SPY(I,5,K)
        SPZ(I,4,K)=SPZ(I,5,K)
        SPX(I,JM3,K)=SPX(I,JM4,K)
        SPY(I,JM3,K)=SPY(I,JM4,K)
        SPZ(I,JM3,K)=SPZ(I,JM4,K)
 7220 CONTINUE

**** íÍïîãyÇ—è„ïî  ****
      DO 7215 J=5,JM4
      DO 7215 I=5,IM4
        SPX(I,J,4)=SPX(I,J,5)
        SPY(I,J,4)=SPY(I,J,5)
        SPZ(I,J,4)=SPZ(I,J,5)
        SPX(I,J,KM3)=SPX(I,J,KM4)
        SPY(I,J,KM3)=SPY(I,J,KM4)
        SPZ(I,J,KM3)=SPZ(I,J,KM4)
 7215 CONTINUE


************************************************************


****************************
***** ÉfÅ[É^ÇÃèëÇ´èoÇµ *****                           !ÉfÅ[É^èëÇ´èoÇµ
****************************

      IF(CYCLE.GT.1) GO TO 5050

      WRITE(60,50) IBAR,JBAR,KBAR,DELX,DELY,DELZ,
     &             DELT,RE,UI,VI,WI,OMG,TWFIN
 5050 CONTINUE

      WRITE(60,77) CYCLE,ITER,RRMAX,EPSI
      IF(CYCLE.EQ.1) GO TO 6000
      TW=TWFIN-DELT
      IF(TA.GT.TW) GO TO 5060
      GO TO 6000
 5060 WRITE(60,77) CYCLE,ITER,RRMAX,EPSI

 6000 CONTINUE

      IF(MOD(CYCLE,NCYCLE).LT.1) THEN

***** ó±éqÉfÅ[É^ÇÃèëÇ´èoÇµ *****

      call post

***** ó¨ëÃÉfÅ[É^ÇÃèëÇ´èoÇµ *****

      open(unit = 10,err = 6060,status = 'old',file = 'acount')
      close(unit = 10, status = 'delete' )
 6060 open(unit = 10,err = 9904,status = 'new',file = 'acount')

      write(10,*) cycle
      close(10)

      open(unit = 11,err = 9904,status = 'old',file = 'acount')
      read(11,*) kota
      close(11)

      if(cycle.lt.10000) then
      kota1=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a000'//kota1//'.dat')
      goto 3000
      endif

      if((cycle.ge.10000).and.(cycle.lt.100000)) then
      kota2=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a00'//kota2//'.dat')
      goto 3000
      endif

      if((cycle.ge.100000).and.(cycle.lt.1000000)) then
      kota3=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a0'//kota3//'.dat')
      goto 3000
      endif

      if(cycle.ge.1000000) then
      kota4=kota
      OPEN (UNIT=1,ERR=9904,STATUS='NEW',FILE='a'//kota4//'.dat')
      goto 3000
      endif

******************************

 3000 continue

      WRITE(1,93) CYCLE
      DO 5203 I=1,IMAX
      DO 5203 J=1,JMAX
      DO 5203 K=1,KMAX
        WRITE(1,48) U(I,J,K),V(I,J,K),W(I,J,K),P(I,J,K)
        WRITE(1,78) UB(I,J,K),VB1(I,J,K),WB(I,J,K)
        WRITE(1,88) VOID (I,J,K),VOIDB(I,J,K)
 5203 CONTINUE
      DO 5303 I=1,IMAX
      DO 5303 J=1,JMAX
      DO 5303 K=1,KMAX
        WRITE(1,78) SPX(I,J,K),SPY(I,J,K),SPZ(I,J,K)
 5303 CONTINUE
      close( 1 )

      goto 9905
 9904 write(9,*) 'open error airfile01.dat (unit=1)'
 9905 continue

      ENDIF

***** âÊñ èoóÕ *****

      if(mod(cycle,100).lt.1) then
      WRITE(*,*)'CYCLE = ',CYCLE,'ITER=',ITER,'epsi=',epsi
      WRITE(*,*)
      endif

**********************
***** éûä‘ÇÃëOêi *****
**********************

      TA=TA+DELT                                       !åvéZéûä‘èIóπÇÃîªíË
      IF(TA.GT.TWFIN) GO TO 5100

      CYCLE=CYCLE+1

      GO TO 1000

***** åvéZèIóπÉfÅ[É^ÇÃèëÇ´èoÇµ *****

 5100 CONTINUE

C      close( 77 )

      TA=TA-DELT
      WRITE(60,110) TA,CYCLE+1
      WRITE(60,*) 'END KEKKA'

  999 CONTINUE

 6500 STOP

***** èëéÆê›íË *****

   48 FORMAT(4(1PE14.6))
   50 FORMAT(1H ,5X'IBAR= 'I3/6X'JBAR= 'I3/6X
     &'KBAR= 'I3/6X
     &'DELX= '1PE12.5/6X'DELY= 'E12.5/6X'DELZ= '1PE12.5/6X
     &'DELT= 'E12.5/8X'RE= 'E12.5/8X'UI= 'E12.5/8X
     &'VI= 'E12.5/8X'WI= 'E12.5/7X'OMG= 'E12.5/5X
     &'TWFIN= 'E12.5)
   77 FORMAT(2X'CYCLE= 'I6,4X,'ITER= 'I7,4X,'RRMAX= '1PE12.5
     &,4X,'EPSI= 'E12.5,5X)
   78 FORMAT(3(1PE14.6))
   88 FORMAT(2(1PE14.6))
   93 FORMAT(I7)
  110 FORMAT(2X'TIME=  '1PE12.5,4X'CYCLE= 'I6)


      END 

