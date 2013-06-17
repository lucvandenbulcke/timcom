MODULE SZONE2DZONE
USE TIMCOM_GENERAL,ONLY:MPASS,FLT1,FLT2,DAODT_GENERAL
IMPLICIT NONE
!REAL,DIMENSION(:,:),PRIVATE,ALLOCATABLE :: &
REAL,DIMENSION(:,:),ALLOCATABLE :: &
  XPEDZONE,UEDZONE,UI0DZONE,VI0DZONE,SI0DZONE,TI0DZONE,&
  UI1DZONE,VI1DZONE,SI1DZONE,TI1DZONE
INTEGER::I,J,K,JJ,II,N
REAL::TEMP,TMP
CONTAINS

SUBROUTINE SZONE2DZONE_INIT
USE OCN_PARA_SZONE,ONLY:K1_SZONE => K1 
USE OCN_PARA_DZONE,ONLY:J0_DZONE => J0
integer :: istat
ALLOCATE(XPEDZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(UEDZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(UI0DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(VI0DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(SI0DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(TI0DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)

ALLOCATE(UI1DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(VI1DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(SI1DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(TI1DZONE(J0_DZONE,K1_SZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)

XPEDZONE=0.;UEDZONE=0.;UI0DZONE=0.;VI0DZONE=0.;SI0DZONE=0.;TI0DZONE=0.
UI1DZONE=0.;VI1DZONE=0.;SI1DZONE=0.;TI1DZONE=0.
END SUBROUTINE SZONE2DZONE_INIT

SUBROUTINE SZONE2DZONE_COUPLE_OUT
USE GRID_VAR_SZONE,ONLY : U,U2,V2,S2,T2,XP
USE OCN_PARA_SZONE,ONLY:K1 !K1_SZONE => K1
USE CONTROL_DZONE,ONLY: DAODT_DZONE => DAODT,MXITN
#TAI_CASE # GIB_EXCLUDE_STA IBE_EXCLUDE_STA GOM_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY:J0 !J0_DZONE => J0
  DO K=1,K1 !K1_SZONE
  ! SZONE pg coupling to DZONE grid
  ! The 127th row (latitude) of cells in SZONE grid spans rows 2,3 of DZONE grid.
    DO J=1,J0-3,2 !J0_DZONE-3,2
      XPEDZONE(J,K)=.5*XP(2,(J+1)/2+125,K)
      XPEDZONE(J+1,K)=XPEDZONE(J,K)
    ENDDO
  ! =============================================
  ! MULTI-GRID COUPLING:
  ! Pass interface data from NPB grid to TAI grid
  ! =============================================
  ! Not all points are needed by TAI domain.
  ! The 127th row (latitude) of cells in NPB grid spans rows 2,3 of TAI grid.
! The southern ghost zones are included in this count.
! J0_DZONE is DZONE model latitudinal resolution
    DO J=2,J0-1 !J0_DZONE-1
      UEDZONE(J,K)=U(2,(J-2)/2+127,K)
      UI0DZONE(J,K)=U2(3,(J-2)/2+127,K)
      VI0DZONE(J,K)=V2(3,(J-2)/2+127,K)
      SI0DZONE(J,K)=S2(3,(J-2)/2+127,K)
      TI0DZONE(J,K)=T2(3,(J-2)/2+127,K)
    ENDDO
  ENDDO
#TAI_CASE # IBE_EXCLUDE_END GOM_EXCLUDE_END GIB_EXCLUDE_END NAB_EXCLUDE_END
#GIB_CASE # NAB_EXCLUDE_STA IBE_EXCLUDE_STA GOM_EXCLUDE_STA TAI_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY:J0 !J0_DZONE => J0
INTEGER:: JMED
! ========================================
! Pass interface data from MED to GIB grid
! ========================================
  TEMP=1./3.
  DO K=1,K1
! pg coupling to GIB grid
! GIB lat resolution is 3 times that of MED
! cell centers J=59,60,61 in GIB correspond to J=51 in MED
    DO J=59,J0-2
      XPEGIB(J,K)=TEMP*XP(2,51+(J-59)/3,K)
    ENDDO
    DO J=59,J0-1
      JMED=51+(J-59)/3
      UEGIB(J,K)=U(2,JMED,K)
      UI0GIB(J,K)=U2(3,JMED,K)
      VI0GIB(J,K)=V2(3,JMED,K)
      SI0GIB(J,K)=S2(3,JMED,K)
      TI0GIB(J,K)=T2(3,JMED,K)
    ENDDO
  ENDDO
#GIB_CASE # NAB_EXCLUDE_END IBE_EXCLUDE_END GOM_EXCLUDE_END TAI_EXCLUDE_END
#IBE_CASE # NAB_EXCLUDE_STA TAI_EXCLUDE_STA GIB_EXCLUDE_STA GOM_EXCLUDE_STA
USE OCN_PARA_SZONE,ONLY:J0,J1 !J0_SZONE => J0
INTEGER:: JIBE
! Pass interface data from GIB to IBE grid
! ========================================
  TMP=1./3.
  TEMP=1./9.
  DO K=1,K1
! pg coupling to IBE grid
! 282nd row (latitude) of cells in IBE grid spans rows 2,3,4 of GIB grid.
    JDZONE=280
    DO J=3,J1,3
      JIBE=JIBE+1
! factors of 3 cancel out
      XPEIBE(JIBE,K)=XP(4,J-2,K)+XP(4,J-1,K)+XP(4,J,K)
    ENDDO
    JIBE=281
    DO J=2,J0-3,3
      JIBE=JIBE+1
      UEIBE(JIBE,K)=TMP*(U(4,J,K)+U(4,J+1,K)+U(4,J+2,K))
      VI0IBE(JIBE,K)=TMP*(V2(5,J,K)+V2(5,J+1,K)+V2(5,J+2,K))
      SI0IBE(JIBE,K)=TMP*(S2(5,J,K)+S2(5,J+1,K)+S2(5,J+2,K))
      TI0IBE(JIBE,K)=TMP*(T2(5,J,K)+T2(5,J+1,K)+T2(5,J+2,K))
      UI1IBE(JIBE,K)=TEMP*(U2(2,J,K)+U2(2,J+1,K)+U2(2,J+2,K)+U2(3,J,K)&
        +U2(3,J+1,K)+U2(3,J+2,K)+U2(4,J,K)+U2(4,J+1,K)+U2(4,J+2,K))
      VI1IBE(JIBE,K)=TEMP*(V2(2,J,K)+V2(2,J+1,K)+V2(2,J+2,K)+V2(3,J,K)&
        +V2(3,J+1,K)+V2(3,J+2,K)+V2(4,J,K)+V2(4,J+1,K)+V2(4,J+2,K))
      SI1IBE(JIBE,K)=TEMP*(S2(2,J,K)+S2(2,J+1,K)+S2(2,J+2,K)+S2(3,J,K)&
        +S2(3,J+1,K)+S2(3,J+2,K)+S2(4,J,K)+S2(4,J+1,K)+S2(4,J+2,K))
      TI1IBE(JIBE,K)=TEMP*(T2(2,J,K)+T2(2,J+1,K)+T2(2,J+2,K)+T2(3,J,K)&
        +T2(3,J+1,K)+T2(3,J+2,K)+T2(4,J,K)+T2(4,J+1,K)+T2(4,J+2,K))
    ENDDO
  ENDDO
#IBE_CASE # NAB_EXCLUDE_END GIB_EXCLUDE_END GOM_EXCLUDE_END TAI_EXCLUDE_END
#NAB_CASE # IBE_EXCLUDE_STA GOM_EXCLUDE_STA TAI_EXCLUDE_STA GIB_EXCLUDE_STA
USE OCN_PARA_SZONE,ONLY:J0,J1 !J0_DZONE => J0
INTEGER:: JIBE
REAL::UESUM,UI0SUM,VI0SUM,SI0SUM,TI0SUM,UI1SUM,VI1SUM,SI1SUM,TI1SUM
! ---------------------------------------------
! Pass interface data from IBE grid to NAB grid
! ---------------------------------------------
  DO K=1,K1
! pg coupling to NAB grid
! 2nd row (latitude) of cells in NAB grid spans rows 2,3 of IBE grid.
    DO J=2,J1,2
! factors of two cancel out!
      XPENAB(J/2,K)=XP(3,J-1,K)+XP(3,J,K)
    ENDDO
    JJ=1
! J0-2 should be evenly divisible by 2 to match up with NAB overlap
    DO J=2,J0-2,2
      JJ=JJ+1
! NAB ghost zones
      UESUM=0.;UI0SUM=0.;VI0SUM=0.;SI0SUM=0.;TI0SUM=0.
! NAB first interior (I=I1) zones
      UI1SUM=0.;VI1SUM=0.;SI1SUM=0.;TI1SUM=0.
! We use uniformly-weighted rather than DY-weighted averages, because
! the latitudinal variation of DY between adjacent zones is very small
      DO N=1,2
! NAB eastern inflow velocity:
! UENAB(JJ,K) goes to the easternmost grid line of the NAB domain.
        UESUM=UESUM+U(3,J+N-1,K)
! UI0SUM       goes to eastern ghost zone of the NAB grid
! NAB ghost zones (east of easternmost NAB domain grid line) come from
! first zones to east of U(3,J,K)
! NAB ghost zones cover exactly 4 zones of IBE grid
        UI0SUM=UI0SUM+U2(4,J+N-1,K)+U2(5,J+N-1,K)
        VI0SUM=VI0SUM+V2(4,J+N-1,K)+V2(5,J+N-1,K)
        SI0SUM=SI0SUM+S2(4,J+N-1,K)+S2(5,J+N-1,K)
        TI0SUM=TI0SUM+T2(4,J+N-1,K)+T2(5,J+N-1,K)
! NAB easternmost interior zones cover exactly 4 zones of IBE grid
        UI1SUM=UI1SUM+U2(2,J+N-1,K)+U2(3,J+N-1,K)
        VI1SUM=VI1SUM+V2(2,J+N-1,K)+V2(3,J+N-1,K)
        SI1SUM=SI1SUM+S2(2,J+N-1,K)+S2(3,J+N-1,K)
        TI1SUM=TI1SUM+T2(2,J+N-1,K)+T2(3,J+N-1,K)
      ENDDO
      UENAB(JJ,K)=.5*UESUM
      UI0NAB(JJ,K)=.25*UI0SUM
      VI0NAB(JJ,K)=.25*VI0SUM
      SI0NAB(JJ,K)=.25*SI0SUM
      TI0NAB(JJ,K)=.25*TI0SUM
      UI1NAB(JJ,K)=.25*UI1SUM
      VI1NAB(JJ,K)=.25*VI1SUM
      SI1NAB(JJ,K)=.25*SI1SUM
      TI1NAB(JJ,K)=.25*TI1SUM
    ENDDO
  ENDDO
#NAB_CASE # IBE_EXCLUDE_END GOM_EXCLUDE_END TAI_EXCLUDE_END GIB_EXCLUDE_END
#GOM_CASE # IBE_EXCLUDE_STA GIB_EXCLUDE_STA TAI_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY:J0 !J0_DZONE => J0
INTEGER:: JIBE
  DO K=1,K1
    DO J=1,J0-3,2
      XPEGOM(J,K)=.5*XP(2,(J+1)/2+38,K)
      XPEGOM(J+1,K)=XPEGOM(J,K)
    ENDDO
    DO J=2,J0-1
      UEGOM(J,K)=U(2,(J-2)/2+40,K)
      UI0GOM(J,K)=U2(3,(J-2)/2+40,K)
      VI0GOM(J,K)=V2(3,(J-2)/2+40,K)
      SI0GOM(J,K)=S2(3,(J-2)/2+40,K)
      TI0GOM(J,K)=T2(3,(J-2)/2+40,K)
    ENDDO
 ENDDO
#GOM_CASE # IBE_EXCLUDE_END GIB_EXCLUDE_END TAI_EXCLUDE_END NAB_EXCLUDE_END
MXITN=DAODT_DZONE/DAODT_GENERAL
!#ifdef _DEBUG_
!print *, "XPEDZONE",maxval(XPEDZONE),minval(XPEDZONE),maxloc(XPEDZONE),minloc(XPEDZONE),sum(XPEDZONE)
!print *, "UEDZONE",maxval(UEDZONE),minval(UEDZONE),maxloc(UEDZONE),minloc(UEDZONE),sum(UEDZONE)
!print *, "UI0DZONE",maxval(UI0DZONE),minval(UI0DZONE),maxloc(UI0DZONE),minloc(UI0DZONE),sum(UI0DZONE)
!print *, "VI0DZONE",maxval(VI0DZONE),minval(VI0DZONE),maxloc(VI0DZONE),minloc(VI0DZONE),sum(VI0DZONE)
!print *, "SI0DZONE",maxval(SI0DZONE),minval(SI0DZONE),maxloc(SI0DZONE),minloc(SI0DZONE),sum(SI0DZONE)
!print *, "TI0DZONE",maxval(TI0SZONE),minval(TI0SZONE),maxloc(TI0SZONE),minloc(TI0SZONE),sum(TI0SZONE)
!pause
!#endif

END SUBROUTINE SZONE2DZONE_COUPLE_OUT

SUBROUTINE SZONE2DZONE_COUPLE_IN
USE OCN_PARA_SZONE,ONLY : K1
USE OCN_PARA_DZONE,ONLY : I0,I1,I2,J1
USE GRID_VAR_DZONE,ONLY : U,IN,U2,V2,S2,T2,U1,V1,S1,T1,XP
#TAI_CASE # GIB_EXCLUDE_STA IBE_EXCLUDE_STA GOM_EXCLUDE_STA NAB_EXCLUDE_STA
  DO K=1,K1
  DO J=2,J1
  ! "IN" factor eliminates possible tiny flow over land from NPB grid
    U(I1,J,K)=IN(I1,J,K)*UEDZONE(J,K)
    !XP(I1,J-1,K)=XPEDZONE(J-1,K)
  ! Neuman conditions (could be done in FSTAI)
    XP(I1,J-1,K)=XP(I2,J-1,K)
    U2(I0,J,K)=UI0DZONE(J,K)
    V2(I0,J,K)=VI0DZONE(J,K)
    !S2(I0,J,K)=SI0DZONE(J,K)
    !T2(I0,J,K)=TI0DZONE(J,K)
  ENDDO
  ENDDO
#TAI_CASE # GIB_EXCLUDE_END IBE_EXCLUDE_END GOM_EXCLUDE_END NAB_EXCLUDE_END
#GIB_CASE # TAI_EXCLUDE_STA IBE_EXCLUDE_STA GOM_EXCLUDE_STA NAB_EXCLUDE_STA
! ================================
! Use interface data from MED grid
! ================================
  DO K=1,K1
    DO J=2,J1
! "IN" factor eliminates possible tiny flow over land from MED grid
      U(I1,J,K)=IN(I1,J,K)*UEGIB(J,K)
      XP(I1,J-1,K)=XPEGIB(J-1,K)
      U2(I0,J,K)=UI0GIB(J,K)
      V2(I0,J,K)=VI0GIB(J,K)
      S2(I0,J,K)=SI0GIB(J,K)
      T2(I0,J,K)=TI0GIB(J,K)
    ENDDO
  ENDDO
#GIB_CASE # TAI_EXCLUDE_END IBE_EXCLUDE_END GOM_EXCLUDE_END NAB_EXCLUDE_END
#IBE_CASE # TAI_EXCLUDE_STA GIB_EXCLUDE_STA GOM_EXCLUDE_STA NAB_EXCLUDE_STA
! =========================================
! Use interface data from GIB and VIS grids
! =========================================
  DO K=1,K1
!   DO J=2,J1
! hardwire to avoid setting i=i1 data to zero at latitudes
! having no GIB or MED coupling!
    DO J=34,386
      XP(I1,J-1,K)=FLT1*XPEIBE(J-1,K)+FLT2*XP(I1,J-1,K)
! NBV
      U(I1,J,K)=IN(I1,J,K)*(FLT1*UEIBE(J,K)+FLT2*U(I1,J,K))
! ghost zones
      U2(I0,J,K)=FLT1*UI0IBE(J,K)+FLT2*U2(I0,J,K)
      V2(I0,J,K)=FLT1*VI0IBE(J,K)+FLT2*V2(I0,J,K)
      S2(I0,J,K)=FLT1*SI0IBE(J,K)+FLT2*S2(I0,J,K)
      T2(I0,J,K)=FLT1*TI0IBE(J,K)+FLT2*T2(I0,J,K)
! first interior zone
      U2(I1,J,K)=FLT1*UI1IBE(J,K)+FLT2*U2(I1,J,K)
      V2(I1,J,K)=FLT1*VI1IBE(J,K)+FLT2*V2(I1,J,K)
      S2(I1,J,K)=FLT1*SI1IBE(J,K)+FLT2*S2(I1,J,K)
      T2(I1,J,K)=FLT1*TI1IBE(J,K)+FLT2*T2(I1,J,K)
    ENDDO
  ENDDO
#IBE_CASE # GIB_EXCLUDE_END TAI_EXCLUDE_END GOM_EXCLUDE_END NAB_EXCLUDE_END
#NAB_CASE # TAI_EXCLUDE_STA IBE_EXCLUDE_STA GOM_EXCLUDE_STA GIB_EXCLUDE_STA
! Pass interface data from IBE grid to NAB grid
! from overlap region with IBE grid
  DO K=1,K1
    DO J=2,J1
      XP(I1,J-1,K)=FLT1*XPENAB(J-1,K)+FLT2*XP(I1,J-1,K)
      U(I1,J,K)=IN(I1,J,K)*(FLT1*UENAB(J,K)+FLT2*U(I1,J,K))
      U2(I0,J,K)=FLT1*UI0NAB(J,K)+FLT2*U2(I0,J,K)
      V2(I0,J,K)=FLT1*VI0NAB(J,K)+FLT2*V2(I0,J,K)
      S2(I0,J,K)=FLT1*SI0NAB(J,K)+FLT2*S2(I0,J,K)
      T2(I0,J,K)=FLT1*TI0NAB(J,K)+FLT2*T2(I0,J,K)
! first interior zone
      U1(I1,J,K)=FLT1*UI1NAB(J,K)+FLT2*U1(I1,J,K)
      V1(I1,J,K)=FLT1*VI1NAB(J,K)+FLT2*V1(I1,J,K)
      S1(I1,J,K)=FLT1*SI1NAB(J,K)+FLT2*S1(I1,J,K)
      T1(I1,J,K)=FLT1*TI1NAB(J,K)+FLT2*T1(I1,J,K)
    ENDDO
  ENDDO
#NAB_CASE # GIB_EXCLUDE_END IBE_EXCLUDE_END GOM_EXCLUDE_END TAI_EXCLUDE_END
#GOM_CASE # TAI_EXCLUDE_STA IBE_EXCLUDE_STA GIB_EXCLUDE_STA NAB_EXCLUDE_STA
! UEGOM(J,K) is from NAB grid U(2,(J-1)/3+2,K)
! time filter eastern GOM boundary data
  DO K=1,K1
    DO J=2,J1
! "IN" factor eliminates possible tiny flow over land from NAB grid
      U(I1,J,K)=IN(I1,J,K)*UEGOM(J,K)
      XP(I1,J-1,K)=XPEGOM(J-1,K)
      U2(I0,J,K)=UI0GOM(J,K)
      V2(I0,J,K)=VI0GOM(J,K)
      S2(I0,J,K)=SI0GOM(J,K)
      T2(I0,J,K)=TI0GOM(J,K)
    ENDDO
  ENDDO
#GOM_CASE # GIB_EXCLUDE_END IBE_EXCLUDE_END TAI_EXCLUDE_END NAB_EXCLUDE_END
END SUBROUTINE SZONE2DZONE_COUPLE_IN

SUBROUTINE ALLOCATE_STATUS(istat)
INTEGER :: istat
if (istat/=0) STOP "Cannot allocate memory"
END SUBROUTINE ALLOCATE_STATUS

END MODULE SZONE2DZONE
