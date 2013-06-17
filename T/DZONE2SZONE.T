MODULE DZONE2SZONE
USE TIMCOM_GENERAL,ONLY:MPASS,FLT1,FLT2,RESTARTSIG
IMPLICIT NONE
!REAL,DIMENSION(:,:),PRIVATE,ALLOCATABLE :: & 
REAL,DIMENSION(:,:),ALLOCATABLE :: & 
  XPWSZONE,UWSZONE,U1SZONE,V1SZONE,S1SZONE,T1SZONE,U2SZONE,V2SZONE,S2SZONE,T2SZONE
INTEGER::I,J,K,JJ,II,N
REAL::TEMP,TMP,TTMP1,TTMP2
CONTAINS

SUBROUTINE DZONE2SZONE_INIT
USE OCN_PARA_DZONE,ONLY:J0_DZONE => J0, K1_DZONE => K1
USE OCN_PARA_SZONE,ONLY:J0_SZONE => J0
integer :: istat
ALLOCATE(XPWSZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(UWSZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(U1SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(V1SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(S1SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(T1SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(U2SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(V2SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(S2SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)
ALLOCATE(T2SZONE(J0_SZONE,K1_DZONE),STAT=istat)
CALL ALLOCATE_STATUS(istat)

XPWSZONE=0.;UWSZONE=0.;U1SZONE=0.;V1SZONE=0.;S1SZONE=0.;T1SZONE=0.;U2SZONE=0.;V2SZONE=0.;S2SZONE=0.;T2SZONE=0.
RESTART_CONDITION:IF (RESTARTSIG.NE.0) THEN 

!READ(19) UEDZONE,UI0DZONE,VI0DZONE,SI0DZONE,TI0DZONE, &
 !     UWSZONE,U1SZONE,V1SZONE,S1SZONE,T1SZONE,U2SZONE,V2SZONE,S2SZONE,T2SZONE

!#ifdef _DEBUG_      
      !print *, "SZONE UEDZONE", maxval(UEDZONE),minval(UEDZONE),maxloc(UEDZONE),minloc(UEDZONE),sum(UEDZONE)
      !print *, "SZONE UI0DZONE", maxval(UI0DZONE),minval(UI0DZONE),maxloc(UI0DZONE),minloc(UI0DZONE),sum(UI0DZONE)
      !print *, "SZONE VI0DZONE", maxval(VI0DZONE),minval(VI0DZONE),maxloc(VI0DZONE),minloc(VI0DZONE),sum(VI0DZONE)
      !print *, "SZONE SI0DZONE", maxval(SI0DZONE),minval(SI0DZONE),maxloc(SI0DZONE),minloc(SI0DZONE),sum(SI0DZONE)
      !print *, "SZONE TI0DZONE", maxval(TI0DZONE),minval(TI0DZONE),maxloc(TI0DZONE),minloc(TI0DZONE),sum(TI0DZONE)
      !print *, "SZONE UWSZONE", maxval(UWSZONE),minval(UWSZONE),maxloc(UWSZONE),minloc(UWSZONE),sum(UWSZONE)
      !print *, "SZONE U1SZONE", maxval(U1SZONE),minval(U1SZONE),maxloc(U1SZONE),minloc(U1SZONE),sum(U1SZONE)
      !print *, "SZONE V1SZONE", maxval(V1SZONE),minval(V1SZONE),maxloc(V1SZONE),minloc(V1SZONE),sum(V1SZONE)
      !print *, "SZONE S1SZONE", maxval(S1SZONE),minval(S1SZONE),maxloc(S1SZONE),minloc(S1SZONE),sum(S1SZONE)
      !print *, "SZONE T1SZONE", maxval(T1SZONE),minval(T1SZONE),maxloc(T1SZONE),minloc(T1SZONE),sum(T1SZONE)
      !print *, "SZONE U2SZONE", maxval(U2SZONE),minval(U2SZONE),maxloc(U2SZONE),minloc(U2SZONE),sum(U2SZONE)
      !print *, "SZONE V2SZONE", maxval(V2SZONE),minval(V2SZONE),maxloc(V2SZONE),minloc(V2SZONE),sum(V2SZONE)
      !print *, "SZONE S2SZONE", maxval(S2SZONE),minval(S2SZONE),maxloc(S2SZONE),minloc(S2SZONE),sum(S2SZONE)
      !print *, "SZONE T2SZONE", maxval(T2SZONE),minval(T2SZONE),maxloc(T2SZONE),minloc(T2SZONE),sum(T2SZONE)
!#endif

 ENDIF RESTART_CONDITION
                                                                                    
CLOSE(19)

END SUBROUTINE DZONE2SZONE_INIT

SUBROUTINE DZONE2SZONE_COUPLE_IN
USE OCN_PARA_DZONE,ONLY:K1
USE GRID_VAR_SZONE,ONLY: U,IN,U1,U2,ULF,V1,V2,VLF,S1,S2,SLF,T1,T2,TLF,XP
#NPB_CASE # MED_EXCLUDE_STA GIB_EXCLUDE_STA IBE_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY:J0_DZONE => J0
  JJ=(J0_DZONE-2)/2+1
  DO K=1,K1
! =============================================
! Pass interface data from DZONE grid to SZONE grid
! =============================================
! Overlap with DZONE, J0T=440
  DO J=2,JJ
    N=J+125
!  XP(1,N-1,K)=FLT1*XPWSZONE(J-1,K)+FLT2*XP(1,N-1,K)
! no pg filter
!  XP(1,N-1,K)=XPWSZONE(J-1,K)
! no pg coupling
!  XP(1,N-1,K)=0.
! Neuman bcs (could be done in FSSZONE)
    XP(1,N-1,K)=XP(2,N-1,K)
! The 127th row (latitude) of cells in SZONE grid spans rows 2,3 of DZONE grid.
! The southern ghost zones are included in this count.
! Thus, UWSZONE(2,K) is the sum of rows 2 and 3 of eastern DZONE cells.
! from U(I0-3,J,K) of DZONE grid
!! "IN" factor eliminates possible tiny flow over land from DZONE grid
    U(1,N,K)=IN(2,N,K)*(FLT1*UWSZONE(J,K)+FLT2*U(1,N,K))
!  U(1,N,K)=IN(2,N,K)*UWSZONE(J,K)
! no time filter needed due to spatial averaging
    U2(1,N,K)=U1SZONE(J,K)
    V2(1,N,K)=V1SZONE(J,K)
    S2(1,N,K)=S1SZONE(J,K)
    T2(1,N,K)=T1SZONE(J,K)
!! first interior zone
    TMP=U2SZONE(J,K)-U1(2,N,K)
    U1(2,N,K)=U1(2,N,K)+TMP
    U2(2,N,K)=U2(2,N,K)+TMP
    ULF(2,N,K)=ULF(2,N,K)+TMP
    TMP=V2SZONE(J,K)-V1(2,N,K)
    V1(2,N,K)=V1(2,N,K)+TMP
    V2(2,N,K)=V2(2,N,K)+TMP
    VLF(2,N,K)=VLF(2,N,K)+TMP
    TMP=S2SZONE(J,K)-S1(2,N,K)
    ttmp1= S1(2,N,K)
    S1(2,N,K)=S1(2,N,K)+TMP
    ttmp2= S2(2,N,K)
    S2(2,N,K)=S2(2,N,K)+TMP
    !IF (S2(2,N,K)<0.) PRINT *, "S2<0", ttmp1,ttmp2,S1(2,N,K),S2(2,N,K),N,K,TMP,S2SZONE(J,K)
    SLF(2,N,K)=SLF(2,N,K)+TMP
    TMP=T2SZONE(J,K)-T1(2,N,K)
    T1(2,N,K)=T1(2,N,K)+TMP
    T2(2,N,K)=T2(2,N,K)+TMP
    TLF(2,N,K)=TLF(2,N,K)+TMP
    !U1(2,N,K)=U2SZONE(J,K)
    !V1(2,N,K)=V2SZONE(J,K)
    !S1(2,N,K)=S2SZONE(J,K)
    !T1(2,N,K)=T2SZONE(J,K)
  ENDDO
  ENDDO
#NPB_CASE # MED_EXCLUDE_END GIB_EXCLUDE_END IBE_EXCLUDE_END NAB_EXCLUDE_END
#MED_CASE # NPB_EXCLUDE_STA GIB_EXCLUDE_STA IBE_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_SZONE,ONLY:J1
! ================================
! Use interface data from GIB grid
! ================================
  DO K=1,K1
    DO J=2,J1
      XP(1,J-1,K)=FLT1*XPWMED(J-1,K)+FLT2*XP(1,J-1,K)
! "IN" factor eliminates possible tiny flow over land from GIB grid
      U(1,J,K)=IN(2,J,K)*(FLT1*UWMED(J,K)+FLT2*U(1,J,K))
      U2(1,J,K)=FLT1*U1MED(J,K)+FLT2*U2(1,J,K)
      V2(1,J,K)=FLT1*V1MED(J,K)+FLT2*V2(1,J,K)
      S2(1,J,K)=FLT1*S1MED(J,K)+FLT2*S2(1,J,K)
      T2(1,J,K)=FLT1*T1MED(J,K)+FLT2*T2(1,J,K)
! first interior zone
      U2(2,J,K)=FLT1*U2MED(J,K)+FLT2*U2(2,J,K)
      V2(2,J,K)=FLT1*V2MED(J,K)+FLT2*V2(2,J,K)
      S2(2,J,K)=FLT1*S2MED(J,K)+FLT2*S2(2,J,K)
      T2(2,J,K)=FLT1*T2MED(J,K)+FLT2*T2(2,J,K)
    ENDDO
  ENDDO
#MED_CASE # NPB_EXCLUDE_END GIB_EXCLUDE_END IBE_EXCLUDE_END NAB_EXCLUDE_END
#GIB_CASE # NPB_EXCLUDE_STA NAB_EXCLUDE_STA IBE_EXCLUDE_STA MED_EXCLUDE_STA
USE OCN_PARA_SZONE,ONLY:J1
! ================================
! Use interface data from IBE grid
! ================================
  DO K=1,K1
    DO J=2,J1
! "IN" factor eliminates possible tiny flow over land from IBE grid
      U(1,J,K)=IN(2,J,K)*UWGIB(J,K)
      XP(1,J-1,K)=XPWGIB(J-1,K)
      U2(1,J,K)=U1GIB(J,K)
      V2(1,J,K)=V1GIB(J,K)
      S2(1,J,K)=S1GIB(J,K)
      T2(1,J,K)=T1GIB(J,K)
    ENDDO
  ENDDO
#GIB_CASE # MED_EXCLUDE_END NAB_EXCLUDE_END IBE_EXCLUDE_END NPB_EXCLUDE_END
#IBE_CASE # NPB_EXCLUDE_STA GIB_EXCLUDE_STA MED_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_SZONE,ONLY:J1
! ================================
! Use interface data from NAB grid
! ================================
  DO K=1,K1
    DO J=2,J1
! "IN" factor eliminates possible tiny flow over land from NAB grid
      U(1,J,K)=IN(2,J,K)*UWIBE(J,K)
      XP(1,J-1,K)=XPWIBE(J-1,K)
      S2(1,J,K)=S1IBE(J,K)
      T2(1,J,K)=T1IBE(J,K)
      U2(1,J,K)=U1IBE(J,K)
      V2(1,J,K)=V1IBE(J,K)
    ENDDO
  ENDDO
#IBE_CASE # MED_EXCLUDE_END GIB_EXCLUDE_END NPB_EXCLUDE_END NAB_EXCLUDE_END
#NAB_CASE # NPB_EXCLUDE_STA GIB_EXCLUDE_STA IBE_EXCLUDE_STA MED_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY:J0
! Pass interface data from GOM grid to NAB grid
 
  DO K=1,K1
     JJ=(J0-2)/2+1
     DO J=2,JJ
      N=J+38
! The 40th row (latitude) of cells in NAB grid spans rows 2,3 of GOM grid.
! The southern ghost zones are included in this count.
! Thus, UWNAB(2,K) is the sum of rows 2 and 3 of eastern GOM cells.
! from U(I0-3,J,K) of GOM grid
! "IN" factor eliminates possible tiny flow over land from GOM grid
      XP(1,N-1,K)=FLT1*XPWNAB(J-1,K)+FLT2*XP(1,N-1,K)
      U(1,N,K)=IN(2,N,K)*(FLT1*UWNAB(J,K)+FLT2*U(1,N,K))
!     U2(1,N,K)=U(1,N,K)
      U2(1,N,K)=FLT1*U1NAB(J,K)+FLT2*U2(1,N,K)
      V2(1,N,K)=FLT1*V1NAB(J,K)+FLT2*V2(1,N,K)
      S2(1,N,K)=FLT1*S1NAB(J,K)+FLT2*S2(1,N,K)
      T2(1,N,K)=FLT1*T1NAB(J,K)+FLT2*T2(1,N,K)
! first interior zone
      U1(2,N,K)=FLT1*U2NAB(J,K)+FLT2*U1(2,N,K)
      V1(2,N,K)=FLT1*V2NAB(J,K)+FLT2*V1(2,N,K)
      S1(2,N,K)=FLT1*S2NAB(J,K)+FLT2*S1(2,N,K)
      T1(2,N,K)=FLT1*T2NAB(J,K)+FLT2*T1(2,N,K)
    ENDDO
  ENDDO
#NAB_CASE # MED_EXCLUDE_END GIB_EXCLUDE_END IBE_EXCLUDE_END NPB_EXCLUDE_END

!#ifdef _DEBUG_
         !print *, "SZONE u2",maxval(u2),minval(u2),maxloc(u2),minloc(u2),sum(u2)
         !print *, maxval(v2),minval(v2),maxloc(v2),minloc(v2),sum(v2)
         !print *, maxval(xP),minval(xP),maxloc(xP),minloc(xP),sum(xP)
         !print *, maxval(s2),minval(s2),maxloc(s2),minloc(s2),sum(s2)
         !print *, maxval(t2),minval(t2),maxloc(t2),minloc(t2),sum(t2)
         !print *, maxval(u),minval(u),maxloc(u),minloc(u),sum(u)
!#endif
END SUBROUTINE DZONE2SZONE_COUPLE_IN 


SUBROUTINE DZONE2SZONE_COUPLE_OUT
USE GRID_VAR_DZONE,ONLY : U,U2,V2,S2,T2,XP
#NPB_CASE # MED_EXCLUDE_STA GIB_EXCLUDE_STA IBE_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY : I1,I2,I3,K1
USE OCN_PARA_DZONE,ONLY : J0
  DO K=1,K1
    JJ=1
! J0-2 should be evenly divisible by 2 to match up with SZONE overlap
    DO J=2,J0-2,2
      JJ=JJ+1
! pg coupling to SZONE grid
! 127th row (latitude) of cells in SZONE grid spans rows 2,3 of DZONE grid.
! factors of two cancel out!
      XPWSZONE(JJ-1,K)=XP(I3,J-1,K)+XP(I3,J,K)
      UWSZONE(JJ,K)=0.
! SZONE ghost zones
      U1SZONE(JJ,K)=0.
      V1SZONE(JJ,K)=0.
      S1SZONE(JJ,K)=0.
      T1SZONE(JJ,K)=0.
! SZONE first interior (I=2) zones
      U2SZONE(JJ,K)=0.
      V2SZONE(JJ,K)=0.
      S2SZONE(JJ,K)=0.
      T2SZONE(JJ,K)=0.

! The 127th row of cells in SZONE grid spans rows 2,3 of DZONE grid.
!! The southern ghost zones are included in this count.
!! Thus, UWSZONE(2,K) is the sum of rows 2 and 3 of eastern DZONE cells.

! We use uniformly-weighted rather than DY-weighted averages, because
! the latitudinal variation of DY between adjacent zones is very small

      DO N=1,2
! SZONE western inflow velocity:
! UWSZONE(JJ,K) goes to the westernmost grid line of the SZONE domain.
! U(I0-1,J,K) is the easternmost grid line of the DZONE domain.
        UWSZONE(JJ,K)=UWSZONE(JJ,K)+U(I3,J+N-1,K)
      ENDDO
      UWSZONE(JJ,K)=.5*UWSZONE(JJ,K)
      DO N=1,2
      DO I=I2,I1
! SZONE western ghost zones:
! SZONE ghost zones (west of westernmost SZONE domain grid line) come from
! first zones to west of U(I0-3,J,K)
        U1SZONE(JJ,K)=U1SZONE(JJ,K)+U2(I-2,J+N-1,K)
        V1SZONE(JJ,K)=V1SZONE(JJ,K)+V2(I-2,J+N-1,K)
        S1SZONE(JJ,K)=S1SZONE(JJ,K)+S2(I-2,J+N-1,K)
        T1SZONE(JJ,K)=T1SZONE(JJ,K)+T2(I-2,J+N-1,K)
! SZONE westernmost interior zones: these cover exactly 4 zones of DZONE grid
        U2SZONE(JJ,K)=U2SZONE(JJ,K)+U2(I,J+N-1,K)
        V2SZONE(JJ,K)=V2SZONE(JJ,K)+V2(I,J+N-1,K)
        S2SZONE(JJ,K)=S2SZONE(JJ,K)+S2(I,J+N-1,K)
        T2SZONE(JJ,K)=T2SZONE(JJ,K)+T2(I,J+N-1,K)
      ENDDO
      ENDDO
      U1SZONE(JJ,K)=.25*U1SZONE(JJ,K)
      V1SZONE(JJ,K)=.25*V1SZONE(JJ,K)
      S1SZONE(JJ,K)=.25*S1SZONE(JJ,K)
      T1SZONE(JJ,K)=.25*T1SZONE(JJ,K)
      U2SZONE(JJ,K)=.25*U2SZONE(JJ,K)
      V2SZONE(JJ,K)=.25*V2SZONE(JJ,K)
      S2SZONE(JJ,K)=.25*S2SZONE(JJ,K)
      T2SZONE(JJ,K)=.25*T2SZONE(JJ,K)
    ENDDO
  ENDDO
#NPB_CASE # MED_EXCLUDE_END GIB_EXCLUDE_END IBE_EXCLUDE_END NAB_EXCLUDE_END
#MED_CASE # NPB_EXCLUDE_STA GIB_EXCLUDE_STA IBE_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY : I1,I2,I3,I4,K1
USE OCN_PARA_DZONE,ONLY : J0
INTEGER::JMED
! ==========================
! Interface data to MED grid
! ==========================
  DO K=1,K1
! 51st row (latitude) of cells in MED grid spans rows 59,60,61 of GIB grid.
      JMED=50
    DO J=59,J0-3,3
      JMED=JMED+1
! for pg coupling to MED grid, factors of 3 cancel out
      XPWMED(JMED-1,K)=XP(I4,J-1,K)+XP(I4,J,K)+XP(I4,J+1,K)
      UWMED(JMED,K)=TMP*(U(I4,J,K)+U(I4,J+1,K)+U(I4,J+2,K))
      U1MED(JMED,K)=TMP*(U2(I4,J,K)+U2(I4,J+1,K)+U2(I4,J+2,K))
      V1MED(JMED,K)=TMP*(V2(I4,J,K)+V2(I4,J+1,K)+V2(I4,J+2,K))
      S1MED(JMED,K)=TMP*(S2(I4,J,K)+S2(I4,J+1,K)+S2(I4,J+2,K))
      T1MED(JMED,K)=TMP*(T2(I4,J,K)+T2(I4,J+1,K)+T2(I4,J+2,K))
      U2MED(JMED,K)=TEMP*(&
       U2(I1,J,K)+U2(I1,J+1,K)+U2(I1,J+2,K)&
      +U2(I2,J,K)+U2(I2,J+1,K)+U2(I2,J+2,K)&
      +U2(I3,J,K)+U2(I3,J+1,K)+U2(I3,J+2,K))
      V2MED(JMED,K)=TEMP*(&
       V2(I1,J,K)+V2(I1,J+1,K)+V2(I1,J+2,K)&
      +V2(I2,J,K)+V2(I2,J+1,K)+V2(I2,J+2,K)&
      +V2(I3,J,K)+V2(I3,J+1,K)+V2(I3,J+2,K))
      S2MED(JMED,K)=TEMP*(&
       S2(I1,J,K)+S2(I1,J+1,K)+S2(I1,J+2,K)&
      +S2(I2,J,K)+S2(I2,J+1,K)+S2(I2,J+2,K)&
      +S2(I3,J,K)+S2(I3,J+1,K)+S2(I3,J+2,K))
      T2MED(JMED,K)=TEMP*(&
       T2(I1,J,K)+T2(I1,J+1,K)+T2(I1,J+2,K)&
      +T2(I2,J,K)+T2(I2,J+1,K)+T2(I2,J+2,K)&
      +T2(I3,J,K)+T2(I3,J+1,K)+T2(I3,J+2,K))
    ENDDO
  ENDDO
#MED_CASE # NPB_EXCLUDE_END GIB_EXCLUDE_END IBE_EXCLUDE_END NAB_EXCLUDE_END
#GIB_CASE # MED_EXCLUDE_STA NPB_EXCLUDE_STA IBE_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY : I2,K1
USE OCN_PARA_SZONE,ONLY : J0
INTEGER::JIBE
  TMP=1./3.
  DO K=1,K1
! pg coupling to GIB grid
! GIB lat resolution is 3 times that of IBE
! cell centers J=2,3,4 in GIB correspond to J=282 in IBE
    DO J=1,J0-2
      XPWGIB(J,K)=TMP*XP(I2,281+(J-1)/3,K)
    ENDDO
! UWGIB is the longitudinal velocity on the westernmost grid line of the
! GIB grid. Here we specify it from the easternmost interior longitudinal
! velocity in the IBE grid.
! J0J is GIB model latitudinal resolution, which is 3 times that of IBE
! latitudinal resolution
    DO J=2,J0-1
      JIBE=282+(J-2)/3
      UWGIB(J,K)=U(I2,JIBE,K)
      U1GIB(J,K)=U2(I2,JIBE,K)
      V1GIB(J,K)=V2(I2,JIBE,K)
      S1GIB(J,K)=S2(I2,JIBE,K)
      T1GIB(J,K)=T2(I2,JIBE,K)
    ENDDO
  ENDDO
#GIB_CASE # MED_EXCLUDE_END NPB_EXCLUDE_END IBE_EXCLUDE_END NAB_EXCLUDE_END
#IBE_CASE # MED_EXCLUDE_STA NPB_EXCLUDE_STA GIB_EXCLUDE_STA NAB_EXCLUDE_STA
USE OCN_PARA_SZONE,ONLY : J0
USE OCN_PARA_DZONE,ONLY : I2,K1
  DO K=1,K1
! =============================================
! MULTI-GRID COUPLING:
! Pass interface data from NAB grid to IBE grid
! =============================================
! pg coupling
    DO J=1,J0-3,2
      XPWIBE(J,K)=.5*XP(I2,(J+1)/2,K)
      XPWIBE(J+1,K)=XPWIBE(J,K)
    ENDDO
! UWESTI is the longitudinal velocity on the westernmost grid line of the
! IBE grid. Here we specify it from the easternmost interior longitudinal
! velocity in the NAB grid.
! J0I is IBE model latitudinal resolution, which is twice the NAB
! latitudinal resolution
    DO J=2,J0-1
      JJ=J/2+1
      UWIBE(J,K)=U(I2,JJ,K)
      U1IBE(J,K)=U2(I2,JJ,K)
      V1IBE(J,K)=V2(I2,JJ,K)
      S1IBE(J,K)=S2(I2,JJ,K)
      T1IBE(J,K)=T2(I2,JJ,K)
    ENDDO
  ENDDO
#IBE_CASE # MED_EXCLUDE_END GIB_EXCLUDE_END NPB_EXCLUDE_END NAB_EXCLUDE_END
#NAB_CASE # MED_EXCLUDE_STA NPB_EXCLUDE_STA IBE_EXCLUDE_STA GIB_EXCLUDE_STA
USE OCN_PARA_DZONE,ONLY : I1,I2,I3,K1
USE OCN_PARA_DZONE,ONLY : J0
! Update fine grid boundary data
! Result is time filtered in NAB model to avoid high frequency forcing
  DO K=1,K1
    JJ=1
! J0-2 should be evenly divisible by 2 to match up with NAB overlap
    DO J=2,J0-2,2
      JJ=JJ+1
! pg coupling to NAB grid
! 40th row (latitude) of cells in NAB grid spans rows 2,3 of GOM grid.
! factors of two cancel out!
      XPWNAB(JJ-1,K)=XP(I3,J-1,K)+XP(I3,J,K)
      UWNAB(JJ,K)=0.
! NAB ghost zones
      U1NAB(JJ,K)=0.
      V1NAB(JJ,K)=0.
      S1NAB(JJ,K)=0.
      T1NAB(JJ,K)=0.
! NAB first interior (I=2) zones
      U2NAB(JJ,K)=0.
      V2NAB(JJ,K)=0.
      S2NAB(JJ,K)=0.
      T2NAB(JJ,K)=0.
! The 40th row of cells in NAB grid spans rows 2,3 of GOM grid.
! The southern ghost zones are included in this count.
! Thus, UWNAB(2,K) is the sum of rows 2 and 3 of eastern GOM cells.
! We use uniformly-weighted rather than DY-weighted averages, because
! the latitudinal variation of DY between adjacent zones is very small
      DO N=1,2
! NAB western inflow velocity:
! UWNAB(JJ,K) goes to the westernmost grid line of the NAB domain.
! U(I0-1,J,K) is the easternmost grid line of the GOM domain.
        UWNAB(JJ,K)=UWNAB(JJ,K)+U(I3,J+N-1,K)
      ENDDO
      UWNAB(JJ,K)=.5*UWNAB(JJ,K)
      DO N=1,2
        DO I=I2,I1
! NAB western ghost zones:
! NAB ghost zones (west of westernmost NAB domain grid line) come from
! first zones to west of U(I0-3,J,K)
          U1NAB(JJ,K)=U1NAB(JJ,K)+U2(I-2,J+N-1,K)
          V1NAB(JJ,K)=V1NAB(JJ,K)+V2(I-2,J+N-1,K)
          S1NAB(JJ,K)=S1NAB(JJ,K)+S2(I-2,J+N-1,K)
          T1NAB(JJ,K)=T1NAB(JJ,K)+T2(I-2,J+N-1,K)
! NAB westernmost interior zones: these cover exactly 4 zones of GOM grid
          U2NAB(JJ,K)=U2NAB(JJ,K)+U2(I,J+N-1,K)
          V2NAB(JJ,K)=V2NAB(JJ,K)+V2(I,J+N-1,K)
          S2NAB(JJ,K)=S2NAB(JJ,K)+S2(I,J+N-1,K)
          T2NAB(JJ,K)=T2NAB(JJ,K)+T2(I,J+N-1,K)
        ENDDO
      ENDDO
      U1NAB(JJ,K)=.25*U1NAB(JJ,K)
      V1NAB(JJ,K)=.25*V1NAB(JJ,K)
      S1NAB(JJ,K)=.25*S1NAB(JJ,K)
      T1NAB(JJ,K)=.25*T1NAB(JJ,K)
      U2NAB(JJ,K)=.25*U2NAB(JJ,K)
      V2NAB(JJ,K)=.25*V2NAB(JJ,K)
      S2NAB(JJ,K)=.25*S2NAB(JJ,K)
      T2NAB(JJ,K)=.25*T2NAB(JJ,K)
    ENDDO
  ENDDO
#NAB_CASE # MED_EXCLUDE_END GIB_EXCLUDE_END IBE_EXCLUDE_END NPB_EXCLUDE_END
!#ifdef _DEBUG_
!print *, "U1SZONE", maxval(U1SZONE),minval(U1SZONE),maxloc(U1SZONE),minloc(U1SZONE),sum(U1SZONE)
!print *, maxval(V1SZONE),minval(V1SZONE),maxloc(V1SZONE),minloc(V1SZONE),sum(V1SZONE)
!print *, maxval(S1SZONE),minval(S1SZONE),maxloc(S1SZONE),minloc(S1SZONE),sum(S1SZONE)
!print *, maxval(T1SZONE),minval(T1SZONE),maxloc(T1SZONE),minloc(T1SZONE),sum(T1SZONE)
!print *, maxval(U2SZONE),minval(U2SZONE),maxloc(U2SZONE),minloc(U2SZONE),sum(U2SZONE)
!print *, maxval(V2SZONE),minval(V2SZONE),maxloc(V2SZONE),minloc(V2SZONE),sum(V2SZONE)
!print *, maxval(S2SZONE(2:,:)),minval(S2SZONE(2:,:)),maxloc(S2SZONE(2:,:)),minloc(S2SZONE(2:,:)),sum(S2SZONE(2:,:))
!print *, maxval(T2SZONE),minval(T2SZONE),maxloc(T2SZONE),minloc(T2SZONE),sum(T2SZONE)
!print *, maxval(XPWSZONE),minval(XPWSZONE),maxloc(XPWSZONE),minloc(XPWSZONE),sum(XPWSZONE)
!pause
!#endif

END SUBROUTINE DZONE2SZONE_COUPLE_OUT

SUBROUTINE ALLOCATE_STATUS(istat)
INTEGER :: istat
if (istat/=0) STOP "Cannot allocate memory"
END SUBROUTINE ALLOCATE_STATUS

END MODULE DZONE2SZONE
