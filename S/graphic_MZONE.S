MODULE GRAPHIC_MZONE
! GRAPHICS PACKAGE
USE OCN_PARA_MZONE
!use SCRATCH_MAIN
USE GRID_VAR_MZONE, ONLY: SCR
INTEGER,PARAMETER :: FN_XYDATA0=28,FN_XYDEEP=29,FN_XYDATA=30,FN_XZDATA=31,FN_YZDATA=32
! 2-d cross-section files for graphics
! XY PLOTS: XY/DATA (LOGICAL UNIT 30)
! XZ PLOTS: XZ/DATA (LOGICAL UNIT 31)
! YZ PLOTS: YZ/DATA (LOGICAL UNIT 32)

CONTAINS

!-----------------------------------------------------------------------
SUBROUTINE OPENPLOTS
!-----------------------------------------------------------------------
USE INPUT 
CHARACTER(160)::FILENAME
  FILENAME=TRIM(WORKDIR)//'/OUTPUT/XY/GLO/DATA_0'
  IF (LRSTRT.EQ.0)&
    OPEN(FN_XYDATA0,CONVERT='BIG_ENDIAN',file=FILENAME,form='unformatted')

  FILENAME=TRIM(WORKDIR)//'/OUTPUT/XY/GLO/DEEP'
    OPEN(FN_XYDEEP,CONVERT='BIG_ENDIAN',file=FILENAME,form='unformatted')

  FILENAME=TRIM(WORKDIR)//'/OUTPUT/XY/GLO/DATA'
    OPEN(FN_XYDATA,CONVERT='BIG_ENDIAN',file=FILENAME,form='unformatted')

  FILENAME=TRIM(WORKDIR)//'/OUTPUT/XZ/GLO/DATA'
    OPEN(FN_XZDATA,CONVERT='BIG_ENDIAN',file=FILENAME,form='unformatted')

  FILENAME=TRIM(WORKDIR)//'/OUTPUT/YZ/GLO/DATA'
    OPEN(FN_YZDATA,CONVERT='BIG_ENDIAN',file=FILENAME,form='unformatted')

  REWIND FN_XYDEEP 
  REWIND FN_XYDATA 
  REWIND FN_XZDATA 
  REWIND FN_YZDATA 

END SUBROUTINE OPENPLOTS


SUBROUTINE WRITE_PLOTS_HEAD
USE INPUT
  INTEGER :: N
  IF (LRSTRT.EQ.0) THEN
      WRITE(FN_XYDATA0) CASE_NAME,DSCRIB,LRSTRT,MXIT,I0,J0,K0,IN,ODX,ODY,ODZ,Z
      DAYS=0.
  ENDIF
  WRITE(FN_XYDEEP) CASE_NAME,DSCRIB,LRSTRT,MXIT,I0,J0,K0,IN,ODX,ODY,ODZ,Z
  WRITE(FN_XYDATA) CASE_NAME,DSCRIB,LRSTRT,MXIT,I0,J0,K0,IN,ODX,ODY,ODZ,Z
  WRITE(FN_XZDATA) CASE_NAME,DSCRIB,LRSTRT,MXIT,I0,J0,K0,ODX,ODY
  WRITE(FN_YZDATA) CASE_NAME,DSCRIB,LRSTRT,MXIT,I0,J0,K0,ODX,ODY
! Special writes of data that needs to be inverted vertically
! Needed because of IDIOTIC limitation of new fortran i/o protocal
  DO N=1,K1
    WRITE(FN_XZDATA) ODZ(N)
    WRITE(FN_XZDATA) ((IN(I,J,N),I=1,I0),J=1,J0)
    WRITE(FN_YZDATA) ODZ(N)
    WRITE(FN_YZDATA) ((IN(I,J,N),I=1,I0),J=1,J0)
  ENDDO
  DO N=1,K0+K1
    WRITE(FN_XZDATA) Z(N)
    WRITE(FN_YZDATA) Z(N)
  ENDDO
END SUBROUTINE WRITE_PLOTS_HEAD

SUBROUTINE FLUSH_PLOTS
USE INPUT
  IF (LRSTRT.EQ.0) CALL FLUSH(FN_XYDATA0)
  CALL FLUSH(FN_XYDEEP)
  CALL FLUSH(FN_XYDATA)
  CALL FLUSH(FN_XZDATA)
  CALL FLUSH(FN_YZDATA)
END SUBROUTINE FLUSH_PLOTS

! ----------------------------------------------------------------------
SUBROUTINE PLOTSGLO(MXIT,NFLG)
! ----------------------------------------------------------------------
!use BGLO
USE GRID_VAR_MZONE, ONLY: RHO,Mcrho,McTP,McS,McTPTP,McSTP,P0,PX,PY,U1,U2,V1,V2,S1,S2,T1,T2,P,ULF,VLF,SLF,TLF,EV,HV,F,TANPHI,SUMIN,VAR_ALLOCATE_BGLO
!use CGRIDGLO
USE GRID_VAR_MZONE, ONLY: U,V,W,VAR_ALLOCATE_CGRIDGLO
!use ZFSGLO
USE GRID_VAR_MZONE, ONLY: Z,ODZ,ODZW,VAR_ALLOCATE_ZFSGLO
!use TAVGGLO_MAIN
USE GRID_VAR_MZONE, ONLY: PBAR,PVAR,XBAR,UCLI,VCLI,RMSV,SBAR,TBAR
!use METRICGLO
USE GRID_VAR_MZONE, ONLY: A,XDEG,YV,YVDEG,YDEG,CS,OCS,DX,ODX,DY,ODY,CSV,OCSV,DXV,ODXV,DYV,ODYV,VAR_ALLOCATE_METRICGLO
!use BATHYGLO
USE GRID_VAR_MZONE, ONLY: KB,IU0,IV0,IN,IU,IV,IW,VAR_ALLOCATE_BATHYGLO
use SCA_MZONE
!use IJKPLOT_MAIN
USE GRID_VAR_MZONE, ONLY: KP,VAR_ALLOCATE_IJKPLOT_MAIN
! Show selected vertical cross-sections
!     DATA ILOC/37,100,150,205,256,286,316,346/
!     DATA JLOC/20,48,76,103,114,150,163,185,312,376/
! Show selected horizontal cross-sections
! 20 layers
!     DATA KP/1,2*0,1,4*0,1,0,1,0,1,0,1,5*0/
! 30 layers
!     DATA KP/1,2*0,1,3*0,1,3*0,1,3*0,1,3*0,1,10*0/

! ---------
! X-Y PLOTS
! ---------

CALL VAR_ALLOCATE_IJKPLOT_MAIN

  KSTOP=K1
  IF (NFLG.NE.0.AND.ITF.NE.MXIT) KSTOP=1
  INC=1
  IF (ITF.EQ.MXIT) INC=2
  DO K=1,KSTOP,INC
    IF (.NOT.(ITF.NE.MXIT.AND.KP(K).NE.1)) THEN 
      DO J=2,J1
      DO I=2,I1
      ! scale up density
        SCR(I,J,2)=1000.*RHO(I,J,K)
      ! convert to equivalent free surface anomaly
        SCR(I,J,1)=P(I,J,K)/980.
      ENDDO
      ENDDO
      CALL XYGLO('P','P (cm head) and V (mm/sec)    ',SCR,K,K)
      CALL XYGLO('u','P (cm head) and V (mm/sec)    ',U2(1,1,K),K,K)
      CALL XYGLO('v','P (cm head) and V (mm/sec)    ',V2(1,1,K),K,K)
      IF ( .NOT. (Z(2*K+1).GT.900.E2) ) THEN
        CALL XYGLO('R','RHO-(RHOmin @ present z level)',SCR(1,1,2),K,K)
        CALL XYGLO('S','salinity (parts per thousand) ',S2(1,1,K),K,K)
        CALL XYGLO('T','temperature (degrees c)       ',T2(1,1,K),K,K)
        CYCLE
      ELSE 
        CALL XYGLO('y','(RHO-RHOmin @ present z level)',SCR(1,1,2),K,K)
        CALL XYGLO('y','salinity (parts per thousand) ',S2(1,1,K),K,K)
        CALL XYGLO('y','temperature (degrees c)       ',T2(1,1,K),K,K)
        IF (K.NE.1) CYCLE
      ENDIF
    ELSE
      IF (K.NE.1) CYCLE
    ENDIF   
  ! always plot layer 4 vertical velocity
    N=4
    TMP=.5*24.*36.
  ! mask out lateral boundary layer regions for interior w plot
    DO J=2,J1
    DO I=2,I1
      SCR(I,J,1)=TMP*(W(I,J,N)+W(I,J,N+1))
      IF ( .NOT. (I.NE.2.AND.J.NE.2.AND.I.NE.I1.AND.J.NE.J1) ) THEN
        TEMP=IN(I-1,J-1,N)*IN(I,J-1,N)*IN(I+1,J-1,N)&
            *IN(I-1,  J,N)*IN(I,  J,N)*IN(I+1,  J,N)&
            *IN(I-1,J+1,N)*IN(I,J+1,N)*IN(I+1,J+1,N)
      ELSE
        TEMP=IN(I-2,J-2,N)*IN(I-1,J-2,N)*IN(I,J-2,N)*IN(I+1,J-2,N)&
            *IN(I+2,J-2,N)*IN(I-2,J-1,N)*IN(I-1,J-1,N)*IN(I,J-1,N)&
            *IN(I+1,J-1,N)*IN(I+2,J-1,N)*IN(I-2,J,N)*IN(I-1,J,N)&
            *IN(I,J,N)*IN(I+1,J,N)*IN(I+2,J,N)*IN(I-2,J+1,N)*IN(I-1,J+1,N)&
            *IN(I,J+1,N)*IN(I+1,J+1,N)*IN(I+2,J+1,N)*IN(I-2,J+2,N)&
            *IN(I-1,J+2,N)*IN(I,J+2,N)*IN(I+1,J+2,N)*IN(I+2,J+2,N)
      ENDIF
      SCR(I,J,2)=TEMP*SCR(I,J,1)
    ENDDO
    ENDDO
    CALL XYGLO('w','vertical velocity (meters/day)',SCR,N,N)
    CALL XYGLO('W','interior vertical vel. (m/day)',SCR(1,1,2),N,N)
  ENDDO

! ---------
! X-Z PLOTS
! ---------
  DO N=1,8
    !J=JLOC(N)
    J=J2/16+J2*(N-1)/8
    !J=10+25*(N-1)
    TMP=.5*24.*36.
    TEMP=7.*24.*3600.
    DO K=1,K1
    DO I=2,I1
      SCR(I,3,K)=1000.*RHO(I,J,K)
      SCR(I,4,K)=TMP*(W(I,J,K)+W(I,J,K+1))
      SCR(I,5,K)=TEMP*(&
      (V(I,J-1,K)-V(I-1,J-1,K)+V(I+1,J-1,K)-V(I,J-1,K))*ODXV(J-1)+ &
      (V(I+1,J,K)-V(I,J,K)+V2(I,J,K)-V2(I-1,J,K))*ODXV(J)- &
      (U(I-1,J,K)-U(I-1,J-1,K)+U(I,J,K)-U(I,J-1,K))*ODYV(J-1)- &
      (U(I,J+1,K)-U(I,J,K)+U2(I-1,J+1,K)-U2(I-1,J,K))*ODYV(J))
    ENDDO
    ENDDO
    CALL XZGLO('V','latitudinal velocity (cm/sec) ',V2(1,J,1),J,J)
    CALL XZGLO('R','(RHO-RHOmin) (kg/m3)          ',SCR(1,3,1),J,J)
    CALL XZGLO('S','salinity (parts per thousand) ',S2(1,J,1),J,J)
    CALL XZGLO('T','temperature (degrees C)       ',T2(1,J,1),J,J)
    CALL XZGLO('w','vertical velocity (meters/day)',SCR(1,4,1),J,J)
    CALL XZGLO('s','vorticity (radians per week)  ',SCR(1,5,1),J,J)
  ENDDO

! ---------
! Y-Z PLOTS
! ---------
  DO N=1,8
    !I=ILOC(N)
    I=I2/16+I2*(N-1)/8
    DO K=1,K1
    DO J=2,J1
    ! scale up density
      SCR(3,J,K)=1000.*RHO(I,J,K)
      SCR(2,J,K)=TMP*(W(I,J,K)+W(I,J,K+1))
    ENDDO
    ENDDO
    CALL YZGLO('U','longitudinal vel. (cm/sec)    ',U2(I,1,1),I,I)
    CALL YZGLO('R','(RHO-RHOmin) (kg/m3)          ',SCR(3,1,1),I,I)
    CALL YZGLO('S','salinity (parts per thousand) ',S2(I,1,1),I,I)
    CALL YZGLO('T','temperature (degrees c)       ',T2(I,1,1),I,I)
    CALL YZGLO('w','vertical velocity (meters/day)',SCR(2,1,1),I,I)
  ENDDO

  IF (ITF.NE.MXIT) RETURN

! ----------------------------
! ZONALLY AVERAGED DIAGNOSTICS
! ----------------------------
! SCR(1,J,K)= zonal mean latitudinal velocity
! SCR(2,J,K)= zonal mean vertical velocity
! SCR(3,J,K)= meridional streamfunction
! Zero meridional scratch arrays
  DO K=1,K0
  DO J=1,J1
  DO I=1,4
    SCR(I,J,K)=0.
  ENDDO
  ENDDO
  ENDDO
  DO K=1,K1
  DO J=2,J2
    TMP=0.
    N=0
    DO I=2,I1
      N=N+IV(I,J,K)
      TMP=TMP+IV(I,J,K)*V(I,J,K)
    ENDDO
    SCR(1,J,K)=0.
  ! zonal mean latitudinal velocity
    IF (N.NE.0) SCR(1,J,K)=TMP/N
  ! meridional streamfunction in Sverdrups at "natural" grid points
  ! (grid line intersects)
  ! increment streamfunction by total open area times mean lat. velocity
  ! total open area= N*DXV(J)/ODZ(K)
    SCR(3,J,K+1)=SCR(3,J,K)+1.E-12*(N*DXV(J)/ODZ(K))*SCR(1,J,K)
  ENDDO
  ENDDO

! mean vertical velocity in meters/week
  DO K=2,K1
  DO J=2,J1
    TMP=0.
    N=0
    DO I=2,I1
      N=N+IW(I,J,K)
      TMP=TMP+IW(I,J,K)*W(I,J,K)
    ENDDO
    SCR(2,J,K)=0.
    IF (N.NE.0) SCR(2,J,K)=TMP/N
  ENDDO
  ENDDO

! interpolate streamfunction to cell centers
  DO K=1,K1
  DO J=2,J1
    SCR(4,J,K)=.25*(SCR(3,J-1,K)+SCR(3,J,K)+SCR(3,J-1,K+1)+SCR(3,J,K+1))
  ENDDO
  ENDDO

! the last two arguments need to change for non-flat-bottom cases
  CALL YZGLO('X','meridional streamfunction (Sv)',SCR(4,1,1),1,1)

! interpolate mean latitudinal velocity to cell centers
  DO K=1,K1
  DO J=2,J1
    SCR(4,J,K)=.5*(SCR(1,J-1,K)+SCR(1,J,K))
  ENDDO
  ENDDO

  CALL YZGLO('V','zonal-mean lat. vel. (cm/sec) ',SCR(4,1,1),1,1)

! interpolate mean vertical velocity to cell centers
  TMP=7*12*36
  DO K=1,K1
  DO J=2,J1
    SCR(4,J,K)=TMP*(SCR(2,J,K)+SCR(2,J,K+1))
  ENDDO
  ENDDO

  CALL YZGLO('W','zonal-mean vert. vel. (m/week)',SCR(4,1,1),1,1)

! mean RHO, S, and T
  DO K=1,K1
  DO J=2,J1
    SCR(2,J,K)=0.
    SCR(3,J,K)=0.
    SCR(4,J,K)=0.
    N=0
    DO I=2,I1
      N=N+IN(I,J,K)
      SCR(2,J,K)=SCR(2,J,K)+IN(I,J,K)*1000.*RHO(I,J,K)
      SCR(3,J,K)=SCR(3,J,K)+IN(I,J,K)*S2(I,J,K)
      SCR(4,J,K)=SCR(4,J,K)+IN(I,J,K)*T2(I,J,K)
    ENDDO
    IF (.NOT.(N.EQ.0)) THEN
      SCR(2,J,K)=SCR(2,J,K)/N
      SCR(3,J,K)=SCR(3,J,K)/N
      SCR(4,J,K)=SCR(4,J,K)/N
    ENDIF
  ENDDO
  ENDDO

  CALL YZGLO('R','zonal mean (RHO-RHOmin) kg/m3 ',SCR(2,1,1),1,1)
  CALL YZGLO('S','zonal-mean salinity (ppt)     ',SCR(3,1,1),1,1)
  CALL YZGLO('T','zonal-mean temperature (deg.C)',SCR(4,1,1),1,1)
END SUBROUTINE PLOTSGLO

! ----------------------------------------------------------------------
SUBROUTINE COMXYGLO(FLD,DAYS,IN,FN)
! save movie data
! ----------------------------------------------------------------------
INTEGER(2) IN,NFLD
INTEGER FN
DIMENSION IN(I0,*),FLD(I0,*),NFLD(I0,J0)

  CALL RANGER(FLD,IN,I0,2,2,I1,J1,ILO,JLO,IHI,JHI,FMIN,FMAX)
  !WRITE(*,5) ILO,IHI,JLO,JHI,FMIN,FMAX
!5    FORMAT('ILO,IHI,JLO,JHI,FMIN,FMAX',4I4,2(1X,1PE8.1))
  FRNG=FMAX-FMIN
  RMIN=FMIN+1.E-5*FRNG
  RMAX=FMAX-1.E-5*FRNG
  IF ((RMAX-RMIN).EQ.0.) RETURN
  RF=9999./FRNG
  !NFLD=9999.*(FLD-RMIN)/FRNG
  DO J=2,J1
  DO I=2,I1
    NFLD(I,J)=MIN(9999.,RF*(FLD(I,J)-RMIN))
  ENDDO
  ENDDO
  WRITE(FN)DAYS,ILO,JLO,IHI,JHI,RMIN,RMAX,((NFLD(I,J),I=2,I1),J=2,J1)
END SUBROUTINE COMXYGLO

! ----------------------------------------------------------------------
SUBROUTINE XYGLO(FN,FNAME,FLD,KL,KH)
! ----------------------------------------------------------------------
!use BATHYGLO
USE GRID_VAR_MZONE, ONLY: KB,IU0,IV0,IN,IU,IV,IW,VAR_ALLOCATE_BATHYGLO
use SCA_MZONE
INTEGER(2)::NFLD
CHARACTER FN*1,FNAME*30
DIMENSION FLD(I0,J0,*),NFLD(I0,J0)

 DO K=KL,KH
   KK=K-KL+1
   CALL RANGER(FLD(1,1,KK),IN(1,1,K),I0,2,2,I1,J1,ILO,JLO,IHI,JHI,FMIN,FMAX)
   ! WRITE(*,5) FN,K,FMIN,ILO,JLO,FMAX,IHI,JHI
   ! IF (ITF.LT.100) WRITE(14,5) FN,K,FMIN,ILO,JLO,FMAX,IHI,JHI
   FRNG=FMAX-FMIN
   RMIN=FMIN+1.E-5*FRNG
   RMAX=FMAX-1.E-5*FRNG
   IF ((RMAX-RMIN).EQ.0.) RETURN
   RF=9999./FRNG
   ! NFLD=9999.*(FLD-RMIN)/FRNG
   DO J=2,J1
   DO I=2,I1
     NFLD(I,J)=MIN(9999.,RF*(FLD(I,J,KK)-RMIN))
   ENDDO
   ENDDO
   IF (DAYS.EQ.0.) THEN 
     WRITE(FN_XYDATA0) FN,FNAME,DAYS,K,ILO,JLO,IHI,JHI,RMIN,RMAX,&
               ((NFLD(I,J),I=2,I1),J=2,J1)
   ELSE
     IF (K.EQ.1.OR.(K.EQ.4.AND.(FN.EQ.'W'.OR.FN.EQ.'w')))&
        WRITE(FN_XYDATA) FN,FNAME,DAYS,K,ILO,JLO,IHI,JHI,RMIN,RMAX,&
                         ((NFLD(I,J),I=2,I1),J=2,J1)
     IF (K.NE.1.OR.FN.EQ.'X'.OR.FN.EQ.'x')&
        WRITE(FN_XYDEEP) FN,FNAME,DAYS,K,ILO,JLO,IHI,JHI,RMIN,RMAX,&
                         ((NFLD(I,J),I=2,I1),J=2,J1)
   ENDIF
 ENDDO
 5   FORMAT(A1,' AT LEVEL ',I2,': MIN=',1PE9.2,' AT (',I3,',',I3,&
       '); MAX=',1PE9.2,' AT (',I3,',',I3,')')
END SUBROUTINE XYGLO

! ----------------------------------------------------------------------
SUBROUTINE XZGLO(FN,FNAME,FLD,JL,JH)
! ----------------------------------------------------------------------
!use BATHYGLO
USE GRID_VAR_MZONE, ONLY: KB,IU0,IV0,IN,IU,IV,IW,VAR_ALLOCATE_BATHYGLO
use SCA_MZONE
INTEGER(2) INXZ,NFLD
CHARACTER FN*1,FNAME*30
DIMENSION FLD(I0,J0,*),NFLD(I0,K1),XZ(I0,K1),INXZ(I0,K1)

  DO J=JL,JH
    JJ=J-JL+1
    DO K=1,K1
    DO I=2,I1
      XZ(I,K)=FLD(I,JJ,K)
      INXZ(I,K)=IN(I,J,K)
    ENDDO
    ENDDO
    CALL RANGER(XZ,INXZ,I0,2,1,I1,K1,ILO,KLO,IHI,KHI,FMIN,FMAX)
    ! IF (ITF.LT.100) WRITE(14,5) FN,J,FMIN,FMAX
    FRNG=FMAX-FMIN
    RMIN=FMIN+1.E-5*FRNG
    RMAX=FMAX-1.E-5*FRNG
    IF ((RMAX-RMIN).EQ.0.) RETURN
    RF=9999./FRNG
    ! NFLD=9999.*(FLD-RMIN)/FRNG
    DO K=1,K1
    DO I=2,I1
      NFLD(I,K)=MIN(9999.,RF*(XZ(I,K)-RMIN)*INXZ(I,K))
    ENDDO
    ENDDO
    WRITE(FN_XZDATA) FN,FNAME,DAYS,J,ILO,KLO,IHI,KHI,RMIN,RMAX,((NFLD(I,K),I=2,I1),K=1,K1)
  ENDDO
 5   FORMAT(A1,': MIN,MAX AT LATITUDE ',I2,' = ',2(1X,1PE9.2))
END SUBROUTINE XZGLO

! ----------------------------------------------------------------------
SUBROUTINE YZGLO(FN,FNAME,FLD,IL,IH)
! ----------------------------------------------------------------------
!use BATHYGLO
USE GRID_VAR_MZONE, ONLY: KB,IU0,IV0,IN,IU,IV,IW,VAR_ALLOCATE_BATHYGLO
use SCA_MZONE
CHARACTER FN*1,FNAME*30
INTEGER(2) INYZ,NFLD
DIMENSION FLD(I0,J0,*),NFLD(J0,K1),YZ(J0,K1),INYZ(J0,K1)

  DO I=IL,IH
    II=I-IL+1
    DO K=1,K1
    DO J=2,J1
      YZ(J,K)=FLD(II,J,K)
      INYZ(J,K)=IN(I,J,K)
    ENDDO
    ENDDO
  ! For plotting zonal averages (indicated by I=1)
    IF (.NOT.(I.NE.1)) THEN
      DO K=1,K1
      DO J=2,J1
        N=0
        DO III=2,I1
          N=N+IN(III,J,K)
        ENDDO
      INYZ(J,K)=0
      INYZ(J,K)=MIN(1,N)
      ENDDO
      ENDDO
    ELSE
      CALL RANGER(YZ,INYZ,J0,2,1,J1,K1,JLO,KLO,JHI,KHI,FMIN,FMAX)
    ENDIF
    ! IF (ITF.LT.100) WRITE(14,21) FN,I,FMIN,FMAX
    FRNG=FMAX-FMIN
    RMIN=FMIN+1.E-5*FRNG
    RMAX=FMAX-1.E-5*FRNG
    IF ((RMAX-RMIN).EQ.0.) RETURN
    RF=9999./FRNG
    ! NFLD=9999.*(FLD-RMIN)/FRNG
    DO K=1,K1
    DO J=2,J1
      NFLD(J,K)=MIN(9999.,RF*(YZ(J,K)-RMIN)*INYZ(J,K))
    ENDDO
    ENDDO
    WRITE(FN_YZDATA) FN,FNAME,DAYS,I,JLO,KLO,JHI,KHI,RMIN,RMAX,((NFLD(J,K),J=2,J1),K=1,K1)
  ENDDO
 21   FORMAT(A1,': MIN,MAX AT LONGITUDE ',I3,' = ',2(1X,1PE9.2))
END SUBROUTINE YZGLO

END MODULE GRAPHIC_MZONE
