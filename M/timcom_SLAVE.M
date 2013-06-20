SUBROUTINE TIMCOM_SLAVE 
USE TIMCOM_GENERAL 
! Controls calculation, performs diagnostics, and saves graphics data.
! Graphics and computer animation is done by postprocessors

! *** User-defined resolution parameters ***
USE OCN_PARA_SLAVE
! Horizontal resolution parameters are I0,I1,I2,I3, J0,J1,J2,J3
! Vertical resolution parameters are K0,K1,K2
! NB0,NB1 are dimension parameters for SEVP elliptic solver
 
! Double precision SEVP elliptic solver arrays
USE WINDID_SLAVE
USE XYFS_SLAVE
USE ZCO_SLAVE
USE GRID_VAR_SLAVE, ONLY: RINV,RINV1,DUM0,DUM1,DUM2,XX,H,X,S,AL,AB,AC,AR,AT,SRC,CL,CB,CC,CR,CT,IE,VAR_ALLOCATE_SEVP
!use B_SLAVE
USE GRID_VAR_SLAVE, ONLY: RHO,Mcrho,McTP,McS,McTPTP,McSTP,P0,PX,PY,U1,U2,V1,V2,S1,S2,T1,T2,P,ULF,VLF,SLF,TLF,EV,HV,F,TANPHI,SUMIN,VAR_ALLOCATE_B
!use CGRID_SLAVE
USE GRID_VAR_SLAVE, ONLY: U,V,W,VAR_ALLOCATE_CGRID
!use ZFS_SLAVE
USE GRID_VAR_SLAVE, ONLY: Z,ODZ,ODZW,VAR_ALLOCATE_ZFS
!use TAVG_SLAVE_MAIN
USE GRID_VAR_SLAVE, ONLY: PBAR,PVAR,XBAR,UCLI,VCLI,RMSV,SBAR,TBAR,VAR_ALLOCATE_TAVG_MAIN
!use GLO2NAB_MZONE_MAIN
USE GRID_VAR_SLAVE, ONLY: VGLO,UAVG,VAVG,SAVG,TAVG,VAR_ALLOCATE_GLO2NAB_MAIN
!use METRIC_SLAVE
USE GRID_VAR_SLAVE, ONLY: A,XDEG,YV,YVDEG,YDEG,CS,OCS,DX,ODX,DY,ODY,CSV,OCSV,DXV,ODXV,DYV,ODYV,VAR_ALLOCATE_METRIC
!use BATHY_SLAVE
USE GRID_VAR_SLAVE, ONLY: KB,IU0,IV0,IN,IU,IV,IW,VAR_ALLOCATE_BATHY
USE CLIMAT_SLAVE
USE GRID_VAR_SLAVE, ONLY: TNUDGE,QAVG,WAVG,SCLI,TCLI,QDAVG,SDAVG,SSURF,TSURF,SSSP,SNSP,TSSP,TNSP,VAR_ALLOCATE_CLIMAT
USE CONTROL_SLAVE
USE SCA_SLAVE
!use SCRATCH_SLAVE_MAIN
USE GRID_VAR_SLAVE, ONLY: SCR,VAR_ALLOCATE_SCRATCH_MAIN
!@USE GRID_VAR_SLAVE, ONLY: UL,VL,WL,VAR_ALLOCATE_PMOVE
!USE GRAPHIC_SLAVE
USE INIT_VAR_SLAVE
USE INPUT_SLAVE
USE REALWINDS_SLAVE
USE PMOVE_SLAVE
USE DIR_SLAVE
USE OUTPUT_SLAVE
USE RESTART_SLAVE 

USE GRID_VAR_SLAVE, ONLY: U2,V2,S2,P,T2
use date

! **********************************************************************
  INTEGER :: ITN

  ITN=0

! ------------------------------
! MAIN TIME INTEGRATION LOOP 100
! ------------------------------
SLAVE: DO WHILE(.TRUE.)

! ===============================
! failure signal receive and exit
! ===============================
         IF ( STOPSIG == TIMCOM_FAILURE ) THEN
            CALL PMOVE(TIMCOM_FINAL)
            EXIT
         ENDIF

         ITN=ITN+1
         ITF=ITF+1
         DAYS=ITF/DAODT
         N=int(DAYS/360.+.001)
         TMP=DAYS/360.
         ! Luc:	commented the following	3 lines, we now	use real days in years
         ! During year, N360 goes from 0 to 360
         ! N360=int((TMP-N)*360.+.001)
         ! NYR=N+1
         NEXP=8
         DTS=DT/NEXP
         modelmjd=t0mjd+days
         call gregd2(modelmjd,year,month,day,hour,minut)
         N360=mod(int(modelmjd-t0mjd),365) ! days from the start of simulation
         NYR=int(modelmjd-t0mjd)/365

         IF ( (ITF_GENERAL .LT. TOPTS) .OR. (MOD(ITF_GENERAL,TOPTS) .EQ. 0) .OR. (ITFTO .LE. NITFTO) ) THEN
           WRITE(*,'(A,i10,A,f10.3,A,f10.3)')"SLAVE timestep :: ", itf,", ",DAYS," days has been run -- mjd=",modelmjd
           !WRITE(*,'(A,i10)')"SLAVE timestep :: ", itf
#ifdef _DEBUG_
           print *, "Before FS_SLAVE:"
           print *, "SLAVE u2", maxval(u2),minval(u2),maxloc(u2),minloc(u2),sum(u2)
           print *, "SLAVE v2", maxval(v2),minval(v2),maxloc(v2),minloc(v2),sum(v2)
           print *, "SLAVE P ", maxval(P),minval(P),maxloc(P),minloc(P),sum(P)
           print *, "SLAVE s2", maxval(s2),minval(s2),maxloc(s2),minloc(s2),sum(s2)
           print *, "SLAVE t2", maxval(t2),minval(t2),maxloc(t2),minloc(t2),sum(t2)
           print *, "SLAVE u ", maxval(u),minval(u),maxloc(u),minloc(u),sum(u)
#endif
         ENDIF

! ==================================================
! FS marches one time step each time it is called
! ==================================================
         CALL FS_SLAVE
#ifdef _DEBUG_
         IF ( (ITF_GENERAL .LT. TOPTS) .OR. (MOD(ITF_GENERAL,TOPTS) .EQ. 0) .OR. (ITFTO .LE. NITFTO) ) THEN
           print *, "After FS_SLAVE:"
           print *, "SLAVE u2", maxval(u2),minval(u2),maxloc(u2),minloc(u2),sum(u2)
           print *, "SLAVE v2", maxval(v2),minval(v2),maxloc(v2),minloc(v2),sum(v2)
           print *, "SLAVE p", maxval(P),minval(P),maxloc(P),minloc(P),sum(P)
           print *, "SLAVE s2", maxval(s2),minval(s2),maxloc(s2),minloc(s2),sum(s2)
           print *, "SLAVE t2", maxval(t2),minval(t2),maxloc(t2),minloc(t2),sum(t2)
         ENDIF
#endif

! =========
! realwinds
! =========
         CALL REALWIND(TIMCOM_RUN)

! =======
! tracers
! =======
         CALL PMOVE(TIMCOM_RUN)

! ======================
! diagnostic for blow up
! ======================
         IF ( DIAG_BLOWUP() == .true. ) THEN
           WRITE(*,*)"TIMCOM_SLAVE: DIAG_BLOWUP occured!"
           WRITE(*,*)"I must be stop it!"
           CALL PMOVE(TIMCOM_FINAL)
           STOPSIG=TIMCOM_FAILURE
           EXIT
         ENDIF

! ========
! average
! ========
         CALL AVERAGE

! ===========
! diagnostics
! ===========
! special
!           CALL DIAG_CROSS

! ============================================
! OUTPUT DATA WHILE REACH THE OUTPUT CONDITION
! ============================================
         IF ( STOPSIG == TIMCOM_CONTINUE ) THEN
           CALL SNAPSHOT_OUTPUT(TIMCOM_COND)
         ENDIF

! ===================================================
! check computation complete of this general timestep
! ===================================================
         IF (ITN.LT.MXITN) THEN
           CYCLE
         ELSE
           EXIT
         ENDIF

ENDDO SLAVE 

! ============================================
! OUTPUT DATA WHILE REACH THE OUTPUT CONDITION
! ============================================
  IF ( STOPSIG == TIMCOM_FINISH ) THEN
    CALL SNAPSHOT_OUTPUT(TIMCOM_FORCE)
  ENDIF

! ============================================
! restart output & checkpoint then stop signal
! ============================================
  IF ( STOPSIG == TIMCOM_CONTINUE ) THEN
    IF ( CHKPTSIG == 1 ) THEN
      CALL WRITE_RESTART(TIMCOM_FORCE)
    ENDIF
  ELSEIF ( STOPSIG == TIMCOM_STOP ) THEN
    CALL PMOVE(TIMCOM_FINAL)
    CALL WRITE_RESTART(TIMCOM_FORCE)
  ENDIF

! =============
! finish signal
! =============
  IF ( STOPSIG == TIMCOM_FINISH ) THEN ! finish job normally!
    CALL PMOVE(TIMCOM_FINAL)
    CALL WRITE_RESTART(TIMCOM_FORCE)
  ENDIF

CONTAINS

INCLUDE './diagnostic.f90'
INCLUDE './average.f90'
INCLUDE './realwind.f90'
INCLUDE './pmove.f90'
INCLUDE './output.f90'
INCLUDE './write_restart.f90'

END SUBROUTINE TIMCOM_SLAVE 
