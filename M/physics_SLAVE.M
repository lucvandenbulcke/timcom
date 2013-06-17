MODULE PHYSICS_SLAVE
USE OCN_PARA_SLAVE
!use B_SLAVE
USE GRID_VAR_SLAVE, ONLY: RHO,Mcrho,McTP,McS,McTPTP,McSTP,P0,PX,PY,U1,U2,V1,V2,S1,S2,T1,T2,P,ULF,VLF,SLF,TLF,EV,HV,F,TANPHI,SUMIN
!use CGRID_SLAVE
USE GRID_VAR_SLAVE, ONLY: U,V,W
!use ZFS_SLAVE
USE GRID_VAR_SLAVE, ONLY: Z,ODZ,ODZW
!use BATHY_SLAVE
USE GRID_VAR_SLAVE, ONLY: KB,IU0,IV0,IN,IU,IV,IW
use CONTROL_SLAVE
use SCA_SLAVE
!use WINDMX_SLAVE_MAIN
USE GRID_VAR_SLAVE, ONLY: VBK,HBK,INFX,INFY
USE GRID_VAR_SLAVE, ONLY: ADD

INTEGER,PARAMETER :: TURB_PP82=0,TURB_P81=1

CONTAINS

SUBROUTINE PP82
! ----------------------------------------------------------------------
!calculate gradient Ri based vertical mixing coefficents
! so called "eddy viscosity and diffusivity"
! as per Pacanowski and Philander(1982)
      
! In loop 750 below, EV,HV units are cm-cm/s
! but EV,HV normalization by DZ is done 
      DO 750 K=1,K2
      L=K+1
      HBK0=HBK(K)
      TMPW=ORZMX/ODZW(L)
      DO 750 J=2,J1
      DO 750 I=2,I1
      !DO 750 I=2,I1
! TEMP must have units of cm-cm/s
      TEMP=TMPW*ABS(W(I,J,L))
! rho is double precision
!     RI=MAX(-0.5D0,980.*(RHO(I,J,L)-RHO(I,J,K))*ODZW(L)/(ODZW(L)**2*
      RI=MAX(-0.9D0,980.*(RHO(I,J,L)-RHO(I,J,K))*ODZW(L)/(ODZW(L)**2*&
      (0.001D0+(U2(I,J,L)-U2(I,J,K))**2+(V2(I,J,L)-V2(I,J,K))**2)))
      TMP=1./(1.+RI)
      TEMP=TMP*TEMP
! we add ODZW factor & apply explicit stability limit
      EVISC=MIN(20.*TMP**2,100.)
      HV(I-1,J-1,K)=(EVISC*TMP+HBK0+TEMP)*ODZW(L)
! ADD includes VBK0
 750  EV(I-1,J-1,K)=(EVISC+TEMP+ADD(I-1,J-1,K))*ODZW(L)
 
END SUBROUTINE PP82

END MODULE PHYSICS_SLAVE
