! ----------------------------------------------------------------------
! SEVP elliptic solver arrays
! SEVP subregion boundary array "IE" is a user-defined array
! Double precision SEVP elliptic solver arrays
!
! ******************
! *ELLIPTIC SOLVER *
! ******************
! ----------------------------------------------------------------------
SUBROUTINE REPBIR(AX,AY,BB,CX,CY,RINV,RINV1,DUM0,DUM1,DUM2,F,H,X,IE,I0,I2,M0,M2,NBLK)
! ----------------------------------------------------------------------
  INTEGER, INTENT (IN) :: I0,I2,M0,M2,NBLK
  REAL(8) :: RINV(M2,M2,*),RINV1(M2,M2,*),DUM0(M2,*),DUM1(*),DUM2(*),H(M0,*),X(I0,*)
  REAL :: AX(I2,*),AY(I2,*),BB(I2,*),CX(I2,*),CY(I2,*),F(M2,*)
  INTEGER :: IE(*)

  JS=1
  DO NB=1,NBLK
    JF=IE(NB)-2
    DO J=JS,JF
    DO I=1,M2
      X(I+1,J+2)=(F(I,J)-AX(I,J)*X(I,J+1)-AY(I,J)*X(I+1,J)-BB(I,J)*X(I+1,J+1)-CX(I,J)*X(I+2,J+1))/CY(I,J)
    ENDDO
    ENDDO
    IF (NB.EQ.NBLK) GO TO 150
    J=IE(NB)-1
    DO I=1,M2
      DUM1(I)=F(I,J)-AX(I,J)*X(I,J+1)-AY(I,J)*X(I+1,J)-BB(I,J)*X(I+1,J+1)-CX(I,J)*X(I+2,J+1)-CY(I,J)*X(I+1,J+2)
    ENDDO
    J=IE(NB)
    DO N=1,M2
      DUM2(N)=0.
      DO M=1,M2
        DUM2(N)=DUM2(N)+DUM1(M)*RINV1(M,N,NB)
      ENDDO
      DUM0(N,NB)=X(N+1,J)
      X(N+1,J)=X(N+1,J)-DUM2(N)
    ENDDO
150 JS=IE(NB)
  ENDDO
  DO NBS=1,NBLK
    NB=NBLK-NBS+1
    JS=1
    IF (NB.NE.1) JS=IE(NB-1)
    JF=IE(NB)-2
    IF (NB.EQ.NBLK) GO TO 201
    J=IE(NB)
    DO N=1,M2
      X(N+1,J)=DUM0(N,NB)
    ENDDO
201 N=IE(NB)
    DO J=JS,N
    DO I=1,M0
      H(I,J)=0.
    ENDDO
    ENDDO
    J=IE(NB)-1
    DO I=1,M2
      DUM1(I)=F(I,J)-AX(I,J)*X(I,J+1)-AY(I,J)*X(I+1,J)-BB(I,J)*X(I+1,J+1)-CX(I,J)*X(I+2,J+1)-CY(I,J)*X(I+1,J+2)
    ENDDO
    DO N=1,M2
      DUM2(N)=0.
      DO M=1,M2
        DUM2(N)=DUM2(N)+DUM1(M)*RINV(M,N,NB)
      ENDDO
      H(N+1,JS+1)=DUM2(N)
      X(N+1,JS+1)=X(N+1,JS+1)+DUM2(N)
    ENDDO
    IF (NB.EQ.1) GO TO 250
    DO M=1,M2
      DUM1(M)=H(M+1,JS+1)*CY(M,JS-1)
    ENDDO
    J=IE(NB-1)
    DO N=1,M2
      DUM2(N)=0.
      DO M=1,M2
        DUM2(N)=DUM2(N)+DUM1(M)*RINV1(M,N,NB-1)
      ENDDO
      H(N+1,J)=DUM2(N)
    ENDDO
250 DO J=JS,JF
    DO I=1,M2
      H(I+1,J+2)=(-AX(I,J)*H(I,J+1)-AY(I,J)*H(I+1,J)-BB(I,J)*H(I+1,J+1)-CX(I,J)*H(I+2,J+1))/CY(I,J)
      X(I+1,J+2)=X(I+1,J+2)+H(I+1,J+2)
    ENDDO
    ENDDO
  ENDDO
END SUBROUTINE REPBIR


! *******************
! * ELLIPTIC SOLVER *
! *******************
! ----------------------------------------------------------------------
SUBROUTINE REP(AX,AY,BB,CX,CY,RINV,RINV1,DUM0,DUM1,DUM2,F,H,X,IE, I0,I2,NBLK)
! ----------------------------------------------------------------------
! NOTES
! REP is the SEVP Poisson solver for rigid pressure against rigid lid
! (see "Elliptic Marching Methods and Domain Decomposition" book by
! Patrick J. Roache).

! The Poisson approach is equivalent to solving implicitly free surface
! gravity wave terms in the limit as time step goes to infinity, and is
! also equivalent to assuming that the divergence of the barotropic mode
! is zero (as does the rigid lid approximation for incompressible flow).

! The big influence coefficient arrays, RINV and RINV1, are calculated
! in the PREPXXX/prepxxx.f codes for the various grids (XXX). These
! depend only on the grid resolutions and bathymetry data calculated in
! PREPXXX/DATA/inmets.f. The SEVP solver efficiency and roundoff error
! in the solution depend on the choice of NB0 and the IE vector. The
! NB0 value and the spacing of the IE values should ideally be large
! enough to allow reasonable efficiency while small enough for tolerable
! roundoff error. It is recommended that the IE spacings be about 5-8 for
! 64-bit word length (REAL*8) of the RINV, RINV1 and other arrays
! associated with their derivation and usage. Longitudinal multi-grid
! (domain decomposition) used herein for all the field operations
! reduces the size of RINV and RINV1, while being nearly seamless and
! accurate. A truly 4th-order-accurate domain decomposition is being
! developed. To further reduce storage, the SEVP solver may itself be
! decomposed within any of the grids, but that is not implemented here.

  REAL*8 RINV,RINV1,DUM0,DUM1,DUM2,X,H
  DIMENSION AX(I2,*),AY(I2,*),BB(I2,*),CX(I2,*),CY(I2,*), RINV(I2,I2,*),RINV1(I2,I2,*),H(I0,*),IE(*),DUM0(I2,*),DUM1(*), DUM2(*),F(I2,*),X(I0,*)

  JS=1
  DO NB=1,NBLK
    JF=IE(NB)-2
    DO J=JS,JF
    DO I=1,I2
      X(I+1,J+2)=(F(I,J)-AX(I,J)*X(I,J+1)-AY(I,J)*X(I+1,J)-BB(I,J)* X(I+1,J+1)-CX(I,J)*X(I+2,J+1))/CY(I,J)
    ENDDO
    ENDDO
    IF (NB.EQ.NBLK) GO TO 150
    J=IE(NB)-1
    DO I=1,I2
      DUM1(I)=F(I,J)-AX(I,J)*X(I,J+1)-AY(I,J)*X(I+1,J)-BB(I,J)* X(I+1,J+1)-CX(I,J)*X(I+2,J+1)-CY(I,J)*X(I+1,J+2)
    ENDDO
    J=IE(NB)
    DO N=1,I2
      DUM2(N)=0.
      DO M=1,I2
        DUM2(N)=DUM2(N)+DUM1(M)*RINV1(M,N,NB)
      ENDDO
      DUM0(N,NB)=X(N+1,J)
      X(N+1,J)=X(N+1,J)-DUM2(N)
    ENDDO
150 JS=IE(NB)
  ENDDO
  
  DO NBS=1,NBLK
    NB=NBLK-NBS+1
    JS=1
    IF (NB.NE.1) JS=IE(NB-1)
    JF=IE(NB)-2
    IF (NB.EQ.NBLK) GO TO 201
    J=IE(NB)
    DO N=1,I2
      X(N+1,J)=DUM0(N,NB)
    ENDDO
201 N=IE(NB)
    DO J=JS,N
    DO I=1,I0
      H(I,J)=0.
    ENDDO
    ENDDO
    J=IE(NB)-1
    DO I=1,I2
      DUM1(I)=F(I,J)-AX(I,J)*X(I,J+1)-AY(I,J)*X(I+1,J)-BB(I,J)* X(I+1,J+1)-CX(I,J)*X(I+2,J+1)-CY(I,J)*X(I+1,J+2)
    ENDDO
    DO N=1,I2
      DUM2(N)=0.
      DO M=1,I2
        DUM2(N)=DUM2(N)+DUM1(M)*RINV(M,N,NB)
      ENDDO
      H(N+1,JS+1)=DUM2(N)
      X(N+1,JS+1)=X(N+1,JS+1)+DUM2(N)
    ENDDO
    IF (NB.EQ.1) GO TO 250
    DO M=1,I2
      DUM1(M)=H(M+1,JS+1)*CY(M,JS-1)
    ENDDO
    J=IE(NB-1)
    DO N=1,I2
      DUM2(N)=0.
      DO M=1,I2
        DUM2(N)=DUM2(N)+DUM1(M)*RINV1(M,N,NB-1)
      ENDDO
      H(N+1,J)=DUM2(N)
    ENDDO
250 DO J=JS,JF
      DO I=1,I2
        H(I+1,J+2)=(-AX(I,J)*H(I,J+1)-AY(I,J)*H(I+1,J)-BB(I,J)* H(I+1,J+1)-CX(I,J)*H(I+2,J+1))/CY(I,J)
        X(I+1,J+2)=X(I+1,J+2)+H(I+1,J+2)
      END DO
    ENDDO
  ENDDO
END
