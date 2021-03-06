
! fortran unit of error messages

#define stderr 6

! fortran unit of screen output

#define stdout 6

! initfile global pattern matching (c function gmatch in library -lgmatch -lgen )

#define GMATCH


#define ALLOCATE_LOCAL_VARS

#define HAS_GETPID

! the exit subroutine is not conform with the Fortran 95 standard
! but it is very usefull for shell scripts

#define ERROR_STOP call exit(1)

! if strict Fortran 95 standard is required use, ERROR_STOP should be
! defined as

!#define ERROR_STOP stop


! uncomment this line for MISP compiler
! For g95, pgf90 and ifort flush can be called without a status variable

#define flush(unit,stat) FLUSH(unit)

  ! uncomment this line for parallel execution
!#define ASSIM_PARALLEL

#ifndef ASSIM_PARALLEL
#define procnum 1
#else
#define MPI
#endif

#define NETCDF
