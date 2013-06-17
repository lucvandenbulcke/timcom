! Copyright(c) 2002-2009 Alexander Barth and Luc Vandenblucke

!
! Simple module to load binary data in GHER and NetCDF format
!
!
! Copyright 2005 Alexander Barth


! Simplified call diagram for reading routines
!

! Top level                                 uload
! generic interface                          /|\   *    
!                                          /  |  \       *    
!                                        /    |    \          *     
!                                      /      |      \             *
!                                    /        |        \                *
! routines               uload_Real1D   uload_Real2D    uload_Real3D    uload_degenerated
! choosen at                         \        |        /                *
! link-time                            \      |      /             *
! performs allocation                    \    |    /          *
!                                          \  |  /       *
!                                            \|/     *
! parse extraction                          uread
! e.g. (1:10,1:10)                            |
!                                             |
!                                             |
! call the appropriate                    uread_prim
! file format                                / \
!                                           /   \
!                                          /     \
!                                         /       \
! performs                 ureadc_all_kind        nf90_get_var
! even. kind                 GHER format          NETCDF format
! convertion





! Simplified call diagram for writing routines
!

! Top level                                 usave
! generic interface                          /|\   *     
!                                          /  |  \       *     
!                                        /    |    \          *    
!                                      /      |      \             *     
!                                    /        |        \                *     
! routines               usave_Real1D   usave_Real2D    usave_Real3D    usave_degenerated
! choosen at                         \        |        /                *
! link-time                            \      |      /             *
!                                        \    |    /          *
!                                          \  |  /       *
!                                            \|/   *     
! parse extraction                       usave_generic
! e.g. (1:10,1:10)                            |
!                                             |
!                                             |
! call the appropriate                    uwrite_prim
! file format                                / \
!                                           /   \
!                                          /     \
!                                         /       \
! performs                 uwritc_all_kind        nf_put_var_real and friends
! even. kind                 GHER format          NETCDF format
! convertion
! if file already exists
!

! include the fortran preprocessor definitions
#include "ppdef.h"

#define GZIP
!#define NETCDF
!#define DEBUG_SYSTEMCALLS
#define CHECK_SHAPE

module ufileformat

 ! default record length is 10 MB
 integer, parameter :: MaxRecordLength = 10*1024*1024 
 integer, parameter :: MaxDimensions = 10
 integer, parameter :: MaxFNameLength = 256
 real, parameter :: DefaultValex = -9999.

 interface uwritc
   module procedure  &
        uwritc_orig
 end interface

 interface uinquire
   module procedure  &
        uinquire_new, &
        uinquire_deprec
 end interface

 interface uload
   module procedure &
        uload_degenerated, &
        uload_Real0D, &
        uload_Real1D, &
        uload_Real2D, &
        uload_Real3D, &
        uload_Real4D, &
        uload_Integer0D, &
        uload_Integer1D, &
        uload_Integer2D, &
        uload_Integer3D, &
        uload_Double2D
 end interface


 interface usave
   module procedure  &
        usave_generic, &
        usave_generic_double, &
        usave_degenerated, &
        usave_Real0D, &
        usave_Real1D, &
        usave_Real2D, &
        usave_Real3D, &
        usave_Integer0D, &
        usave_Integer1D, &
        usave_Integer2D, &
        usave_Integer3D, &
        usave_Double2D
 end interface

 character(128) :: TmpdirDefault = '/tmp' 

contains

 integer function uniquenumber()
  implicit none
  integer ierror,ipid
  real :: r
#ifdef HAS_GETPID
  integer :: getpid
#endif

#if defined(HAS_GETPID)
  ipid = getpid()
#elif defined(HAS_PXFGETPID)
  call pxfgetpid(ipid, ierror)
#else
  ipid = 0
  call random_number(r)
  ipid = floor(10000000*r)
#endif
  uniquenumber = ipid

 end function uniquenumber


#ifdef GZIP
 subroutine gunzipToTemp(fname,tmpname)
  character(len=*), intent(in) :: fname
  character(len=*), intent(out) :: tmpname

  character(len=256) cmd,spid,tmpdir
  integer ierror,ipid
  logical :: exi

  inquire(file=fname,exist=exi)
  if (.not.exi) then
    write(stderr,*) 'gunzipToTemp: ERROR ',trim(fname), & 
         ' does not exist '
    ERROR_STOP
  end if

  call getenv('TMPDIR',tmpdir)
  if (len_trim(tmpdir).eq.0) tmpdir = TmpdirDefault

  ipid = uniquenumber()
  write(spid,*) ipid
  tmpname = trim(tmpdir) // '/ufileformat.' //trim(ADJUSTL(spid))
  cmd='cp '//trim(fname)//' '//trim(tmpname)//'.gz ; gunzip -f '//trim(tmpname)//'.gz;'

#ifdef DEBUG_SYSTEMCALLS
  write(stdout,*) 'system: ',cmd
#endif
  call system (cmd) 

 end subroutine gunzipToTemp
#endif


 subroutine gunzipFile(fname,tmpname)
  implicit none
  character(len=*), intent(in) :: fname
  character(len=*), intent(out) :: tmpname

  character(len=256) cmd,spid,tmpdir
  integer ierror,ipid,last,length
  logical :: exi

  tmpname=fname

#ifdef GZIP
  length=index(fname,'#')-1
  if (length.eq.-1) length=index(fname,'(')-1
  if (length.eq.-1) length=len_trim(fname)

  !  write(stdout,*) 'length, fname ',length, trim(fname)
  !  write(stdout,*) 'length ',length,fname(length-2:length)

  if (length.ge.3.and.fname(length-2:length).eq.'.gz') then

    inquire(file=fname(1:length),exist=exi)
    if (.not.exi) then
      write(stderr,*) 'gunzipFile: ERROR ',trim(fname), & 
           ' does not exist '
      ERROR_STOP
    end if

    call getenv('TMPDIR',tmpdir)
    if (len_trim(tmpdir).eq.0) tmpdir = TmpdirDefault

    ipid = uniquenumber()

    write(spid,*) ipid
    tmpname = trim(tmpdir)// '/ufileformat.'// trim(ADJUSTL(spid))
    cmd='cp '//trim(fname(1:length))//' '//trim(tmpname)//'.gz ; gunzip -f '//trim(tmpname)//'.gz;'

#ifdef DEBUG_SYSTEMCALLS
    write(stdout,*) 'system: ',cmd
#endif
    call system (cmd)
    tmpname=trim(tmpname)//trim(fname(length+1:))
  end if

#endif

 end subroutine gunzipFile


 subroutine rmgunzipFile(fname,tmpname)
  implicit none
  character(len=*), intent(in) :: fname
  character(len=*), intent(in) :: tmpname

  character(len=256) cmd,spid
  integer ierror,ipid,last,length

#ifdef GZIP
  if (fname.ne.tmpname) then
    length=index(tmpname,'#')-1
    if (length.eq.-1) length=index(tmpname,'(')-1
    if (length.eq.-1) length=len_trim(tmpname)

    !    write(stdout,*) 'length, fname ',length, trim(tmpname)
#ifdef DEBUG_SYSTEMCALLS
    write(stdout,*) 'system: rm '//tmpname(1:length)
#endif
    call system('rm '//tmpname(1:length))
  end if
#endif
 end subroutine rmgunzipFile

 !____________________________________________________________________
 !

 subroutine uinquire_prim(filename,valex,prec,ndim,vshape,isdegen)
#ifdef NETCDF
  use netcdf
#endif 
  implicit none
#ifdef NETCDF
  include 'netcdf.inc' 
#endif 

  character(len=*), intent(in) :: filename
  integer, intent(out) :: prec,ndim,vshape(*)
  real, intent(out) :: valex

  logical, intent(out) :: isdegen

  character(len=256) :: fname,unzipfname
  logical :: isNetcdf, isZipped
  integer ::  kb, nbmots,length,iu, stat, pos
  integer, parameter :: kblanc=10

  real (kind=4) :: valex4

#ifdef NETCDF
  integer :: i,ncid,rcode, ndims, nvars, natts, recdim, cid, &
       vtype, cdim(MaxDimensions), sizedim(MaxDimensions), &
       vtypeattr, ndimattr
  character(len=128) :: var,name,namedim(MaxDimensions)
#endif

  fname = filename
#ifdef NETCDF
  pos = index(filename,'#')
  isNetcdf = pos.ne.0
  if (isNetcdf) fname = filename(1:pos-1)
#endif

  unzipfname = fname
#ifdef GZIP
  length = len_trim(fname)
  isZipped = length.ge.3.and.fname(length-2:length).eq.'.gz'
  if (isZipped) call  gunzipToTemp(fname,unzipfname)
#endif

#ifdef NETCDF
  if (.not.isNetcdf) then
#endif

!$omp critical (reserveFreeUnit)
    iu = freeunit()
    open(iu,file=trim(unzipfname),iostat=stat,status='old',form='unformatted')
!$omp end critical (reserveFreeUnit)

    if (stat.ne.0) then
      write(stderr,*) 'uinquire: error while opening file "',trim(filename),'".'
      ERROR_STOP
    end if

    ! skip KBLANC lines

    do  kb=1,KBLANC
      read(iu,err=99,end=99)
    end do

    read(iu,err=99,end=99) vshape(1),vshape(2),vshape(3),prec,nbmots,valex4
    close(iu)

    valex = valex4

    isdegen = any(vshape(1:3).lt.0)
    vshape(1:3) = abs(vshape(1:3))

    if (vshape(3).eq.1) then
      if (vshape(2).eq.1) then
        ndim = 1
      else
        ndim = 2
      end if
    else 
      ndim = 3
    end if

#ifdef NETCDF

  else
    var = filename(pos+1:)

    ncid = ncopn (unzipfname,ncnowrit,rcode)
    cid  = ncvid (ncid,var,rcode)
    call ncvinq (ncid,cid,name,vtype,ndim,cdim,natts,rcode)

    valex = DefaultValex
    isdegen = .false.

    do i=1,natts
      CALL NCANAM (NCID,cid,i,name, RCODE)
      CALL NCAINQ(NCID,cid,name,vtypeattr,ndimattr,RCODE)

      !    write(stdout,*) 'name attribute: ',trim(name)

      if (name.eq.'shape') then
        isdegen =.true.
        ndim = ndimattr
        CALL NCAGT(NCID,cid, 'shape',vshape, RCODE)
      elseif (name.eq.'missing_value') then
        rcode = nf90_get_att(ncid, cid,'missing_value', valex)
      end if
    end do

    if (.not.isdegen) then
      do i=1,ndim
        call ncdinq (ncid,cdim(i),namedim(i),vshape(i),rcode)
        !      write(stdout,*) trim(namedim(i)),vshape(i),rcode
      end do
    end if

    prec = 4

    !  write(stdout,*) 'nc',trim(filename(1:pos-1)),trim(var),rcode
    !  write(stdout,*) name,vtype,ndim,cdim,natts,rcode

    !  call ncagt  (ncid,cid,'missing_value',valex,rcode)

    !  write(stdout,*) 'valex ',valex
    call ncclos (ncid,rcode)
  end if

  ! endif corresponing to #ifdef NETCDF
#endif 

#ifdef GZIP
  if (isZipped) then
#    ifdef DEBUG_SYSTEMCALLS
    write(stdout,*) 'system: rm '//trim(unzipfname)
#    endif
    call system('rm '//unzipfname)
  end if
#endif

  !write(stdout,*) 'vshape ',vshape(1:ndim)

  return

99 continue
  write(*,*) 'Data error in UINQUIRE, not a conform file: "',trim(filename),'".'
 end subroutine uinquire_prim

 !____________________________________________________________________
 !


 subroutine uinquire_new(filename,valex,prec,ndim,vshape,isdegen)
  implicit none
  character(len=*), intent(in) :: filename
  integer, intent(out), optional  :: prec, ndim, vshape(*)
  real, intent(out), optional  :: valex
  logical, intent(out), optional :: isdegen

  integer  :: precc,ndimc,vshapec(MaxDimensions)=1
  real  :: valexc
  logical :: isdegenc

  integer :: indp
  integer :: e(2,MaxDimensions)
  integer, pointer :: extraction(:,:)

  indp = index(filename,'(')

  if (indp.eq.0) then
    call uinquire_prim(filename,valexc,precc,ndimc,vshapec,isdegenc)
  else
    e = 1
    call uinquire_prim(filename(1:indp-1),valexc,precc,ndimc,e(2,:),isdegenc)
    call parseExtraction(filename,e,extraction)
    vshapec(1:ndimc) = extraction(2,:)-extraction(1,:)+1
    deallocate(extraction)
  end if


  if (present(ndim))     ndim = ndimc
  if (present(vshape))   vshape(1:ndimc) = vshapec(1:ndimc)
  if (present(prec))     prec = precc
  if (present(valex))    valex = valexc
  if (present(isdegen))  isdegen = isdegenc

  !write(stdout,*) 'imaxc,jmaxc,kmaxc ',imaxc,jmaxc,kmaxc

 end subroutine uinquire_new

 !____________________________________________________________________
 !

 subroutine uinquire_deprec(filename,valex,prec,imax,jmax,kmax,nbmots,isdegen)
  implicit none
  character(len=*), intent(in) :: filename
  integer, intent(out)  :: prec,imax,jmax,kmax,nbmots
  real, intent(out)  :: valex
  logical, intent(out) :: isdegen

  integer :: ndim,vshape(MaxDimensions)

  vshape = 1
  call uinquire_new(filename,valex,prec,ndim,vshape,isdegen)
  imax = vshape(1)
  jmax = vshape(2)
  kmax = vshape(3)
  nbmots=MaxRecordLength
  !  write(stdout,*) 'imax,jmax,kmax ',imax,jmax,kmax
 end subroutine uinquire_deprec

 !____________________________________________________________________
 !


 subroutine uread_prim(filename,c,valex,ndim,vshape,isdegen,extraction)
#ifdef NETCDF
  use netcdf
#endif 
  implicit none
#ifdef NETCDF
  include 'netcdf.inc' 
#endif 
  character(len=*), intent(in) :: filename
  real :: c(*)
  real, intent(out) :: valex
  integer, intent(out) :: ndim, vshape(*)
  logical, intent(out) :: isdegen
  integer, optional, intent(in) :: extraction(:,:)

  character(len=256) :: fname,unzipfname
  logical :: isNetcdf, isZipped
  integer ::  kb, nbmots,length,iu, stat, pos, iprec
  integer, parameter :: kblanc=10

#ifdef NETCDF
  integer :: i,ncid,rcode, ndims, nvars, natts, recdim, cid, &
       vtype, cdim(MaxDimensions), sizedim(MaxDimensions), &
       vtypeattr, ndimattr
  character(len=128) :: var,name,namedim(MaxDimensions)

  ! for conversion from double
  real(kind=8), pointer :: p8(:)
  real(kind=8) :: valex8

  ! for conversion from short
  integer(kind=2), pointer :: p2(:)
  integer(kind=2) :: valex2

  real :: scale_factor, add_offset
#endif

  fname = filename
#ifdef NETCDF
  pos = index(filename,'#')
  isNetcdf = pos.ne.0
  if (isNetcdf) fname = filename(1:pos-1)
#endif

  unzipfname = fname
#ifdef GZIP
  length = len_trim(fname)
  isZipped = length.ge.3.and.fname(length-2:length).eq.'.gz'
  if (isZipped) call  gunzipToTemp(fname,unzipfname)
#endif

#ifdef NETCDF
  if (.not.isNetcdf) then
#endif

!$omp critical (reserveFreeUnit)
    iu = freeunit()
    open(iu,file=unzipfname,iostat=stat,status='old',form='unformatted')
!$omp end critical (reserveFreeUnit)

    if (stat.ne.0) then
      write(stderr,*) 'uread_prim: Error while opening file "',trim(filename),'".'
      write(stderr,*) 'uread_prim: unzipfname "',trim(unzipfname),'". iostat: ',stat
      ERROR_STOP
    end if

    call ureadc_all_kind(iu,c,valex,iprec,vshape(1),vshape(2),vshape(3),nbmots,extraction)
    isdegen = any(vshape(1:3).lt.0)
    vshape(1:3) = abs(vshape(1:3))

    if (vshape(3).eq.1) then
      if (vshape(2).eq.1) then
        ndim = 1
      else
        ndim = 2
      end if
    else 
      ndim = 3
    end if


!$omp critical
    close(iu)
!$omp end critical

#ifdef NETCDF
  else

    var = filename(pos+1:)

    ncid = ncopn (unzipfname,ncnowrit,rcode)
    cid  = ncvid (ncid,var,rcode)
    call ncvinq (ncid,cid,name,vtype,ndim,cdim,natts,rcode)

    ! default value for attributes

    valex = DefaultValex
    isdegen = .false.
    scale_factor=1
    add_offset=0

    ! loop over all attributes

    do i=1,natts
      CALL NCANAM (NCID,cid,i,name, RCODE)
      CALL NCAINQ(NCID,cid,name,vtypeattr,ndimattr,RCODE)

      if (name.eq.'shape') then
        isdegen =.true.
        ndim = ndimattr
        CALL NCAGT(NCID,cid, 'shape',vshape, RCODE)
      elseif (name.eq.'missing_value') then
        rcode = nf90_get_att(ncid, cid,name, valex)
      elseif (name.eq.'scale_factor') then
        rcode = nf90_get_att(ncid, cid, name, scale_factor)
      elseif (name.eq.'add_offset') then
        rcode = nf90_get_att(ncid, cid, name, add_offset)
      end if
    end do

    if (.not.isdegen) then
      do i=1,ndim
        call ncdinq (ncid,cdim(i),namedim(i),vshape(i),rcode)
        !      write(stdout,*) trim(namedim(i)),vshape,rcode
      end do
    end if

    if (present(extraction)) then
      !     vshape = extraction(2,:)-extraction(1,:)+1
      vshape(1:ndim) = extraction(2,1:ndim)-extraction(1,1:ndim)+1
    end if

    iprec = 4

    !  write(stdout,*) 'imaxc,jmaxc,kmaxc ',imaxc,jmaxc,kmaxc
    !  write(stdout,*) 'nc',trim(filename(1:pos-1)),trim(var),rcode
    !  write(stdout,*) name,vtype,ndim,cdim(1:ndim),natts,rcode
    !  write(stdout,*) ndim, vshape(1:ndim),cid

    !    call ncagt  (ncid,cid,'missing_value',valex,rcode)

    !write(stdout,*) 'valex ',valex

    if (isdegen) then
      rcode = nf90_get_var(ncid, cid, c(1:ndim+1), (/ 1 /), (/ ndim+1 /))
      if (present(extraction)) then
        c(1) = c(1) + sum(c(2:ndim+1)*(extraction(1,1:ndim)-1))
      end if
    else
      if (.not.present(extraction)) then
        rcode = nf90_get_var(ncid, cid, c(1:product(vshape(1:ndim))), spread(1,1,ndim), vshape(1:ndim))
      else
        rcode = nf90_get_var(ncid, cid, c(1:product(vshape(1:ndim))), extraction(1,:), vshape(1:ndim))
      end if
    end if

    call ncclos (ncid,rcode)

    !write(stdout,*) 'c(1) ',c(1)
    !
    ! scale variable if necessary
    !

    if ((scale_factor.ne.1.or.add_offset.ne.0).and..not.isdegen) then
      i = product(vshape(1:ndim))
      where (c(1:i).ne.valex)
        c(1:i) = c(1:i) * scale_factor + add_offset
      end where
    end if
  end if
#endif

#ifdef GZIP
  if (isZipped) then
#ifdef DEBUG_SYSTEMCALLS
    write(stdout,*) 'system: rm '//trim(unzipfname)
#endif
    call system('rm '//unzipfname)
  end if
#endif

 end subroutine uread_prim


 !____________________________________________________________________
 !


 subroutine uread_prim_double(filename,c,valex,ndim,vshape,isdegen,extraction)
#ifdef NETCDF
  use netcdf
#endif
  implicit none
#ifdef NETCDF
  include 'netcdf.inc'
#endif
  character(len=*), intent(in) :: filename
  real(8) :: c(*)
  real, intent(out) :: valex
  integer, intent(out) :: ndim, vshape(*)
  logical, intent(out) :: isdegen
  integer, optional, intent(in) :: extraction(:,:)

  character(len=256) :: fname,unzipfname
  logical :: isNetcdf, isZipped
  integer ::  kb, nbmots,length,iu, stat, pos, iprec
  integer, parameter :: kblanc=10

#ifdef NETCDF
  integer :: i,ncid,rcode, ndims, nvars, natts, recdim, cid, &
       vtype, cdim(MaxDimensions), sizedim(MaxDimensions), &
       vtypeattr, ndimattr
  character(len=128) :: var,name,namedim(MaxDimensions)

  ! for conversion from double
  real(kind=8), pointer :: p8(:)
  real(kind=8) :: valex8

  ! for conversion from short
  integer(kind=2), pointer :: p2(:)
  integer(kind=2) :: valex2

  real :: scale_factor, add_offset
#endif

  fname = filename
#ifdef NETCDF
  pos = index(filename,'#')
  isNetcdf = pos.ne.0
  if (isNetcdf) fname = filename(1:pos-1)
#endif

  unzipfname = fname
#ifdef GZIP
  length = len_trim(fname)
  isZipped = length.ge.3.and.fname(length-2:length).eq.'.gz'
  if (isZipped) call  gunzipToTemp(fname,unzipfname)
#endif

#ifdef NETCDF
  if (.not.isNetcdf) then
#endif

      write(stderr,*) 'cannot read real*8 variable from gher file'
      write(stderr,*) 'actually I can but the code need small adaptations in uread_prim'

#ifdef NETCDF
  else

    var = filename(pos+1:)

    ncid = ncopn (unzipfname,ncnowrit,rcode)
    cid  = ncvid (ncid,var,rcode)
    call ncvinq (ncid,cid,name,vtype,ndim,cdim,natts,rcode)

    ! default value for attributes

    valex = DefaultValex
    isdegen = .false.
    scale_factor=1
    add_offset=0

    ! loop over all attributes

    do i=1,natts
      CALL NCANAM (NCID,cid,i,name, RCODE)
      CALL NCAINQ(NCID,cid,name,vtypeattr,ndimattr,RCODE)

      if (name.eq.'shape') then
        isdegen =.true.
        ndim = ndimattr
        CALL NCAGT(NCID,cid, 'shape',vshape, RCODE)
      elseif (name.eq.'missing_value') then
        rcode = nf90_get_att(ncid, cid,name, valex)
      elseif (name.eq.'scale_factor') then
        rcode = nf90_get_att(ncid, cid, name, scale_factor)
      elseif (name.eq.'add_offset') then
        rcode = nf90_get_att(ncid, cid, name, add_offset)
      end if
    end do

    if (.not.isdegen) then
      do i=1,ndim
        call ncdinq (ncid,cdim(i),namedim(i),vshape(i),rcode)
        !      write(stdout,*) trim(namedim(i)),vshape,rcode
      end do
    end if

    if (present(extraction)) then
      !     vshape = extraction(2,:)-extraction(1,:)+1
      vshape(1:ndim) = extraction(2,1:ndim)-extraction(1,1:ndim)+1
    end if

    if (isdegen) then
      rcode = nf90_get_var(ncid, cid, c(1:ndim+1), (/ 1 /), (/ ndim+1 /))
      if (present(extraction)) then
        c(1) = c(1) + sum(c(2:ndim+1)*(extraction(1,1:ndim)-1))
      end if
    else
      if (.not.present(extraction)) then
        rcode = nf90_get_var(ncid, cid, c(1:product(vshape(1:ndim))), spread(1,1,ndim), vshape(1:ndim))
      else
        rcode = nf90_get_var(ncid, cid, c(1:product(vshape(1:ndim))), extraction(1,:), vshape(1:ndim))
      end if
    end if

    call ncclos (ncid,rcode)

    !write(stdout,*) 'c(1) ',c(1)
    !
    ! scale variable if necessary
    !

    if ((scale_factor.ne.1.or.add_offset.ne.0).and..not.isdegen) then
      i = product(vshape(1:ndim))
      where (c(1:i).ne.valex)
        c(1:i) = c(1:i) * scale_factor + add_offset
      end where
    end if
  end if
#endif
#ifdef GZIP
  if (isZipped) then
#ifdef DEBUG_SYSTEMCALLS
    write(stdout,*) 'system: rm '//trim(unzipfname)
#endif
    call system('rm '//unzipfname)
  end if
#endif

 end subroutine uread_prim_double

 !____________________________________________________________________
 !





 !____________________________________________________________________
 !


 subroutine uread(filename,c,valex,ndim,vshape,isdegen)
  implicit none
  character(len=*), intent(in) :: filename
  real :: c(*)
  real,    optional, intent(out) :: valex
  integer, optional, intent(out) :: ndim, vshape(*)
  logical, optional, intent(out) :: isdegen

  integer :: indp,j,prec,nbmots
  integer :: e(2,MaxDimensions),lentot,ip,jp,kp,l,lp,d,ndimc,vshapec(MaxDimensions)
  real :: valexc
  integer, pointer :: extraction(:,:), ind(:), cummul(:)
  real, pointer :: tmp(:)
  logical :: isdegenc

  indp = index(filename,'(')
  vshapec = 1

  if (indp.eq.0) then
    call uread_prim(filename,c,valexc,ndimc,vshapec,isdegenc)
  else
    e = 1
    call uinquire(filename(1:indp-1),valexc,prec,ndimc,e(2,:),isdegenc)
    call parseExtraction(filename,e,extraction)
    call uread_prim(filename(1:indp-1),c,valexc,ndimc,vshapec,isdegenc,extraction)
    deallocate(extraction)
  end if

  !  write(stdout,*) 'ndimc ',ndimc

  if (present(ndim))    ndim = ndimc
  if (present(vshape))  vshape(1:ndimc) = vshapec(1:ndimc)
  if (present(isdegen)) isdegen = isdegenc
  if (present(valex)) valex = valexc

 end subroutine uread

 !____________________________________________________________________
 !


 subroutine uread_double(filename,c,valex,ndim,vshape,isdegen)
  implicit none
  character(len=*), intent(in) :: filename
  real(8) :: c(*)
  real,    optional, intent(out) :: valex
  integer, optional, intent(out) :: ndim, vshape(*)
  logical, optional, intent(out) :: isdegen

  integer :: indp,j,prec,nbmots
  integer :: e(2,MaxDimensions),lentot,ip,jp,kp,l,lp,d,ndimc,vshapec(MaxDimensions)
  real :: valexc
  integer, pointer :: extraction(:,:), ind(:), cummul(:)
  real, pointer :: tmp(:)
  logical :: isdegenc

  indp = index(filename,'(')
  vshapec = 1

  if (indp.eq.0) then
    call uread_prim_double(filename,c,valexc,ndimc,vshapec,isdegenc)
  else
    e = 1
    call uinquire(filename(1:indp-1),valexc,prec,ndimc,e(2,:),isdegenc)
    call parseExtraction(filename,e,extraction)
    call uread_prim_double(filename(1:indp-1),c,valexc,ndimc,vshapec,isdegenc,extraction)
    deallocate(extraction)
  end if

  !  write(stdout,*) 'ndimc ',ndimc

  if (present(ndim))    ndim = ndimc
  if (present(vshape))  vshape(1:ndimc) = vshapec(1:ndimc)
  if (present(isdegen)) isdegen = isdegenc
  if (present(valex)) valex = valexc

 end subroutine uread_double

 !____________________________________________________________________
 !

 subroutine makeSubset(tmp,vshapet,i,c,vshapec)
  implicit none
  real, intent(in) :: tmp(:)
  integer, intent(in) :: vshapet(:),i(:,:)
  real, intent(out) :: c(*)
  integer, intent(out) :: vshapec(:)

  integer :: ndimc,lentot,ip,jp,kp,l,lp,d
  integer, pointer :: cummul(:),ind(:)

  ndimc = size(vshapet)
  vshapec(1:ndimc) = i(2,:)-i(1,:)+1


  lentot = product(vshapec(1:ndimc))
  allocate(ind(ndimc),cummul(ndimc))
!!$
!!$ write(stdout,*) 'varshapet ',vshapet
!!$ write(stdout,*) 'varshapet ',size(tmp)
!!$ write(stdout,*) 'varshapec ',vshapec
!!$ write(stdout,*) 'varshapet2 ',c(lentot)

  cummul(1) = 1
  do d=1,ndimc-1
    cummul(d+1) = cummul(d)*vshapec(d)
  end do

  do l=0,lentot-1
    ! l,lp,ind  = zero-based indexes

    !      ip = l
    !      kp = ip/(imaxc*jmaxc); ip = ip-kp*imaxc*jmaxc
    !      jp = ip/imaxc;        ip = ip-jp*imaxc

    ! transform l -> ind

    ind(1) = l
    do d=ndimc,2,-1
      ind(d) = ind(1)/cummul(d)
      ind(1) = ind(1)-ind(d)*cummul(d)
    end do

    ! transform ind -> lp

    lp = i(1,ndimc)+ind(ndimc)-1
    do d=ndimc-1,1,-1
      lp = i(1,d)+ind(d)-1 + lp*vshapet(d) 
    end do

    !    write(stdout,*) 'l,ip,jp,kp ',l,ip,jp,kp, &
    !      i(1,1)+ip + (i(1,2)+jp-1 + (i(1,3)+kp-1)*e(2,2))*e(2,1)

    !      c(l+1) = tmp(i(1,1)+ip + (i(1,2)+jp-1 + (i(1,3)+kp-1)*e(2,2))*e(2,1))
    c(l+1) = tmp(lp+1)
  end do

  deallocate(ind,cummul)

 end subroutine makeSubset

 !____________________________________________________________________
 !


 subroutine ureadfull(filename,c,valex,ndim,vshape,isdegen)
  implicit none
  character(len=*), intent(in) :: filename
  real :: c(*)
  real,    optional, intent(out) :: valex
  integer, optional, intent(out) :: ndim, vshape(*)
  logical, optional, intent(out) :: isdegen

  integer :: indp,j,prec,nbmots
  integer :: e(2,MaxDimensions),lentot,ip,jp,kp,l,lp,d,ndimc,vshapec(MaxDimensions)
  real :: valexc
  integer, pointer :: i(:,:), ind(:), cummul(:)
  real, pointer :: tmp(:)
  logical :: isdegenc

  call uread(filename,c,valexc,ndimc,vshapec,isdegenc)
  if (isdegenc) call uexplo(c,ndimc,vshapec)

  if (present(ndim))    ndim = ndimc
  if (present(vshape))  vshape(1:ndimc) = vshapec(1:ndimc)
  if (present(isdegen)) isdegen = isdegenc
  if (present(valex)) valex = valexc

 end subroutine ureadfull




 !____________________________________________________________________
 !



 !____________________________________________________________________
 !


 Subroutine UREADC_orig(iu,c8,c4,valexr,iprecr,imaxr,jmaxr,kmaxr,nbmotr)
  !                ======
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! Reads the field C(I,J,K) from fortran unit iu 
  ! returns the field in the array c4 if the returned iprecr=4
  ! returns the field in the array c8 if the returned iprecr=8
  ! returns the values if imaxr,jmaxr,kmaxr found in the file
  !
  ! JMB 6/3/91 
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !
  IMPLICIT NONE
  INTEGER(4) :: kblanc,nbmots,kmaxc,iprec,nl,kl,kc,ir,ide,jmaxc,iprecr, &
       imaxr,iu,jmaxr,kb,imaxc,kmaxr,nbmotr
  real(4) :: c4(*),valexc,valexr
  real(8) :: c8(*)
  PARAMETER(KBLANC=10)
  ! in the calling routin you can specify the following equivalence to
  ! save memory space:
  !      equivalence(c,c4)
  !      equivalence(c,c8)
  !
  ! skip KBLANC lines
  do kb=1,KBLANC
    read(iu,ERR=99)
  end do
  !
  read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc
  !
  ! pass the values read to the calling routine
  iprecr=iprec
  imaxr=imaxc
  jmaxr=jmaxc
  kmaxr=kmaxc
  nbmotr=nbmots
  valexr=valexc
  !
  ! compute the number of full records to read and the remaining words
  nl=(imaxc*jmaxc*kmaxc)/nbmots
  ir=imaxc*jmaxc*kmaxc-nbmots*nl
  ide=0
  !
  ! if pathological case, read only four values C0 and DCI,DCJ,DCK
  ! and return
  ! them as the two four elements of the array
  if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
    nl=0
    ir=4
  endif
  !
  !
  ! single precision
  if(iprec.eq.4) then
    do kl=1,nl
      read(iu,ERR=99) (c4(ide+kc),kc=1,nbmots)
      ide=ide+nbmots
    end do
    read(iu,ERR=99) (c4(ide+kc),kc=1,ir)
  else
    !
    ! double precision
    if(iprec.eq.8) then
      do kl=1,nl
        read(iu,ERR=99) (c8(ide+kc),kc=1,nbmots)
        ide=ide+nbmots
      end do
      read(iu,ERR=99) (c8(ide+kc),kc=1,ir)
    else
      goto 99
    endif
  endif
  !
  return
99 continue
  write(*,*) 'Data error in UREADC, not a conform file'
  return

 end subroutine UREADC_orig

 !____________________________________________________________________
 !

 ! similar to ureadc_orig, but it accept single and double precision 
 ! argumets and optionally performs an extraction
 !
 !____________________________________________________________________
 !


 !____________________________________________________________________
 !

 subroutine ureadc_all_kind(iu,c,valex,iprecr,imaxr,jmaxr,kmaxr,nbmotr,extraction)
  implicit none

  integer, intent(in) :: iu
  real, intent(out) :: c(*), valex
  integer, intent(out) :: iprecr,imaxr,jmaxr,kmaxr,nbmotr
  integer, optional, intent(in) :: extraction(:,:)

  ! variables with well defined kind
  integer(4) ::  imaxc,jmaxc,kmaxc,iprec,nbmots
  real(4) :: valex4
  ! allocatable space for kind convertion
  real(4), allocatable :: p4(:)
  real(8), allocatable :: p8(:)

  ! local variables

  integer nl,kl,kc,ir,ide,kb
  real, allocatable :: tmp(:)
  integer :: shapec(3),extr(2,3)
  integer, parameter :: kblanc=10
  !
  ! skip KBLANC lines
  do kb=1,kblanc
    read(iu,ERR=99)
  end do
  !
  read(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valex4
  !
  ! pass the values read to the calling routine and perform necessary kind convertion
  iprecr=iprec
  imaxr=imaxc
  jmaxr=jmaxc
  kmaxr=kmaxc
  nbmotr=nbmots
  valex=valex4
  !
  ! compute the number of full records to read and the remaining words

  nl=(imaxc*jmaxc*kmaxc)/nbmots
  ir=imaxc*jmaxc*kmaxc-nbmots*nl
  ide=0

  !
  ! if pathological case, read only four values C0 and DCI,DCJ,DCK
  ! and return
  ! them as the two four elements of the array
  if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
    nl=0
    ir=4
  endif


  if (.not.present(extraction)) then
    call read_stream(iu,iprecr,nbmotr,nl,ir,c)
  else
    allocate(tmp(nl*nbmots+ir))
    call read_stream(iu,iprecr,nbmotr,nl,ir,tmp)

    if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
      imaxc  = -abs(extraction(2,1)-extraction(1,1)+1)
      jmaxc  = -abs(extraction(2,2)-extraction(1,2)+1)
      kmaxc  = -abs(extraction(2,3)-extraction(1,3)+1)
      c(1) = tmp(1) + sum(tmp(2:4)*(extraction(1,1:3)-1))
      c(2:3) = tmp(2:3)
    else
      extr = 1
      extr(:,1:size(extraction,2)) = extraction 
      call makeSubset(tmp,(/imaxr,jmaxr,kmaxr/),extr,c,shapec)
      imaxc = shapec(1)
      jmaxc = shapec(2)
      kmaxc = shapec(3)
    end if

    deallocate(tmp)
  end if

  return
99 continue
  write(*,*) 'Data error in UREADC, not a conform file'
  return


 end subroutine ureadc_all_kind

 !____________________________________________________________________
 !


 subroutine read_stream(iu,iprec,nbmots,nl,ir,var)
  implicit none
  integer, intent(in) :: iu,iprec,nbmots,nl,ir
  real, intent(out) :: var(*)

  integer :: kl,kc,ide,kb

  ! allocatable space for kind convertion

  real(4), allocatable :: p4(:)
  real(8), allocatable :: p8(:)

  ide=0  

  if (iprec.eq.kind(var)) then
    ! subroutine argument and file have the same precision: no temporary space needed

    do kl=1,nl
      read(iu,ERR=99) (var(ide+kc),kc=1,nbmots)
      ide=ide+nbmots
    end do
    read(iu,ERR=99) (var(ide+kc),kc=1,ir)

  elseif (iprec.eq.8.and.kind(var).eq.4) then
    ! subroutine argument and file have the simple and double precision respectively: 
    ! temporary space in double precision needed

    allocate(p8(nbmots))

    do kl=1,nl
      read(iu,ERR=99) (p8(kc),kc=1,nbmots)
      var(ide+1:ide+nbmots) = p8
      ide=ide+nbmots
    end do
    read(iu,ERR=99) (p8(kc),kc=1,ir)
    var(ide+1:ide+ir) = p8(1:ir)

    deallocate(p8)
  elseif (iprec.eq.4.and.kind(var).eq.8) then
    ! subroutine argument and file have the double and simple precision respectively: 
    ! temporary space in simple precision needed

    allocate(p4(nbmots))

    do kl=1,nl
      read(iu,ERR=99) (p4(kc),kc=1,nbmots)
      var(ide+1:ide+nbmots) = p4
      ide=ide+nbmots
    end do
    read(iu,ERR=99) (p4(kc),kc=1,ir)
    var(ide+1:ide+ir) = p4(1:ir)

    deallocate(p4)
  else
    goto 99
  end if

  return

  ! error section

99 write(0,*) 'Data error in UREADC, not a conform file'
  ERROR_STOP
 end subroutine read_stream


 !____________________________________________________________________
 !

 subroutine uwritc_all_kind(iu,c,valexc,imaxc,jmaxc,kmaxc,nbm)
  integer, parameter :: kblanc=10
  integer :: ic,imaxc,jmaxc,kmaxc,nbm
  real :: c(*)
  real :: valexc
  character(256) :: name
  integer :: iu,nbmots,iprec,nl,ide,kc,ir,kb,kl
  nbmots = nbm
  iprec = kind(c)

  if( (imaxc.gt.0).and.(jmaxc.gt.0).and.(kmaxc.gt.0) ) then

    IF (NBM.EQ.-1) NBMOTS = IMAXC*JMAXC*KMAXC

  endif
  !
  ! skip KBLANC lines
  do kb=1,KBLANC
    write(iu,ERR=99)
  end do

  write(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,real(valexc,4)

  ! compute the number of full records to read and the remaining words
  nl=(imaxc*jmaxc*kmaxc)/nbmots
  ir=imaxc*jmaxc*kmaxc-nbmots*nl
  ide=0

  ! if pathological case, write only four values C0 and DCI,DCJ,DCK found 
  ! as the two four elements of the array so that C(I,J,K) =
  ! C0 + I * DCI + J * DCJ + K * DCK
  if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
    nl=0
    ir=4
  endif


  ! single or double precision
  do kl=1,nl
    write(iu,ERR=99) (c(ide+kc),kc=1,nbmots)
    ide=ide+nbmots
  end do

  write(iu,ERR=99) (c(ide+kc),kc=1,ir)

  return

99 continue
  inquire(iu,name=name)        
  write(*,*) 'Data error in UWRITC, not a conform file'
  write(*,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc, filename'
  write(*,*) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc, trim(name)
  return
 end subroutine uwritc_all_kind



 !____________________________________________________________________
 !

 subroutine uwritc_orig(iu,c8,c4,valexc,iprec,imaxc,jmaxc,kmaxc,nbm)
  !                ======
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  ! writes the field C(I,J,K)  into fortran unit iu 
  ! writes the field in the array c4 if iprecr=4
  ! writes the field in the array c8 if iprecr=8
  !
  ! The KBLANC blank lines are at the disposal of the user
  ! JMB 6/3/92
  !
  ! IF c(i,j,k)=NaN or infinity, it is replaced by VALEX! 
  !
  ! 
  ! RS 12/1/93
  !
  ! If nbmots = -1  then write only 1 data record
  !     (only for non-degenerated data)
  !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  !
  integer, parameter :: kblanc=10
  real(kind=4) c4(*)
  real(kind=8) c8(*)
  real(kind=4) valexc
  character(256) :: name
  real :: z,un
  integer :: iu,kb,kl,kc
  integer :: ich,ioff,nbmots,nl,ide,ir,iprec,imaxc,jmaxc,kmaxc,nbm

  ! in the calling routin you can specify the following equivalence to
  ! save memory space:
  !      equivalence(c,c4)
  !      equivalence(c,c8)
  !
  ! Putting  Valex where not numbers
  z=0.
  un=1.
  ich=0
  ioff=1
  nbmots = nbm

  if( (imaxc.gt.0).and.(jmaxc.gt.0).and.(kmaxc.gt.0) ) then

    IF (NBM.EQ.-1) NBMOTS = IMAXC*JMAXC*KMAXC

    !$$$       do k=1,kmaxc
    !$$$        do j=1,jmaxc
    !$$$         do i=1,imaxc
    !$$$         if( c4(ioff).eq.(z/z) ) goto 1010 
    !$$$         if( c4(ioff).eq.(un/z) ) goto 1010 
    !$$$         if( c4(ioff).eq.(-z/z) ) goto 1010 
    !$$$         if( c4(ioff).eq.(-un/z) ) goto 1010 
    !$$$         goto 1011
    !$$$ 1010     continue
    !$$$          c4(ioff)=valex
    !$$$          ich=ich+1
    !$$$ 1011    continue 
    !$$$         ioff=ioff+1
    !$$$         enddo
    !$$$        enddo
    !$$$       enddo
    !$$$       if(ich.gt.0) then
    !$$$       write(stdout,*) ' WARNING:',ich,' Values are not numbers'
    !$$$       write(stdout,*) '   Changing them into VALEX'
    !$$$       endif
  endif
  !
  ! skip KBLANC lines
  do kb=1,KBLANC
    write(iu,ERR=99)
  end do

  write(iu) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc

  ! compute the number of full records to read and the remaining words
  nl=(imaxc*jmaxc*kmaxc)/nbmots
  ir=imaxc*jmaxc*kmaxc-nbmots*nl
  ide=0

  ! if pathological case, write only four values C0 and DCI,DCJ,DCK found 
  ! as the two four elements of the array so that C(I,J,K) =
  ! C0 + I * DCI + J * DCJ + K * DCK
  if(imaxc.lt.0.or.jmaxc.lt.0.or.kmaxc.lt.0) then
    nl=0
    ir=4
  endif


  ! single precision
  if(iprec.eq.4) then
    do kl=1,nl
      write(iu,ERR=99) (c4(ide+kc),kc=1,nbmots)
      ide=ide+nbmots
    end do
    write(iu,ERR=99) (c4(ide+kc),kc=1,ir)
  else
    !
    ! double precision
    if(iprec.eq.8) then
      do kl=1,nl
        write (iu,ERR=99) (c8(ide+kc),kc=1,nbmots)
        ide=ide+nbmots
      end do
      write (iu,ERR=99) (c8(ide+kc),kc=1,ir)
    else
      goto 99
    endif
  endif
  !
  return
99 continue
  inquire(iu,name=name)        
  write(*,*) 'Data error in UWRITC, not a conform file'
  write(*,*) 'imaxc,jmaxc,kmaxc,iprec,nbmots,valexc, filename'
  write(*,*) imaxc,jmaxc,kmaxc,iprec,nbmots,valexc, trim(name)
  return
 end subroutine uwritc_orig

 !____________________________________________________________________
 !

 integer function  freeunit()
  integer unit
  integer, parameter :: iu = 1000
  logical :: used, ok

  used = .true.
  unit = iu

  do while (used.or. .not. ok)
    unit = unit+1
    inquire(unit,opened=used,exist=ok)
    !    if (used) write(stdout,*) unit,' used'
  end do

  !  write(stdout,*) unit
  freeunit = unit
 end function freeunit



 !____________________________________________________________________
 !

 subroutine parseExtraction(str,extrems,indexes)
  implicit none
  character(len=*), intent(in) :: str
  integer, intent(in) :: extrems(:,:)
  integer, pointer :: indexes(:,:)

  integer :: i,j,ndim, first,last, fi,li, indv

  first = index(str,'(')+1
  last = index(str,')',.true.)-1

  ndim = 1  
  do i=first,last     
    if (str(i:i).eq.',') ndim = ndim+1
  end do

  allocate(indexes(2,ndim))

  fi = first
  do j=1,ndim
    if (j.ne.ndim) then    
      li = fi + index(str(fi:last),',') -2
    else
      li = last
    end if

    indv = index(str(fi:li),':')

    if (indv.ne.0) then
      indexes(1,j) = tointlower(str(fi:fi+indv-2),extrems(1,j),extrems(2,j))
      indexes(2,j) = tointupper(str(fi+indv:li),  extrems(1,j),extrems(2,j))
    else
      indexes(1,j) = tointlower(str(fi:li),extrems(1,j),extrems(2,j))
      indexes(2,j) = indexes(1,j)
    end if

    fi = li+2
  end do

 contains 

  integer function tointupper(str,minindex,maxindex)
   implicit none
   character(len=*), intent(in) :: str
   integer, intent(in) :: minindex,maxindex

   if (str.eq.'start') then
     tointupper = minindex
   elseif (len_trim(str).eq.0.or.str.eq.'end') then
     tointupper = maxindex
   else
     read(str,*) tointupper
   end if
  end function tointupper

  integer function tointlower(str,minindex,maxindex)
   implicit none
   character(len=*), intent(in) :: str
   integer, intent(in) :: minindex,maxindex

   if (len_trim(str).eq.0.or.str.eq.'start') then
     tointlower = minindex
   elseif (str.eq.'end') then
     tointlower = maxindex
   else
     read(str,*) tointlower
   end if

  end function tointlower

 end subroutine parseExtraction

 !____________________________________________________________________
 !

 !
 !____________________________________________________________________
 !

 subroutine uload_degenerated(filename,c0,cx,cy,cz,imax,jmax,kmax)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(out) :: c0,cx,cy,cz
  integer, intent(out) :: imax,jmax,kmax

  character(len=len(filename)) :: tmpname
  real :: c(4)
  integer  :: prec,nbmots,vshape(MaxDimensions)=1,ndim
  real  :: valex
  logical :: isdegen

  call gunzipFile(filename,tmpname)  
  call uinquire(tmpname,valex,prec,ndim,vshape,isdegen)

  if (isdegen) then
    call uread(tmpname,c)
    c0 = c(1); cx = c(2); cy = c(3); cz = c(4);
    imax = vshape(1)
    jmax = vshape(2)
    kmax = vshape(3)
  else
    write(stderr,*) '"',trim(filename),'" is not degenerated.'
    ERROR_STOP
  end if
  call rmgunzipFile(tmpname,tmpname)

 end subroutine uload_degenerated



 !____________________________________________________________________
 !

 subroutine uload_Real1D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real,allocatable :: c(:)
  real, intent(out), optional :: valex

  character(len=len(filename)) :: tmpname
  integer  :: prec,ndim,vshape(MaxDimensions)=1
  real  :: valexc
  logical :: isdegen

  call gunzipFile(filename,tmpname)
  call uinquire(tmpname,valexc,prec,ndim,vshape,isdegen)

  if (isdegen) then
    write(stderr,*) 'uload_Real1D: Error: "',trim(filename),'" is degenerated'
    ERROR_STOP
  end if

  allocate(c(product(vshape(1:ndim))))
  call uread(tmpname,c,valexc)
  call rmgunzipFile(filename,tmpname)

  if (present(valex)) valex = valexc
 end subroutine uload_Real1D

 !____________________________________________________________________
 !

 subroutine uload_Real0D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real  :: c
  real, intent(out), optional :: valex

  character(len=len(filename)) :: tmpname
  integer  :: prec,ndim,vshape(MaxDimensions)=1
  real  :: valexc
  logical :: isdegen
  real,allocatable :: d(:)

  call gunzipFile(filename,tmpname)
  call uinquire(tmpname,valexc,prec,ndim,vshape,isdegen)

  if (isdegen) then
    write(stderr,*) 'uload_Real1D: Error: "',trim(filename),'" is degenerated'
    ERROR_STOP
  end if

  allocate(d(1))
!  allocate(c(product(vshape(1:ndim))))
  call uread(tmpname,d,valexc)
  call rmgunzipFile(filename,tmpname)

  c=d(1)
  deallocate(d)

  if (present(valex)) valex = valexc
 end subroutine uload_Real0D

 !____________________________________________________________________
 !

 subroutine uload_Real2D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, allocatable  :: c(:,:)
  real, intent(out), optional :: valex

  character(len=len(filename)) :: tmpname
  integer  :: prec,ndim,vshape(MaxDimensions)=1
  real  :: valexc
  logical :: isdegen

  call gunzipFile(filename,tmpname)
  call uinquire(tmpname,valexc,prec,ndim,vshape,isdegen)

  if (isdegen) then
    write(stderr,*) 'uload_Real2D: Error: "',trim(filename),'" is degenerated'
    ERROR_STOP
  end if

  allocate(c(vshape(1),product(vshape(2:ndim))))
  call uread(tmpname,c,valexc)
  call rmgunzipFile(filename,tmpname)

  if (present(valex)) valex = valexc
 end subroutine uload_Real2D

 !____________________________________________________________________
 !

 subroutine uload_Double2D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real(8), allocatable  :: c(:,:)
  real, intent(out), optional :: valex

  character(len=len(filename)) :: tmpname
  integer  :: prec,ndim,vshape(MaxDimensions)=1
  real  :: valexc
  logical :: isdegen

  call gunzipFile(filename,tmpname)
  call uinquire(tmpname,valexc,prec,ndim,vshape,isdegen)

  if (isdegen) then
    write(stderr,*) 'uload_Real2D: Error: "',trim(filename),'" is degenerated'
    ERROR_STOP
  end if

  allocate(c(vshape(1),product(vshape(2:ndim))))
  call uread_double(tmpname,c,valexc)
  call rmgunzipFile(filename,tmpname)

  if (present(valex)) valex = valexc
 end subroutine uload_Double2D

 !____________________________________________________________________
 !

 subroutine uload_Real3D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, allocatable  :: c(:,:,:)
  real, intent(out), optional :: valex

  character(len=len(filename)) :: tmpname
  integer  :: prec,ndim,vshape(MaxDimensions)
  real  :: valexc
  logical :: isdegen

  vshape=1
  call gunzipFile(filename,tmpname)
  call uinquire(tmpname,valexc,prec,ndim,vshape,isdegen)

  !  if (isdegen) then
  !    write(stderr,*) 'uload_Real3D: Error: "',trim(filename),'" is degenerated'
  !    ERROR_STOP
  !  end if

  !write(stderr,*) 'shape ',vshape,ndim
  allocate(c(vshape(1),vshape(2),product(vshape(3:ndim))))
  call uread(tmpname,c,valexc)
  call rmgunzipFile(filename,tmpname)

  if (isdegen) call uexplo(c,ndim,vshape)

  if (present(valex)) valex = valexc
 end subroutine uload_Real3D


 !____________________________________________________________________
 !

 subroutine uload_Real4D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, pointer  :: c(:,:,:,:)
  real, intent(out), optional :: valex

  character(len=len(filename)) :: tmpname
  integer  :: prec,ndim,vshape(MaxDimensions)
  real  :: valexc
  logical :: isdegen

  vshape=1
  call gunzipFile(filename,tmpname)
  call uinquire(tmpname,valexc,prec,ndim,vshape,isdegen)

  !  if (isdegen) then
  !    write(stderr,*) 'uload_Real3D: Error: "',trim(filename),'" is degenerated'
  !    ERROR_STOP
  !  end if

  !write(stderr,*) 'shape ',vshape,ndim
  allocate(c(vshape(1),vshape(2),vshape(3),product(vshape(4:ndim))))
  call uread(tmpname,c,valexc)
  call rmgunzipFile(filename,tmpname)

  if (isdegen) call uexplo(c,ndim,vshape)

  if (present(valex)) valex = valexc
 end subroutine uload_Real4D


 !____________________________________________________________________
 !

 subroutine uload_Integer0D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  integer  :: c
  real, intent(out), optional :: valex

  character(len=len(filename)) :: tmpname
  integer  :: prec,ndim,vshape(MaxDimensions)=1
  real  :: valexc
  logical :: isdegen
  real,pointer :: r(:)

  call gunzipFile(filename,tmpname)
  call uinquire(tmpname,valexc,prec,ndim,vshape,isdegen)

  if (isdegen) then
    write(stderr,*) 'uload_Integer0D: Error: "',trim(filename),'" is degenerated'
    ERROR_STOP
  end if

!  allocate(c(product(vshape(1:ndim))))
  allocate(r(1))
  call uread(tmpname,r,valexc)
  call rmgunzipFile(filename,tmpname)

  c=floor(r(1)+0.5)
  deallocate(r)

  if(present(valex)) valex=valexc

 end subroutine uload_Integer0D

 !____________________________________________________________________
 !

 subroutine uload_Integer1D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  integer :: c(:)
!  integer,allocatable :: c(:)
  integer, intent(out),optional :: valex

  real, allocatable :: r(:)
  real :: valexr

  call uload_Real1D(filename,r,valexr)
!  allocate(c(size(r,1)))
  c = floor(r+.5)
  valex = floor(valexr+.5)
  deallocate(r)

 end subroutine uload_Integer1D

 !____________________________________________________________________
 !

 subroutine uload_Integer2D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  integer, allocatable  :: c(:,:)
  integer, intent(out) :: valex

  real, allocatable :: r(:,:)
  real :: valexr

  call uload_Real2D(filename,r,valexr)
  allocate(c(size(r,1),size(r,2)))
  c = floor(r+.5)
  valex = floor(valexr+.5)
  deallocate(r)

 end subroutine uload_Integer2D

 !____________________________________________________________________
 !

 subroutine uload_Integer3D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  integer, allocatable  :: c(:,:,:)
  integer, intent(out) :: valex

  real, allocatable :: r(:,:,:)
  real :: valexr

  call uload_Real3D(filename,r,valexr)
  allocate(c(size(r,1),size(r,2),size(r,3)))
  c = floor(r+.5)
  valex = floor(valexr+.5)
  deallocate(r)

 end subroutine uload_Integer3D


 !____________________________________________________________________
 !

 !____________________________________________________________________
 !


 subroutine usave_generic(filename,c,valex,ndim,vshape,isdegen)
  implicit none
  character(len=*), intent(in) :: filename
  real :: c(*)
  real,     intent(in) :: valex
  integer,  intent(in) :: ndim, vshape(:)
  logical,  intent(in) :: isdegen

  integer :: indp,j,prec,nbmots,ndimc
  integer :: e(2,MaxDimensions),lentot,ip,jp,kp,l,lp,d
  integer, pointer :: extraction(:,:), ind(:), cummul(:)
  real, pointer :: tmp(:)
  real :: valexc
  logical :: isdegenc

  !  write(stdout,*) 'usave ', vshape(1:ndim), ndim

  indp = index(filename,'(')

  if (indp.eq.0) then
    call uwrite_prim(filename,c,valex,ndim,vshape,isdegen)
  else
    e = 1
    call uinquire(filename(1:indp-1),valexc,prec,ndimc,e(2,:),isdegenc)
    call parseExtraction(filename,e,extraction)
    !  write(stdout,*) 'usave 2 ', vshape(1:ndim), ndim
    call uwrite_prim(filename(1:indp-1),c,valex,ndim,vshape,isdegen,extraction)
    deallocate(extraction)
  end if


 end subroutine usave_generic

 !____________________________________________________________________
 !


 subroutine usave_generic_double(filename,c8,valex,ndim,vshape,isdegen)
  implicit none
  character(len=*), intent(in) :: filename
  real(8) :: c8(*)
  real,     intent(in) :: valex
  integer,  intent(in) :: ndim, vshape(:)
  logical,  intent(in) :: isdegen

  integer :: indp,j,prec,nbmots,ndimc
  integer :: e(2,MaxDimensions),lentot,ip,jp,kp,l,lp,d
  integer, pointer :: extraction(:,:), ind(:), cummul(:)
  real, pointer :: tmp(:)
  real :: valexc
  logical :: isdegenc

  !  write(stdout,*) 'usave ', vshape(1:ndim), ndim

  indp = index(filename,'(')

  if (indp.eq.0) then
    call uwrite_prim_double(filename,c8,valex,ndim,vshape,isdegen)
  else
    e = 1
    call uinquire(filename(1:indp-1),valexc,prec,ndimc,e(2,:),isdegenc)
    call parseExtraction(filename,e,extraction)
    !  write(stdout,*) 'usave 2 ', vshape(1:ndim), ndim
    call uwrite_prim_double(filename(1:indp-1),c8,valex,ndim,vshape,isdegen,extraction)
    deallocate(extraction)
  end if


 end subroutine usave_generic_double

 !____________________________________________________________________
 !

 !
 ! ndim, vshape : dimension and shape of the variable to save
 ! ndimp, vshapep : dimension and shape of the variable already present in the netcdf with the same name
 ! ncvdimp, ncvshap : dimension and shape of the netcdf variable to save, it is different 
 !             from ndim and vshape only for degenerated files
 !
 ! ncvalex: missing value of netcdf variable if already present, otherwise ncvalex=valex
 !
 subroutine uwrite_prim(filename,c,valex,ndim,vshape,isdegen,extraction)
#ifdef NETCDF
  use netcdf
#endif 
  implicit none
#ifdef NETCDF
  include 'netcdf.inc' 
#endif 

  character(len=*), intent(in) :: filename
  real, intent(in), target :: c(*)
  real, intent(in) :: valex
  integer, intent(in) :: ndim, vshape(:)
  logical, intent(in) :: isdegen
  integer, optional, intent(in) :: extraction(:,:)

  integer :: iu , vshapec(MaxDimensions)
  integer stat, nbmots

#ifdef NETCDF
  integer :: i,j,pos,ncid,rcode, ndims, nvars, natts, recdim, cid, &
       vtype, cdim(MaxDimensions), ncvdim, ncvshape(MaxDimensions), dimlen, rcode2, &
       totalsize, ndimp, vshapep(MaxDimensions)

  real :: ncvalex

  ! temporary space to replace the missing value
  real, pointer :: ncc(:)

  real, allocatable :: temp(:)
  character(len=128) :: var,name,namedim(MaxDimensions)
  logical :: fileexists

  !  write(stdout,*) 'uw 1 ', vshape(1:ndim), ndim


  pos = index(filename,'#')

  if (pos.eq.0) then

#endif
    if (ndim.gt.3.and..not.isdegen) then
      write(stderr,*) 'Warning: collapse dimention 3 to ',ndim
      !    stop
    end if

!$omp critical (reserveFreeUnit)
    iu = freeunit()
    open(iu,file=trim(filename),iostat=stat,form='unformatted')
!$omp end critical (reserveFreeUnit)

    if (stat.ne.0) then
      write(stderr,*) 'usave_generic: Error while creating file "',trim(filename), &
           '". (unit=',iu,', iostat=',stat,')'
      ERROR_STOP
    end if

    vshapec = 1
    vshapec(1:ndim) = vshape(1:ndim)

    if (isdegen) then
      call uwritc_all_kind(iu,c,valex,-abs(vshapec(1)),-abs(vshapec(2)),-abs(vshapec(3)),MaxRecordLength)
    else
      vshapec(3) =  product(vshape(3:ndim))
      call uwritc_all_kind(iu,c,valex,vshapec(1),vshapec(2),vshapec(3),MaxRecordLength)
    end if

!$omp critical
    close(iu)
!$omp end critical

#ifdef NETCDF
  else
    !  write(stdout,*) 'write nc',vshape(1:ndim),ndim

    ncvshape = 1

    if (.not.isdegen) then
      ncvdim = ndim
      ncvshape(1:ndim) = vshape(1:ndim)
    else
      ncvdim = 1
      ncvshape(1) = 4
    end if

    !  write(stderr,*) 'nvcshape ',ncvshape(1:ncvdim),ncvdim

    inquire(file=trim(filename(1:pos-1)),exist=fileexists)

    !    write(stdout,*) fileexists 

    !
    ! open or create NetCDF file
    !

    if (fileexists) then
      ncid = ncopn(trim(filename(1:pos-1)),ncwrite,rcode)
      call ncredf(ncid,rcode)
    else
      ncid = nccre(trim(filename(1:pos-1)),ncnoclob,rcode)
    end if

    ! disable error message 

    CALL NCPOPT(0)
    cid = ncvid(NCID, filename(pos+1:),rcode)
    ! write(6,*) 'cid,  vshape(1:ndim) ',cid,  vshape(1:ndim), rcode, rcode2

    !
    ! re-enable error message
    !

    CALL NCPOPT(NCVERBOS+NCFATAL)

    if (rcode.eq.0) then
      !
      ! variable already exist
      !

      !
      ! retrive its dimension: ndimp
      !

      call ncvinq (ncid,cid,name,vtype,ndimp,cdim,natts,rcode)

      ! retrieve its shape: vshapep

      vshapep = 1 

      do i=1,ndimp
        call ncdinq (ncid,cdim(i),namedim(i), vshapep(i),rcode)
      end do


      ! write(6,*) 'ncvinq ',cid,  vtype,ndimp,ncvdim

      ! no shape check if extraction

      if (.not.present(extraction)) then
#      ifdef CHECK_SHAPE
        !
        ! check shape of variable
        !


        !
        ! check number of dimension
        !

        if (count(ncvshape.ne.1).ne.count(vshapep.ne.1)) then
          write(stderr,*) 'ERROR: wrong number of (non-singelton) dimesion of variable ',trim(name),'".'
          write(stderr,*) 'Expected: ',count(ncvshape.ne.1), "shape ",pack(ncvshape,ncvshape.ne.1)
          write(stderr,*) 'Found: ',count(vshapep.ne.1), " shape ",pack(vshapep,vshapep.ne.1)
          ERROR_STOP
        end if


        !
        ! check length of dimension
        !

        if (any(pack(ncvshape,ncvshape.ne.1).ne.pack(vshapep,vshapep.ne.1))) then
          write(stderr,*) 'ERROR: wrong length of dimension of variable "',trim(name),'".'
          write(stderr,*) 'Expected: ',pack(ncvshape,ncvshape.ne.1)
          write(stderr,*) 'Found: ',pack(vshapep,vshapep.ne.1)
          ERROR_STOP
        end if


#      else
        !
        ! check only total number of elemets and perform eventually a reshape
        !

        if (product(ncvshape(1:ncvdim)).ne.product(vshapep(1:ndimp))) then
          write(stderr,*) 'ERROR: wrong total size of variable "',trim(name),'".'
          write(stderr,*) 'Expected: ',        ncvshape(1:ncvdim)
          write(stderr,*) 'Found: ',vshapep(1:ndimp)
          ERROR_STOP
        else 
          write(stderr,*) 'Expected: ',        ncvshape(1:ncvdim)
          write(stderr,*) 'Found: ',vshapep(1:ndimp)
        end if



#      endif
      end if

      ! all check passed, difference in shape (if any) are irrelevant

    else


      !
      ! create dimensions if not exist
      !

      do j=1,ncvdim
        CALL NCINQ(NCID, NDIMS, NVARS, NATTS, RECDIM, RCODE)
        cdim(j) = -1

        ! 
        ! look for a approriate dimension
        !

        do i=1,ndims
          call ncdinq (ncid,i,namedim(i), dimlen,rcode)

          if (ncvshape(j).eq.dimlen) then
            ! write(stdout,*) 'found ',j,trim(namedim(i)),dimlen,rcode
            cdim(j) = i
            exit 
          end if
        end do

        if (cdim(j).eq.-1) then

          !
          ! create the dimension
          !

          write(name,'("dim",I3.3)') ndims+1
          !        write(stdout,*) 'dimname ',trim(name),j,ncvshape(j)
          cdim(j) = ncddef (ncid,trim(name),ncvshape(j),rcode)
        end if
      end do

      ! precision depend on the default kind of the real variable

      if (kind(c).eq.4) then
        vtype =  ncfloat
      else
        vtype =  ncdouble
      end if


      !
      ! create variable
      !
      cid = ncvdef(ncid,filename(pos+1:),vtype,ncvdim,cdim,rcode)

      ! these shape are equal
      ! they may differ if the veriable already exist

      vshapep = ncvshape
      ndimp = ncvdim  
    end if


    if (isdegen) then
      name = 'linear'
      call ncaptc(ncid,cid,'field_type', ncchar,len_trim(name),trim(name),rcode)
      call ncapt(ncid,cid,'shape',nclong,ndim,vshape(1:ndim),rcode)
    end if

    ! test if the missing_value attribute is already defined

    rcode = nf90_get_att(ncid, cid, 'missing_value', ncvalex)

    if (rcode.ne.nf90_noerr) then
      rcode = nf90_put_att(ncid, cid, 'missing_value', valex)
      ncvalex = valex
    end if
    ! write(stdout,*) 'write out ',rcode,ncvalex,valex

    call ncendf (ncid,rcode)

    ! 
    ! put variable in netcdf file with eventual conversion
    !

    if (isdegen) then
      !        rcode = nf90_put_var(ncid, cid, c, 1, ndim+1)
               rcode = nf90_put_var(ncid, cid, c(1:ndim+1), (/ 1 /), (/ ndim+1 /))
    else
      !        rcode = nf90_put_var(ncid, cid, c, spread(1,1,ndimp), vshapep(1:ndimp))
      !        rcode = nf90_put_var(ncid, cid, c(1:product(vshape(1:ndim))), spread(1,1,ndim), vshape(1:ndim))
      !         write(stdout,*) 'write out ',product(vshapep), sum(c(1:product(vshapep)))/product(vshapep), cid

      ! replace valex by ncvalex if necessary

      if (valex.ne.ncvalex) then
        !write(stdout,*) 'replace ',ncvalex,valex, vshape(1:ndim)

        allocate(ncc(1:product(vshape(1:ndim))))

        !write(stdout,*) 'replace ',vshape(1:ndim),shape(ncc),product(vshape(1:ndim))

           where ( c(1:product(vshape(1:ndim))).eq.valex)
             ncc = ncvalex
           elsewhere
             ncc = c(1:product(vshape(1:ndim)))
           end where
        !write(stdout,*) 'end replace ',ncvalex,valex
      else
             ncc =>  c(1:product(vshape(1:ndim)))
      end if

      ! bug in Netcdf Fortran 90 interface ? Fortran 77 interface works

      if (.not.present(extraction)) then
        if (kind(c).eq.4)     rcode=   nf_put_var_real(ncid,cid,ncc)
        if (kind(c).eq.8)     rcode=   nf_put_var_double(ncid,cid,ncc)
        ! instead of
        !        rcode = nf90_put_var(ncid, cid, ncc)
      else
        ! pad vector extraction(1,1:ndim) and  vshape(1:ndim) with trailing 1 such that their length is equal to ndimp (i.e. the 
        ! dimension of the netcdf variable


        if (kind(c).eq.4) then
          rcode= nf_put_vara_real(ncid,cid,  extraction(1,1:ndimp) , &
               (/ vshape(1:ndim) ,  (1,i=ndim+1,ndimp) /), ncc)
        else
          rcode= nf_put_vara_double(ncid,cid, extraction(1,1:ndimp) , &
               (/ vshape(1:ndim) , (1,i=ndim+1,ndimp) /), ncc)
        end if
      end if

      ! dellocate the temporary space


      if (valex.ne.ncvalex) then
        deallocate(ncc)
      end if

    end if

    call ncclos (ncid,rcode)
  end if
  ! endif for NETCDF
#endif 

 end subroutine uwrite_prim

 !____________________________________________________________________
 !

 subroutine uwrite_prim_double(filename,c,valex,ndim,vshape,isdegen,extraction)
#ifdef NETCDF
  use netcdf
#endif
  implicit none
#ifdef NETCDF
  include 'netcdf.inc'
#endif

  character(len=*), intent(in) :: filename
  real(8), intent(in), target :: c(*)
  real, intent(in) :: valex
  integer, intent(in) :: ndim, vshape(:)
  logical, intent(in) :: isdegen
  integer, optional, intent(in) :: extraction(:,:)

  integer :: iu , vshapec(MaxDimensions)
  integer stat, nbmots
#ifdef NETCDF
  integer :: i,j,pos,ncid,rcode, ndims, nvars, natts, recdim, cid, &
       vtype, cdim(MaxDimensions), ncvdim, ncvshape(MaxDimensions), dimlen, rcode2, &
       totalsize, ndimp, vshapep(MaxDimensions)

  real :: ncvalex

  ! temporary space to replace the missing value
  real(8), pointer :: ncc(:)

  real(8), allocatable :: temp(:)
  character(len=128) :: var,name,namedim(MaxDimensions)
  logical :: fileexists

  !  write(stdout,*) 'uw 1 ', vshape(1:ndim), ndim


  pos = index(filename,'#')

  if (pos.eq.0) then
#endif
    if (ndim.gt.3.and..not.isdegen) then
      write(stderr,*) 'Warning: collapse dimention 3 to ',ndim
      !    stop
    end if

!$omp critical (reserveFreeUnit)
    iu = freeunit()
    open(iu,file=trim(filename),iostat=stat,form='unformatted')
!$omp end critical (reserveFreeUnit)

    if (stat.ne.0) then
      write(stderr,*) 'usave_generic: Error while creating file "',trim(filename), &
           '". (unit=',iu,', iostat=',stat,')'
      ERROR_STOP
    end if

    vshapec = 1
    vshapec(1:ndim) = vshape(1:ndim)

    if (isdegen) then
!      call uwritc_all_kind(iu,c,valex,-abs(vshapec(1)),-abs(vshapec(2)),-abs(vshapec(3)),MaxRecordLength)
       write(*,*) "cannot save explicitly REAL*8 variables in GHER files"
    else
      vshapec(3) =  product(vshape(3:ndim))
!      call uwritc_all_kind(iu,c,valex,vshapec(1),vshapec(2),vshapec(3),MaxRecordLength)
       write(*,*) "cannot save explicitly REAL*8 variables in GHER files"
    end if
!$omp critical
    close(iu)
!$omp end critical

#ifdef NETCDF
  else
    !  write(stdout,*) 'write nc',vshape(1:ndim),ndim

    ncvshape = 1

    if (.not.isdegen) then
      ncvdim = ndim
      ncvshape(1:ndim) = vshape(1:ndim)
    else
      ncvdim = 1
      ncvshape(1) = 4
    end if
    !  write(stderr,*) 'nvcshape ',ncvshape(1:ncvdim),ncvdim

    inquire(file=trim(filename(1:pos-1)),exist=fileexists)

    !    write(stdout,*) fileexists

    !
    ! open or create NetCDF file
    !

    if (fileexists) then
      ncid = ncopn(trim(filename(1:pos-1)),ncwrite,rcode)
      call ncredf(ncid,rcode)
    else
      ncid = nccre(trim(filename(1:pos-1)),ncnoclob,rcode)
    end if

    ! disable error message

    CALL NCPOPT(0)
    cid = ncvid(NCID, filename(pos+1:),rcode)
    ! write(6,*) 'cid,  vshape(1:ndim) ',cid,  vshape(1:ndim), rcode, rcode2

    !
    ! re-enable error message
    !

    CALL NCPOPT(NCVERBOS+NCFATAL)

    if (rcode.eq.0) then
      !
      ! variable already exist
      !

      !
      ! retrive its dimension: ndimp
      !

      call ncvinq (ncid,cid,name,vtype,ndimp,cdim,natts,rcode)

      ! retrieve its shape: vshapep

      vshapep = 1
      do i=1,ndimp
        call ncdinq (ncid,cdim(i),namedim(i), vshapep(i),rcode)
      end do


      ! write(6,*) 'ncvinq ',cid,  vtype,ndimp,ncvdim

      ! no shape check if extraction

      if (.not.present(extraction)) then
#      ifdef CHECK_SHAPE
        !
        ! check shape of variable
        !


        !
        ! check number of dimension
        !

        if (count(ncvshape.ne.1).ne.count(vshapep.ne.1)) then
          write(stderr,*) 'ERROR: wrong number of (non-singelton) dimesion of variable ',trim(name),'".'
          write(stderr,*) 'Expected: ',count(ncvshape.ne.1), "shape ",pack(ncvshape,ncvshape.ne.1)
          write(stderr,*) 'Found: ',count(vshapep.ne.1), " shape ",pack(vshapep,vshapep.ne.1)
          ERROR_STOP
        end if


        !
        ! check length of dimension
        !

        if (any(pack(ncvshape,ncvshape.ne.1).ne.pack(vshapep,vshapep.ne.1))) then
          write(stderr,*) 'ERROR: wrong length of dimension of variable "',trim(name),'".'
          write(stderr,*) 'Expected: ',pack(ncvshape,ncvshape.ne.1)
          write(stderr,*) 'Found: ',pack(vshapep,vshapep.ne.1)
          ERROR_STOP
        end if


#      else
        !
        ! check only total number of elemets and perform eventually a reshape
        !

        if (product(ncvshape(1:ncvdim)).ne.product(vshapep(1:ndimp))) then
          write(stderr,*) 'ERROR: wrong total size of variable "',trim(name),'".'
          write(stderr,*) 'Expected: ',        ncvshape(1:ncvdim)
          write(stderr,*) 'Found: ',vshapep(1:ndimp)
          ERROR_STOP
        else
          write(stderr,*) 'Expected: ',        ncvshape(1:ncvdim)
          write(stderr,*) 'Found: ',vshapep(1:ndimp)
        end if



#      endif
      end if

      ! all check passed, difference in shape (if any) are irrelevant

    else


      !
      ! create dimensions if not exist
      !
      do j=1,ncvdim
        CALL NCINQ(NCID, NDIMS, NVARS, NATTS, RECDIM, RCODE)
        cdim(j) = -1

        !
        ! look for a approriate dimension
        !

        do i=1,ndims
          call ncdinq (ncid,i,namedim(i), dimlen,rcode)

          if (ncvshape(j).eq.dimlen) then
            ! write(stdout,*) 'found ',j,trim(namedim(i)),dimlen,rcode
            cdim(j) = i
            exit
          end if
        end do

        if (cdim(j).eq.-1) then

          !
          ! create the dimension
          !

          write(name,'("dim",I3.3)') ndims+1
          !        write(stdout,*) 'dimname ',trim(name),j,ncvshape(j)
          cdim(j) = ncddef (ncid,trim(name),ncvshape(j),rcode)
        end if
      end do

      ! precision depend on the default kind of the real variable

!      if (kind(c).eq.4) then
!        vtype =  ncfloat
!      else
        vtype =  ncdouble
!      end if


      !
      ! create variable
      !
      cid = ncvdef(ncid,filename(pos+1:),vtype,ncvdim,cdim,rcode)

      ! these shape are equal
      ! they may differ if the veriable already exist

      vshapep = ncvshape
      ndimp = ncvdim
    end if


    if (isdegen) then
      name = 'linear'
      call ncaptc(ncid,cid,'field_type', ncchar,len_trim(name),trim(name),rcode)
      call ncapt(ncid,cid,'shape',nclong,ndim,vshape(1:ndim),rcode)
    end if

    ! test if the missing_value attribute is already defined

    rcode = nf90_get_att(ncid, cid, 'missing_value', ncvalex)

    if (rcode.ne.nf90_noerr) then
      rcode = nf90_put_att(ncid, cid, 'missing_value', valex)
      ncvalex = valex
    end if
    ! write(stdout,*) 'write out ',rcode,ncvalex,valex

    call ncendf (ncid,rcode)

    !
    ! put variable in netcdf file with eventual conversion
    !

    if (isdegen) then
      !        rcode = nf90_put_var(ncid, cid, c, 1, ndim+1)
               rcode = nf90_put_var(ncid, cid, c(1:ndim+1), (/ 1 /), (/ ndim+1 /))
    else
      !        rcode = nf90_put_var(ncid, cid, c, spread(1,1,ndimp), vshapep(1:ndimp))
      !        rcode = nf90_put_var(ncid, cid, c(1:product(vshape(1:ndim))), spread(1,1,ndim), vshape(1:ndim))
      !         write(stdout,*) 'write out ',product(vshapep), sum(c(1:product(vshapep)))/product(vshapep), cid

      ! replace valex by ncvalex if necessary

      if (valex.ne.ncvalex) then
        !write(stdout,*) 'replace ',ncvalex,valex, vshape(1:ndim)

        allocate(ncc(1:product(vshape(1:ndim))))

        !write(stdout,*) 'replace ',vshape(1:ndim),shape(ncc),product(vshape(1:ndim))

           where ( c(1:product(vshape(1:ndim))).eq.valex)
             ncc = ncvalex
           elsewhere
             ncc = c(1:product(vshape(1:ndim)))
           end where
        !write(stdout,*) 'end replace ',ncvalex,valex
      else
             ncc =>  c(1:product(vshape(1:ndim)))
      end if

      ! bug in Netcdf Fortran 90 interface ? Fortran 77 interface works

      if (.not.present(extraction)) then
 !       if (kind(c).eq.4)     rcode=   nf_put_var_real(ncid,cid,ncc)
        if (kind(c).eq.8)     rcode=   nf_put_var_double(ncid,cid,ncc)
        ! instead of
        !        rcode = nf90_put_var(ncid, cid, ncc)
      else
        ! pad vector extraction(1,1:ndim) and  vshape(1:ndim) with trailing 1 such that their length is equal to ndimp (i.e. the
        ! dimension of the netcdf variable


!        if (kind(c).eq.4) then
!          rcode= nf_put_vara_real(ncid,cid,  extraction(1,1:ndimp) , &
!               (/ vshape(1:ndim) ,  (1,i=ndim+1,ndimp) /), ncc)
!        else
          rcode= nf_put_vara_double(ncid,cid, extraction(1,1:ndimp) , &
               (/ vshape(1:ndim) , (1,i=ndim+1,ndimp) /), ncc)
!        end if
      end if

      ! dellocate the temporary space


      if (valex.ne.ncvalex) then
        deallocate(ncc)
      end if

    end if

    call ncclos (ncid,rcode)
  end if
  ! endif for NETCDF
#endif

 end subroutine uwrite_prim_double


 !____________________________________________________________________
 !

 subroutine usave_degenerated(filename,c0,cx,cy,cz,imax,jmax,kmax)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(in) :: c0,cx,cy,cz
  integer, intent(in) :: imax,jmax,kmax

  real :: c(4)
  real  :: valex

  c(1) = c0; c(2) = cx; c(3) = cy; c(4) = cz
  call usave_generic(filename,c,9999.,3,(/ abs(imax),abs(jmax),abs(kmax) /),.true.)

 end subroutine usave_degenerated

 !____________________________________________________________________
 !

 subroutine usave_Real0D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(in)  :: c, valex
  real,pointer :: d(:)

  allocate(d(1))
  d=c
  call usave_generic(filename,d,valex,1,(/1/),.false.)
  deallocate(d)

 end subroutine usave_Real0D

 !____________________________________________________________________
 !

 subroutine usave_Integer0D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  integer, intent(in)  :: c
  real, intent(in) :: valex
  real, allocatable :: d(:)

  allocate(d(1))
  d=float(c)
  !write(*,*) "usave_INTEGER0D c=",c
  !write(*,*) "usave_INTEGER0D c=",d
  call usave_generic(filename,d,valex,1,(/1/),.false.)
  deallocate(d)

 end subroutine usave_Integer0D

 !____________________________________________________________________
 !

 subroutine usave_Real1D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(in)  :: c(:), valex

  call usave_generic(filename,c,valex,1,shape(c),.false.)

 end subroutine usave_Real1D

 !____________________________________________________________________
 !

 subroutine usave_Integer1D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  integer, intent(in) :: c(:)
  real, intent(in)  :: valex
  real,allocatable :: d(:)

  allocate(d(size(c)))
  d=float(c)
  call usave_generic(filename,d,valex,1,shape(c),.false.)
  deallocate(d)

 end subroutine usave_Integer1D

 !____________________________________________________________________
 !

 subroutine usave_Real2D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(in)  :: c(:,:), valex

  call usave_generic(filename,c,valex,2,shape(c),.false.)

 end subroutine usave_Real2D

 !____________________________________________________________________
 !

 subroutine usave_Double2D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real(8), intent(in)  :: c(:,:)
  real, intent(in) :: valex

  call usave_generic_double(filename,c,valex,2,shape(c),.false.)

 end subroutine usave_Double2D

 !____________________________________________________________________
 !

 subroutine usave_Integer2D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(in)  :: valex
  integer, intent(in) :: c(:,:)
  real,allocatable :: d(:,:)

  allocate(d(size(c,1),size(c,2)))
  d=float(c)
  call usave_generic(filename,d,valex,2,shape(c),.false.)
  deallocate(d)

 end subroutine usave_Integer2D

 !____________________________________________________________________
 !

 subroutine usave_Real3D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(in)  :: c(:,:,:), valex

  call usave_generic(filename,c,valex,3,shape(c),.false.)

 end subroutine usave_Real3D


 !____________________________________________________________________
 !

 subroutine usave_Integer3D(filename,c,valex)
  implicit none
  character(len=*), intent(in) :: filename
  real, intent(in) :: valex
  integer, intent(in)  :: c(:,:,:)
  real,allocatable :: d(:,:,:)

  allocate(d(size(c,1),size(c,2),size(c,3)))
  d=float(c)
  call usave_generic(filename,d,valex,3,shape(c),.false.)
  deallocate(d)

 end subroutine usave_Integer3D

 !____________________________________________________________________
 !

 subroutine uexplo(c,ndim,vshape)
  implicit none
  real, intent(inout) :: c(*)
  integer, intent(in) :: ndim
  integer, intent(in) :: vshape(ndim)

  real :: coeff(ndim+1)
  integer :: l,lentot,d,ind(ndim),cummul(ndim)

  coeff = c(1:ndim+1)

  lentot = product(vshape)

  cummul(1) = 1
  do d=1,ndim-1
    cummul(d+1) = cummul(d)*vshape(d)
  end do

  do l=1,lentot
    ! ind  = zero-based indexes

    ! transform l -> ind

    ind(1) = l-1
    do d=ndim,2,-1
      ind(d) = ind(1)/cummul(d)
      ind(1) = ind(1)-ind(d)*cummul(d)
    end do

    c(l) = coeff(1)
    do d=1,ndim
      c(l) = c(l) + (ind(d)+1)*coeff(d+1)
    end do
  end do

  !    write(stdout,*) 'uexplo'
  !    call usave_generic('test.uexplo',c,9999.,ndim,vshape,.false.)
 end subroutine uexplo

end module ufileformat
