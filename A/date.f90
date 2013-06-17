module date
contains

function JulianDay(d,m,y)
implicit none
integer, intent(in) :: d,m,y
real :: JulianDay

JulianDay = ( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +      &
          ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -  &
          ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 + &
          d - 32075.5_8

end function

!_______________________________________________________
!

function ChronologicalJulianDay(d,m,y)
implicit none
integer, intent(in) :: d,m,y
integer :: ChronologicalJulianDay

! Mathematicians and programmers have naturally 
! interested themselves in mathematical and computational 
! algorithms to convert between Julian day numbers and 
! Gregorian dates. The following conversion algorithm is due 
! to Henry F. Fliegel and Thomas C. Van Flandern: 
! The Julian day (jd) is computed from Gregorian day, month and year (d, m, y) as follows:
! http://hermetic.magnet.ch/cal_stud/jdn.htm

ChronologicalJulianDay = ( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +      &
          ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -  &
          ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 + &
          d - 32075

end function

!_______________________________________________________
!

function ModifiedJulianDay(d,m,y)
implicit none
integer, intent(in) :: d,m,y
integer :: ModifiedJulianDay

! Mathematicians and programmers have naturally 
! interested themselves in mathematical and computational 
! algorithms to convert between Julian day numbers and 
! Gregorian dates. The following conversion algorithm is due 
! to Henry F. Fliegel and Thomas C. Van Flandern: 
! The Julian day (jd) is computed from Gregorian day, month and year (d, m, y) as follows:
! http://hermetic.magnet.ch/cal_stud/jdn.htm

! ModifiedJulianDay = 0 for 1858-11-17 CE.

ModifiedJulianDay = ( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +      &
          ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -  &
          ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 + &
          d - 32075 - 2400001
end function

!_______________________________________________________
!

subroutine GregorianDate(cjd,d,m,y)
implicit none
integer, intent(in)  :: cjd
integer, intent(out) :: d,m,y

integer              :: l,n,i,j

! Converting from the chronological Julian day number to the Gregorian 
! date is performed thus:


        l = cjd + 68569
        n = ( 4 * l ) / 146097
        l = l - ( 146097 * n + 3 ) / 4
        i = ( 4000 * ( l + 1 ) ) / 1461001
        l = l - ( 1461 * i ) / 4 + 31
        j = ( 80 * l ) / 2447
        d = l - ( 2447 * j ) / 80
        l = j / 11
        m = j + 2 - ( 12 * l )
        y = 100 * ( n - 49 ) + i + l

end subroutine


!_______________________________________________________
!

      function mjd(y,m,d,s)
      implicit none
      integer d,m,y
      real s
      real mjd

! Mathematicians and programmers have naturally
! interested themselves in mathematical and computational
! algorithms to convert between Julian day numbers and
! Gregorian dates. The following conversion algorithm is due
! to Henry F. Fliegel and Thomas C. Van Flandern:
! The Julian day (jd) is computed from Gregorian day, month and year (d, m, y) as follows:
! http://hermetic.magnet.ch/cal_stud/jdn.htm

! ModifiedJulianDay = 0 for 1858-11-17 CE.

      mjd = (( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +        &
            ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -   &
            ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 +  &
            d - 32075 - 2400001)*1d0 + s/(24*60*60d0)
      end function mjd

!_______________________________________________________
!

      function mjd2(y,m,d,h,minut)
      implicit none
      integer d,m,y,h,minut
      real mjd2

! Mathematicians and programmers have naturally 
! interested themselves in mathematical and computational 
! algorithms to convert between Julian day numbers and 
! Gregorian dates. The following conversion algorithm is due 
! to Henry F. Fliegel and Thomas C. Van Flandern: 
! The Julian day (jd) is computed from Gregorian day, month and year (d, m, y) as follows:
! http://hermetic.magnet.ch/cal_stud/jdn.htm

! ModifiedJulianDay = 0 for 1858-11-17 CE.

      mjd2 = (( 1461 * ( y + 4800 + ( m - 14 ) / 12 ) ) / 4 +        &
            ( 367 * ( m - 2 - 12 * ( ( m - 14 ) / 12 ) ) ) / 12 -   &
            ( 3 * ( ( y + 4900 + ( m - 14 ) / 12 ) / 100 ) ) / 4 +  &
            d - 32075 - 2400001)*1d0 + h/(24d0) + minut/(24*60d0)               
      end function mjd2


!_______________________________________________________
!

      subroutine gregd2(mjd,y,m,d,h,minut)
      implicit none
      real, intent(in)  :: mjd
      real :: temp1,temp2
      integer, intent(out) :: d,m,y,h,minut

      integer              :: l,n,i,j

! Converting from the modified Julian day number to the Gregorian
! date is performed thus:

!date
        l = floor(mjd) + 68569 + 2400001
        n = ( 4 * l ) / 146097
        l = l - ( 146097 * n + 3 ) / 4
        i = ( 4000 * ( l + 1 ) ) / 1461001
        l = l - ( 1461 * i ) / 4 + 31
        j = ( 80 * l ) / 2447
        d = l - ( 2447 * j ) / 80
        l = j / 11
        m = j + 2 - ( 12 * l )
        y = 100 * ( n - 49 ) + i + l

!time in the day
        temp1=mjd-floor(mjd)
        temp2=temp1*24
        h=floor(temp2)
        minut=nint((temp2-h)*60d0)

      end subroutine

!_______________________________________________________
!

      subroutine gregd(mjd,y,m,d,s)
      implicit none
      real, intent(in)  :: mjd
      integer, intent(out) :: d,m,y
      real, intent(out) :: s

      integer              :: l,n,i,j

! Converting from the chronological Julian day number to the Gregorian
! date is performed thus:

        l = floor(mjd) + 68569 + 2400001
        n = ( 4 * l ) / 146097
        l = l - ( 146097 * n + 3 ) / 4
        i = ( 4000 * ( l + 1 ) ) / 1461001
        l = l - ( 1461 * i ) / 4 + 31
        j = ( 80 * l ) / 2447
        d = l - ( 2447 * j ) / 80
        l = j / 11
        m = j + 2 - ( 12 * l )
        y = 100 * ( n - 49 ) + i + l

        s = 24*60*60*(mjd-floor(mjd))
      end subroutine



end module
