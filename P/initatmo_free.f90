SUBROUTINE INITATMO_FREE()
! ----------------------------------------------------------------------
use TIMCOM_GENERAL 
use OCN_PARA
use INPUT

use interfext, interfinit => init, interfgetfield => getfield
use initfile 

if (windtype.eq.1) then
  deallocate(MomentfluxX%array)
  deallocate(MomentfluxY%array)
end if

if (heattype.eq.0.or.heattype.eq.3) then
  deallocate(DewTemperature2m%array)
elseif (heattype.eq.1) then
  deallocate(RelHum2m%array)
elseif (heattype.eq.2) then
  deallocate(shortwavedown%array)
  deallocate(sensibleup%array)
  deallocate(latentup%array)
  deallocate(Qup%array)
end if  

if (salttype.eq.0) then
  deallocate(EvapField%array)
end if

if (raintype.eq.1) then
  deallocate(Precip%array)
end if

if (bulk) then
  deallocate(WindU10m%array)
  deallocate(WindV10m%array)
  deallocate(AirTemperature2m%array)
  if (heattype.lt.3)   deallocate(CloudCoverage%array)
  deallocate(PresMSL.array)
  if (heattype.eq.-1.or.heattype.eq.2) deallocate(RelHum2m%array)
  if (heattype.eq.2) then
    deallocate(DewTemperature2m%array)
    deallocate(DewTemperature2m%day)
  end if
end if

END SUBROUTINE INITATMO_FREE
