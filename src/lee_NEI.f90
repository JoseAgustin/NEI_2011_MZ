!
!   lee_NEI.f90
!   
!
!   Created by Agustin Garcia on 25/04/18.
!   Copyright 2018 Universidad Nacional Autonoma de Mexico. All rights reserved.
!
!   Lee archivos binarios del NEI2011
!     25 abril para RADM
!     28 abril 2018 MOZART

subroutine lee_NEI
use var_nei
implicit none
  integer, parameter:: IX=159
  integer, parameter:: JX=159
  integer, parameter:: KX=8
  integer, parameter:: IHOUR=12
  integer :: HR,ii,i,j,k,n
  character(len=17) :: nfile
  nfile="wrfem_12to24z_d01"
  if(hh.eq.00) then
   nfile="wrfem_00to12z_d01"
   print *,"    ***************  "
   print *,"    ** Convierte **  "
   print *,"    ***************  "
  end if
  print *," "
  print *," ***** ",nfile," *****"
  OPEN(19,FILE=nfile,FORM='UNFORMATTED',CONVERT="BIG_ENDIAN")
  read(19) NRADM
  if (.not. allocated(ename1)) allocate(ename1(NRADM))
  if (.not. allocated(ename)) allocate(ename(NRADM+1))
  if (.not. allocated(EMISS3D)) allocate(EMISS3D(IX,KX,JX,NRADM+1,IHOUR))
  read(19) ename1
  print *, nradm
    ename(NRADM+1)="E_NO2"
  do n=1,NRADM
    ename(n)=ename1(n)
    if(ename1(n).eq."E_CH3COCH") ename(n)="E_CH3COCH3"
  end do
  print *,ename
  do ii=1,IHOUR
    read(19) HR
    !print *,HR
    do n=1,NRADM
    read(19) (((EMISS3D(i,k,j,n,ii),i=1,ix),k=1,kx),j=1,jx)
    !if(hr.eq.12) print *,ename(n)!
    if( ename(n).eq."E_NO") then
        do i=1,ix
            do j=1,jx
                do k=1,kx
            EMISS3D(i,k,j,NRADM+1,ii)=EMISS3D(i,k,j,n,ii)*0.1
            EMISS3D(i,k,j,n,ii)=EMISS3D(i,k,j,n,ii)*0.9
                end do
            end do
        end do
    end if
    end do
  end do !IHOUR
  close (19)

end subroutine lee_NEI
