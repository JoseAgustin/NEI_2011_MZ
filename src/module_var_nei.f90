!
!   module_var_nei.f90
!   
!
!   Created by Agustin Garcia on 25/04/18.
!   Copyright 2018 Universidad Nacional Autonoma de Mexico. All rights reserved.
!
module var_nei
! Emissions Inventories Variables
  integer :: zlev       ! Layer of emission (1 to 8) 8 lower 1 upper
  integer :: nh,hh,NRADM
  parameter(nh=24,radm=39)
  integer :: NDIMS
  parameter (NDIMS=6)
  real,allocatable:: EMISS3D(:,:,:,:,:) ! emissions by nx,ny,level,nh,radm
  real,allocatable:: dlat(:,:),dlon(:,:)     ! by nx,ny from NEW DOMAIN
  real,allocatable ::xlon(:,:,:),xlat(:,:,:)! by nx,ny,nh emissions
  integer:: grid_id
  integer:: julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater
  real :: cenlat,cenlon, dx,dy
  real :: trulat1, trulat2,moadcenlat,stdlon,pollat,pollon
  real :: gmt,num_land_cat
  character(len=3) :: cday
  CHARACTER (len= 9), allocatable :: ENAME1(:)
  CHARACTER (len=10), allocatable :: ENAME(:)
  character(len=19)::mminlu,map_proj_char
  character(len=19):: iTime
  character(len=38):: Title
  character(len=19),dimension(1,1)::Times
  character (len=19),dimension(NDIMS) ::sdim=(/"Time               ",&
  & "DateStrLen         ","west_east          ","south_north        ",&
  &"bottom_top         ","emissions_zdim_stag"/)
  character(len= 19),dimension(radm):: cname=(/'Benzaldehyde','Methanol',&
  'Propylene','Ethanol','Sulfur Dioxide','Nitrogen Oxide','Acetaldehyde',&
  'Formaldehyde','Formic Acid','Acetylene','Cresol','Ammonia','Propane',&
  'lumped alkanes C>3','Methyl ethyl ket','Methyl vinyl ket','Phenol',&
  'Ethane','Carbon Monoxide','Ethene','Lumped alkenes C>3',&
  'Methyl glyoxal','Toluene','m-/o-/p-xylenes','Acetone','Benzene',&
  'Isoprene','Methane','PM25I','PM25J',&
  'SulfatesI','SulfatesJ','Nitrates I','Nitrates J','OrganicI','OrganicJ',&
  'Elemental C I ','Elemental C J','PM_10'/)
  character (len=19) :: current_date,mecha

  ! Domain Variables
  common /domain/ NRADM,zlev,dx,dy,Title,sdim
  common /date/ hh,id_grid,current_date,cday,mecha,cname,Times
  common /wrf/ julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater,&
  cenlat,cenlon,trulat1, trulat2,moadcenlat,stdlon,pollat,pollon,num_land_cat,&
  gmt,mminlu,map_proj_char

contains
!
!  CCCC  H   H  EEEEE   CCCC  K   K
! CC     H   H  E      CC     K K
! C      HHHHH  EEE   C       KK
! CC     H   H  E      CC     K K
!  CCCC  H   H  EEEEE   CCCC  K   K

subroutine check(status)
  USE netcdf
    integer, intent ( in) :: status
    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop 2
    end if
end subroutine check
end module var_nei
