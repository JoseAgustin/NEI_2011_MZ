!
!   lee_wrfinput.f90
!   
!
!   Created by Agustin Garcia on 26/04/18.
!   Copyright 2018 Universidad Nacional Autonoma de Mexico. All rights reserved.
!
!   Lee archivos netcdf de wrfinput
!     26 abril para RADM
!
subroutine lee_wrfinput
use netcdf
use var_nei
implicit none
  integer i,j
  integer ncid,lat_varid,lon_varid,btDimID
  integer land_catID,dimlon,dimlat
  integer latVarId,lonVarId
  integer,dimension(6) ::dim
  character (len=20 ) :: name
  character (len = *), parameter :: LAT_NAME = "XLAT"
  character (len = *), parameter :: LON_NAME = "XLONG"
  character (len = *), parameter :: REC_NAME = "Times"
  print *," "
  print *,'****    Start reading wrfinput file'
  call check(nf90_open("wrfinput", NF90_NOWRITE, ncid))
  call check(nf90_inq_dimid(ncid, "south_north", lat_varid))
  call check(nf90_inq_dimid(ncid, "west_east", lon_varid))
  if (nf90_inq_dimid(ncid, "bottom_top",btDimID).eq.nf90_noerr)then
  print *,'   Dimension bottom_top'
  else
  print *,"   NO dimension bottom_top"
  end if
  if(nf90_inq_dimid(ncid,"land_cat",land_catID).eq.nf90_noerr) then
  print *,'   Dimension land_cat'
  else
  call check(nf90_inq_dimid(ncid,"land_cat_stag",land_catID))
  print *,"   Dimension land_cat_stag"
  end if
  ! Dimensiones
  call check(nf90_inquire_dimension(ncid, lon_varid,name,dimlon))
  !print *,dimlon,name
  call check(nf90_inquire_dimension(ncid, lat_varid,name,dimlat))
  !print *,dimlat,name

  if(.not.ALLOCATED(XLON)) allocate (XLON(dimlon ,dimlat,1))
  if(.not.ALLOCATED(XLAT)) allocate (XLAT(dimlon ,dimlat,1))

  if(nf90_inq_varid(ncid, "XLAT_M", latVarId).eq. nf90_noerr) then
  print *,"XLAT_M"
  else
  call check(nf90_inq_varid(ncid, "XLAT", latVarId))
  print *,"XLAT"
  end if
  if(nf90_inq_varid(ncid, "XLONG_M", lonVarId).eq. nf90_noerr) then
  print *,"XLONG_M"
  else
  call check(nf90_inq_varid(ncid, "XLONG", lonVarId))
  print *,"XLONG"
  end if
  call check(nf90_get_var(ncid, latVarId,xlat,start=(/1,1,1/),count=(/dimlon,dimlat,1/)))
  call check(nf90_get_var(ncid, lonVarId,xlon,start=(/1,1,1/),count=(/dimlon,dimlat,1/)))
  print *,'  Reading Global Attribiutes'
  call check( nf90_get_att(ncid, nf90_global, "DX", dx))
  call check( nf90_get_att(ncid, nf90_global, "DY", dy))
  call check( nf90_get_att(ncid, nf90_global, "CEN_LAT",cenlat))
  call check( nf90_get_att(ncid, nf90_global, "CEN_LON",cenlon))
  call check( nf90_get_att(ncid, nf90_global, "TRUELAT1",trulat1))
  call check( nf90_get_att(ncid, nf90_global, "TRUELAT2",trulat2))
  call check( nf90_get_att(ncid, nf90_global, "MOAD_CEN_LAT",moadcenlat))
  call check( nf90_get_att(ncid, nf90_global, "STAND_LON",stdlon))
  call check( nf90_get_att(ncid, nf90_global, "POLE_LAT",pollat))
  call check( nf90_get_att(ncid, nf90_global, "POLE_LON",pollon))
  call check( nf90_get_att(ncid, nf90_global, "GMT",gmt))
  call check( nf90_get_att(ncid, nf90_global, "JULYR",julyr))
  call check( nf90_get_att(ncid, nf90_global, "JULDAY",julday))
  call check( nf90_get_att(ncid, nf90_global, "MAP_PROJ",mapproj))
  call check( nf90_get_att(ncid, nf90_global, "MAP_PROJ_CHAR",map_proj_char))
  call check( nf90_get_att(ncid, nf90_global, "MMINLU",mminlu))
  call check( nf90_get_att(ncid, nf90_global, "ISWATER",iswater))
  call check( nf90_get_att(ncid, nf90_global, "ISLAKE",islake))
  call check( nf90_get_att(ncid, nf90_global, "ISICE",isice))
  call check( nf90_get_att(ncid, nf90_global, "ISURBAN",isurban))
  call check( nf90_get_att(ncid, nf90_global, "ISOILWATER",isoilwater))
  call check( nf90_get_att(ncid, nf90_global, "GRID_ID",grid_id))
  call check( nf90_get_att(ncid, nf90_global, "NUM_LAND_CAT",num_land_cat))
  call check( nf90_get_att(ncid, nf90_global, "START_DATE",current_date))
  !print *,XLAT(1,1,1),XLAT(1,2,1),XLAT(1,3,1)
  !print *,XLON(1,1,1),XLON(2,1,1),XLON(3,1,1)
  call check( nf90_close(ncid) )
  print * ,'* Done reading wrfinput file'

end subroutine lee_wrfinput
