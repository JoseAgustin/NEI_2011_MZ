!
!   guarda.f90
!   
!
!   Created by Agustin Garcia on 26/04/18.
!   Copyright 2018 Universidad Nacional Autonoma de Mexico. All rights reserved.
!
!   Lee archivos binarios del NEI2011 y genera archivo Netcdf de emisiones
!     26 abril 2018 para RADM
!     28 abril 2018 para MOZART
!****************************************************************************
!  Proposito:
!            Guarda los datos del inventario interpolado para el
!            mecanismo RADM2 en formato netcdf
!***************************************************************************
subroutine guarda_emisiones
  use netcdf
  use var_nei
  implicit none
  integer :: i,j,l
  integer :: ncid
  integer :: id_varlong,id_varlat,id_unlimit
  integer :: periodo,it,ikk,id,iu,iit,eit
  integer,dimension(NDIMS):: dim,id_dim
  integer,dimension(nradm+1):: id_var
  integer :: dimids2(2),dimids3(3),dimids4(4)
  real,ALLOCATABLE :: ea(:,:,:,:)
  character (len=20) :: FILE_NAME
  character(8)  :: date
  character(10) :: time
  character(19) :: hoy
  print *,"Guarda Archivo"
  ! ******************************************************************
  call date_and_time(date,time)
  write(hoy,'(A8,x,A10)')date,time
  print *,hoy
  cday="weekday"
  IF(hh.EQ. 0) THEN
  print *,'PERIODO 1'
  FILE_NAME='wrfchemi_00z_d01'         !******
  TITLE="NEI_2011 Emissions for 0 to 11z"
  PERIODO=1
  iit= 0
  eit=11
  write(current_date(12:13),'(2A)')"00"
  else
  Print *,'PERIODO 2'
  FILE_NAME='wrfchemi_12z_d01'         !******
  TITLE="NEI_2011 Emissions for 12 to 23z"
  PERIODO=2
  iit=12
  eit=23
  write(current_date(12:13),'(2A)')"12"
  end if
  write(FILE_NAME(16:16),'(I1)')grid_id
  ! Open NETCDF emissions file
  call check( nf90_create(FILE_NAME, nf90_clobber, ncid) )
  !     Define dimensiones
  dim(1)=1
  dim(2)=19
  dim(3)=SIZE(EMISS3D,DIM=1)
  dim(4)=SIZE(EMISS3D,DIM=3)
  dim(5)=1!mkx
  dim(6)=SIZE(EMISS3D,DIM=2) ! VERTICAL DATA
  if(.not.ALLOCATED(ea)) allocate (ea(dim(3),dim(4),dim(6),dim(1)))
  call check( nf90_def_dim(ncid,sdim(1), NF90_UNLIMITED, id_dim(1)) )
  do i=2,NDIMS
  call check( nf90_def_dim(ncid, sdim(i), dim(i), id_dim(i)) )
  end do

  dimids2 = (/id_dim(2),id_dim(1)/)
  dimids3 = (/id_dim(3),id_dim(2),id_dim(1) /)
  dimids4 = (/id_dim(3),id_dim(4),id_dim(6),id_dim(1)/)

  !Attributos Globales NF90_GLOBAL
  call check( nf90_put_att(ncid, NF90_GLOBAL, "TITLE",TITLE))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "START_DATE",current_date))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "DAY ",cday))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "SIMULATION_START_DATE",current_date))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "WEST-EAST_GRID_DIMENSION",dim(3)))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "SOUTH-NORTH_GRID_DIMENSION",dim(4)))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "BOTTOM-TOP_GRID_DIMENSION",1))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "DX",dx))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "DY",dy))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LAT",cenlat))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LON",cenlon))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT1",trulat1))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT2",trulat2))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "MOAD_CEN_LAT",moadcenlat))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "STAND_LON",stdlon))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LAT",pollat))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LON",pollon))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "GMT",gmt))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "JULYR",julyr))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "JULDAY",julday))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ",mapproj))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ_CHAR",map_proj_char))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "MMINLU",mminlu))
  call check( nf90_put_att(ncid, nf90_global, "ISWATER",iswater))
  call check( nf90_put_att(ncid, nf90_global, "ISLAKE",islake))
  call check( nf90_put_att(ncid, nf90_global, "ISICE",isice))
  call check( nf90_put_att(ncid, nf90_global, "ISURBAN",isurban))
  call check( nf90_put_att(ncid, nf90_global, "NUM_LAND_CAT",num_land_cat))
  call check( nf90_put_att(ncid, nf90_global, "ISOILWATER",isoilwater))
  call check( nf90_put_att(ncid, nf90_global, "GRID_ID",grid_id))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "MECHANISM","MOZART"))
  call check( nf90_put_att(ncid, NF90_GLOBAL, "CREATION_DATE",hoy))
  !  Define las variables
  call check( nf90_def_var(ncid, "Times", NF90_CHAR, dimids2,id_unlimit ) )
  !  Attributos para cada variable
  call check( nf90_def_var(ncid, "XLONG", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlong ) )
  ! Assign  attributes
  call check( nf90_put_att(ncid, id_varlong, "FieldType", 104 ) )
  call check( nf90_put_att(ncid, id_varlong, "MemoryOrder", "XYZ") )
  call check( nf90_put_att(ncid, id_varlong, "description", "LONGITUDE, WEST IS NEGATIVE") )
  call check( nf90_put_att(ncid, id_varlong, "units", "degree_east"))
  call check( nf90_put_att(ncid, id_varlong, "axis", "X") )
  call check( nf90_def_var(ncid, "XLAT", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlat ) )
  ! Assign  attributes
  call check( nf90_put_att(ncid, id_varlat, "FieldType", 104 ) )
  call check( nf90_put_att(ncid, id_varlat, "MemoryOrder", "XYZ") )
  call check( nf90_put_att(ncid, id_varlat, "description", "LATITUDE, SOUTH IS NEGATIVE") )
  call check( nf90_put_att(ncid, id_varlat, "units", "degree_north"))
  call check( nf90_put_att(ncid, id_varlat, "axis", "Y") )

  do i=1,radm
  if(i.lt.29 .or.i.gt.41) then
  call crea_attr(ncid,4,dimids4,ename(i),cname(i),id_var(i))
  else
  call crea_attr2(ncid,4,dimids4,ename(i),cname(i),id_var(i))
  end if
  end do
  !
  !   Terminan definiciones
  call check( nf90_enddef(ncid) )
  !    Inicia loop de tiempo
  tiempo: do it=iit,eit
    write(6,'(A,x,I3)')'TIEMPO: ', it
    gases: do ikk=1,radm
      ea=0.0
      if(ikk.eq.1) then
        if (it.lt.10) then
          write(current_date(13:13),'(A1)')char(it+48)
        else
          id = int((it)/10)+48 !  Decenas
          iu = it-10*int((it)/10)+48 ! unidades
          write(current_date(12:13),'(A1,A1)')char(id),char(iu)
        end if
        write(current_date(1:4),'(I4)') julyr
        Times(1,1)=current_date(1:19)
        if (periodo.eq. 1) then
          call check( nf90_put_var(ncid, id_unlimit,Times,start=(/1,it+1/)) )
          call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1,it+1/)) )
          call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1,it+1/)) )
        else
          call check( nf90_put_var(ncid, id_unlimit,Times,start=(/1,it-11/)) )
          call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1,it-11/)) )
          call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1,it-11/)) )
        endif
      end if   ! for kk == 1
      do i=1, dim(3)
        do j=1, dim(4)
          do l=1,dim(6)
            if(periodo.eq.1) then
              ea(i,j,l,1)=EMISS3D(i,l,j,ikk,it+1)
            else
              ea(i,j,l,1)=EMISS3D(i,l,j,ikk,it-11)
            endif
          end do
        end do
      end do
      if(periodo.eq.1) then
      call check( nf90_put_var(ncid, id_var(ikk),ea,start=(/1,1,1,it+1/)) )
      else
      call check( nf90_put_var(ncid, id_var(ikk),ea,start=(/1,1,1,it-11/)) )        !******
      endif
    end do gases
  end do tiempo
  call check( nf90_close(ncid) )
  if(periodo.eq.2) deallocate(ea,EMISS3D,ename,xlat,xlon)
contains

!  CCCC RRRR  EEEEE  AAA      AAA  TTTTT TTTTT RRRR
! CC    R  RR E     A   A    A   A   T     T   R  RR
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR
! CC    R  R  E     A   A    A   A   T     T   R  R
!  CCCC R   R EEEEE A   A____A   A   T     T   R   R
subroutine crea_attr(ncid,idm,dimids,svar,cname,id_var)
  implicit none
  integer , INTENT(IN) ::ncid,idm
  integer, INTENT(out) :: id_var
  integer, INTENT(IN),dimension(idm):: dimids
  character(len=*), INTENT(IN)::svar,cname
  character(len=50) :: cvar
  cvar=trim(cname)//" emission rate"

  call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
  ! Assign  attributes
  call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
  call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
  call check( nf90_put_att(ncid, id_var, "description", Cvar) )
  call check( nf90_put_att(ncid, id_var, "units", "mol km^-2 hr^-1"))
  call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
  call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
  ! print *,"Entro a Attributos de variable",dimids,id,jd
  return
end subroutine crea_attr
!  CCCC RRRR  EEEEE  AAA      AAA  TTTTT TTTTT RRRR   222
! CC    R  RR E     A   A    A   A   T     T   R  RR 2   2
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR     2
! CC    R  R  E     A   A    A   A   T     T   R  R   2
!  CCCC R   R EEEEE A   A____A   A   T     T   R   R 22222
subroutine crea_attr2(ncid,idm,dimids,svar,cname,id_var)
  implicit none
  integer, INTENT(IN) ::ncid,idm
  integer, INTENT(out) :: id_var
  integer,INTENT(IN) ,dimension(idm):: dimids
  character(len=*),INTENT(IN) ::svar,cname
  character(len=50) :: cvar
  cvar=trim(cname)//" emission rate"
  call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
  ! Assign  attributes
  call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
  call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
  call check( nf90_put_att(ncid, id_var, "description",cvar) )
  call check( nf90_put_att(ncid, id_var, "units", "ug m-2 s-1"))
  call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
  call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
  ! print *,"Entro a Attributos de variable",dimids,id,jd
return
end subroutine crea_attr2
end subroutine guarda_emisiones
