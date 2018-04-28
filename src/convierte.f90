!
!   convierte.f90
!   
!
!   Created by Agustin Garcia on 25/04/18.
!   Copyright 2018 Universidad Nacional Autonoma de Mexico. All rights reserved.
!
!   Lee archivos binarios del NEI2011 y genera archivo Netcdf de emisiones
!     25 abril para RADM
!

program nei_2011
use var_nei

    hh=0
    call lee_NEI

    call lee_wrfinput

    call guarda_emisiones
    hh=12
    call lee_NEI

    call guarda_emisiones

end program nei_2011
