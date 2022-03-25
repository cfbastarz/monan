program combine_topo

  use netcdf

  implicit none

  integer, parameter :: im0 = 401
  integer, parameter :: lm = 1
  integer, parameter :: nm = 6

  integer, parameter :: im=im0, jm=im

  integer :: list = 10

  real, dimension(0:im+1,0:jm+1,nm) :: hgt = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: sm = 1.0e+22

  integer :: ncid, status
  integer :: im_dimid, jm_dimid, nm_dimid, lm_dimid
  integer :: hgt_varid
  integer :: sm_varid

  print *, trim(nf90_inq_libvers())

  status = nf90_create(path = 'cube_topo.nc', cmode = NF90_CLOBBER, ncid = ncid); call check(status)

  status = nf90_def_dim(ncid, "im", im+2, im_dimid); call check(status)
  status = nf90_def_dim(ncid, "jm", jm+2, jm_dimid); call check(status)
  status = nf90_def_dim(ncid, "nm", nm, nm_dimid); call check(status)
  status = nf90_def_dim(ncid, "lm", lm, lm_dimid); call check(status)

  status = nf90_put_att(ncid, NF90_GLOBAL, "fcstime", 0); call check(status)

  status = nf90_def_var(ncid, 'hgt', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/), hgt_varid); call check(status)
  status = nf90_def_var(ncid, 'sm',  NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/), sm_varid); call check(status)

  status = nf90_enddef(ncid); call check(status)

  open(unit=list, file='topo.dat', form='unformatted', status='old', action='read') 
  read(list) hgt,sm
  close(unit=list)

  status = nf90_put_var(ncid, hgt_varid, values=hgt, start=(/1,1,1/),   count=(/im+2,jm+2,nm/)    ); call check(status)
  status = nf90_put_var(ncid,  sm_varid, values=sm , start=(/1,1,1/),   count=(/im+2,jm+2,nm/)    ); call check(status)

  status = nf90_close(ncid); call check(status)

contains

  subroutine check(status)
    use netcdf
    implicit none
    integer, intent (in) :: status

    if (status /= nf90_noerr) then
      write(0,*) trim(nf90_strerror(status))
      stop "stopped"
    end if
  end subroutine check
end program combine_topo
