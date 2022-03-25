program combine_netcdf

  use netcdf

  implicit none


  integer, parameter :: im0 = 401
  integer, parameter :: lm = 38
  integer, parameter :: nm = 6

  integer, parameter :: im=im0, jm=im
  integer, parameter :: nsub = 10

  integer, parameter :: ixm = nsub
  integer, parameter :: jym = ixm
  integer, parameter :: ilm = (im - 1)/ixm +1
  integer, parameter :: jlm = (jm - 1)/jym +1
  integer, parameter :: nxy = ixm*jym
  integer, parameter :: ildom = (im - 1)/ixm
  integer, parameter :: jldom = (jm - 1)/jym

  integer, parameter :: npes=nsub*nsub*nm

  integer :: mype,n,mype0,jy,ix,imaxV,imaxH,imin,jmaxV,jmaxH,jmin
  integer :: list = 10
  integer :: ihr
  character(len=64) :: filename

  real, dimension(0:ilm+1,0:jlm+1) :: dum2
  real, dimension(0:ilm+1,0:jlm+1,lm) :: dum3
  real, dimension(0:im+1,0:jm+1,nm) :: pd = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm,lm) :: t = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm,lm) :: q = 1.0e+22

  integer :: ncid, status
  integer :: im_dimid, jm_dimid, nm_dimid, lm_dimid
  integer :: pd_varid
  integer :: t_varid
  integer :: q_varid

  print *, trim(nf90_inq_libvers())

  write(filename,'(a,i3.3,a)') 'cube_vrbls.nc'

  status = nf90_create(path = trim(filename), cmode = NF90_CLOBBER, ncid = ncid); call check(status)

  status = nf90_def_dim(ncid, "im", im+2, im_dimid); call check(status)
  status = nf90_def_dim(ncid, "jm", jm+2, jm_dimid); call check(status)
  status = nf90_def_dim(ncid, "nm", nm, nm_dimid); call check(status)
  status = nf90_def_dim(ncid, "lm", lm, lm_dimid); call check(status)

  status = nf90_put_att(ncid, NF90_GLOBAL, "fcstime", ihr); call check(status)

  status = nf90_def_var(ncid, 'pd', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),          pd_varid); call check(status)
  status = nf90_def_var(ncid,  't', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid,lm_dimid/),  t_varid); call check(status)
  status = nf90_def_var(ncid,  'q', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid,lm_dimid/),  q_varid); call check(status)

  status = nf90_enddef(ncid); call check(status)

  do mype = 0, npes-1

    n = mype/nxy + 1
    mype0 = mype - (n-1)*nxy
    jy = (mype0/ixm) + 1
    ix = mod(mype0, ixm)+1

    imaxV = ix * ildom
    imaxH = imaxV + 1
    imin  = imaxH - ildom
    jmaxV = jy * jldom
    jmaxH = jmaxV + 1
    jmin  = jmaxH - jldom

    write(filename,'(a,i4.4,a,i4.4)') 'vrbls01.',mype
    open(unit=list, file=filename, form='unformatted', status='old', action='read') 

    read(list) dum2    ! pd
    pd(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum3    ! t
    t(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)
    read(list) dum3    ! q
    q(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)

    close(unit=list)

  end do

  status = nf90_put_var(ncid, pd_varid, values=pd, start=(/1,1,1/),   count=(/im+2,jm+2,nm/)    ); call check(status)
  status = nf90_put_var(ncid,  t_varid, values=t , start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
  status = nf90_put_var(ncid,  q_varid, values=q , start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)

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
end program combine_netcdf
