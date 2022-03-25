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
  real, dimension(0:im+1,0:jm+1,nm,lm) :: cwm = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: cfracl = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: cfracm = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: cfrach = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: cnvtop = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: cnvbot = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: htop = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: hbot = 1.0e+22

  real, dimension(0:im+1,0:jm+1,nm) :: RSWIN = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: RSWOUT = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: RSWTOA = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: RLWIN = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: RLWOUT = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm) :: RLWTOA = 1.0e+22

  integer :: ncid, status
  integer :: im_dimid, jm_dimid, nm_dimid, lm_dimid
  integer :: pd_varid
  integer :: t_varid
  integer :: q_varid
  integer :: cwm_varid
  integer :: cfracl_varid
  integer :: cfracm_varid
  integer :: cfrach_varid
  integer :: cnvtop_varid
  integer :: cnvbot_varid
  integer :: htop_varid
  integer :: hbot_varid
  integer :: rswin_varid
  integer :: rswout_varid
  integer :: rswtoa_varid
  integer :: rlwin_varid
  integer :: rlwout_varid
  integer :: rlwtoa_varid


  print *, trim(nf90_inq_libvers())

  do ihr = 0, 48, 24
  write(filename,'(a,i3.3,a)') 'cube_',ihr,'.nc'

  status = nf90_create(path = trim(filename), cmode = NF90_CLOBBER, ncid = ncid); call check(status)

  status = nf90_def_dim(ncid, "im", im+2, im_dimid); call check(status)
  status = nf90_def_dim(ncid, "jm", jm+2, jm_dimid); call check(status)
  status = nf90_def_dim(ncid, "nm", nm, nm_dimid); call check(status)
  status = nf90_def_dim(ncid, "lm", lm, lm_dimid); call check(status)

  status = nf90_put_att(ncid, NF90_GLOBAL, "fcstime", ihr); call check(status)

  status = nf90_def_var(ncid, 'pd', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),          pd_varid); call check(status)
  status = nf90_def_var(ncid,  't', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid,lm_dimid/),  t_varid); call check(status)
!  status = nf90_def_var(ncid,  'q', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid,lm_dimid/),  q_varid); call check(status)
  status = nf90_def_var(ncid,  'cwm', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid,lm_dimid/),  cwm_varid); call check(status)

!  status = nf90_def_var(ncid,  'cfracl', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  cfracl_varid); call check(status)
  status = nf90_def_var(ncid,  'cfracm', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  cfracm_varid); call check(status)
!  status = nf90_def_var(ncid,  'cfrach', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  cfrach_varid); call check(status)
  status = nf90_def_var(ncid,  'cnvtop', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  cnvtop_varid); call check(status)
  status = nf90_def_var(ncid,  'cnvbot', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  cnvbot_varid); call check(status)
  status = nf90_def_var(ncid,  'htop', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  htop_varid); call check(status)
  status = nf90_def_var(ncid,  'hbot', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  hbot_varid); call check(status)

  status = nf90_def_var(ncid,  'rswin',  NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  rswin_varid); call check(status)
!  status = nf90_def_var(ncid,  'rswout', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  rswout_varid); call check(status)
!  status = nf90_def_var(ncid,  'rswtoa', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  rswtoa_varid); call check(status)
!  status = nf90_def_var(ncid,  'rlwin',  NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  rlwin_varid); call check(status)
!  status = nf90_def_var(ncid,  'rlwout', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  rlwout_varid); call check(status)
!  status = nf90_def_var(ncid,  'rlwtoa', NF90_FLOAT, (/im_dimid,jm_dimid,nm_dimid/),  rlwtoa_varid); call check(status)

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

    write(filename,'(a,i4.4,a,i4.4)') 'GEFfcst_',mype,'.',ihr
    open(unit=list, file=filename, form='unformatted', status='old', action='read') 

    read(list) dum2    ! pd
    pd(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum3    ! t
    t(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)
    read(list) dum3    ! u
    read(list) dum3    ! v
    read(list) dum3    ! q
    q(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)
    read(list) dum3    ! div
    read(list) dum3    ! cwm
    cwm(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)


    close(unit=list)

    write(filename,'(a,i4.4,a,i4.4)') 'GEF_clouds',mype,'.',ihr
    open(unit=list, file=filename, form='unformatted', status='old', action='read')
    read(list) dum2    ! CUPPT
    read(list) dum2    ! cfracl
    cfracl(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2    ! cfracm
    cfracm(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2    ! cfrach
    cfrach(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2    ! acfrcv
    read(list) dum2    ! acfrst
    read(list) dum2    ! cnvtop
    cnvtop(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2    ! cnvbot
    cnvbot(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2    ! htop
    htop(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2    ! hbot
    hbot(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    close(unit=list)

    write(filename,'(a,i4.4,a,i4.4)') 'GEF_sw_rdtn',mype,'.',ihr
    open(unit=list, file=filename, form='unformatted', status='old', action='read')
    read(list) dum2 ; rswin(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2 ; rswout(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    read(list) dum2 ; rswtoa(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
    close(unit=list)

!    write(filename,'(a,i4.4,a,i4.4)') 'GEF_lw_rdtn',mype,'.',ihr
!    open(unit=list, file=filename, form='unformatted', status='old', action='read')
!    read(list) dum2 ; rlwin(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
!    read(list) dum2 ; rlwout(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
!    read(list) dum2 ; rlwtoa(imin:imaxH, jmin:jmaxH,n)  = dum2(1:ilm,1:jlm)
!    close(unit=list)


  end do

  status = nf90_put_var(ncid, pd_varid, values=pd, start=(/1,1,1/),   count=(/im+2,jm+2,nm/)    ); call check(status)
  status = nf90_put_var(ncid,  t_varid, values=t , start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
!  status = nf90_put_var(ncid,  q_varid, values=q , start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
  status = nf90_put_var(ncid,  cwm_varid, values=cwm , start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)

!  status = nf90_put_var(ncid,  cfracl_varid, values=cfracl, start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
  status = nf90_put_var(ncid,  cfracm_varid, values=cfracm, start=(/1,1,1/), count=(/im+2,jm+2,nm/) ); call check(status)
!  status = nf90_put_var(ncid,  cfrach_varid, values=cfrach, start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
  status = nf90_put_var(ncid,  cnvtop_varid, values=cnvtop, start=(/1,1,1/), count=(/im+2,jm+2,nm/) ); call check(status)
  status = nf90_put_var(ncid,  cnvbot_varid, values=cnvbot, start=(/1,1,1/), count=(/im+2,jm+2,nm/) ); call check(status)
  status = nf90_put_var(ncid,  htop_varid, values=htop, start=(/1,1,1/), count=(/im+2,jm+2,nm/) ); call check(status)
  status = nf90_put_var(ncid,  hbot_varid, values=hbot, start=(/1,1,1/), count=(/im+2,jm+2,nm/) ); call check(status)

  status = nf90_put_var(ncid,  rswin_varid,  values=rswin,  start=(/1,1,1/), count=(/im+2,jm+2,nm/) ); call check(status)
!  status = nf90_put_var(ncid,  rswout_varid, values=rswout, start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
!  status = nf90_put_var(ncid,  rswtoa_varid, values=rswtoa, start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
!  status = nf90_put_var(ncid,  rlwin_varid,  values=rlwin,  start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
!  status = nf90_put_var(ncid,  rlwout_varid, values=rlwout, start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)
!  status = nf90_put_var(ncid,  rlwtoa_varid, values=rlwtoa, start=(/1,1,1,1/), count=(/im+2,jm+2,nm,lm/) ); call check(status)

  status = nf90_close(ncid); call check(status)

  end do

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
