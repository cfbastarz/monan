program combine

  implicit none

  integer, parameter :: im0 = 215
  integer, parameter :: lm = 38
  integer, parameter :: nm = 6

  integer, parameter :: im=im0, jm=im
  integer, parameter :: nsub = 2

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
  character(len=64) :: filename

  real, dimension(0:ilm+1,0:jlm+1) :: dum2
  real, dimension(0:ilm+1,0:jlm+1,lm) :: dum3
  real, dimension(0:im+1,0:jm+1,nm) :: pd = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm,lm) :: t = 1.0e+22
  real, dimension(0:im+1,0:jm+1,nm,lm) :: q = 1.0e+22

  print *, ilm, jlm
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

    write(filename,'(a,i4.4,a)') 'data_out/GEFfcst_',mype,'.0018'
    open(unit=list, file=filename, form='unformatted', status='old', action='read') 

    read(list) dum2    ! pd
    pd(imin:imaxH, jmin:jmaxH,n) = dum2(1:ilm,1:jlm)
    read(list) dum3    ! t
    t(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)
    read(list) dum3    ! u
!    t(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)
    read(list) dum3    ! v
!    t(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)
    read(list) dum3    ! q
    q(imin:imaxH, jmin:jmaxH,n,:) = dum3(1:ilm,1:jlm,:)

    close(unit=list)

  end do

  write(40)im,jm,nm,1
  write(40)pd

  write(41)im,jm,nm,lm
  write(41)t

  write(42)im,jm,nm,lm
  write(42)q

end program combine
