# for pgi

FC=pgfortran
FCMPI=pgfortran -I/sw/rhel6-x64/intel/impi/2017.1.132/compilers_and_libraries/linux/mpi/intel64/include/pgfortran/16.9 -I/sw/rhel6-x64/intel/impi/2017.1.132/compilers_and_libraries/linux/mpi/intel64/include -L/sw/rhel6-x64/intel/impi/2017.1.132/compilers_and_libraries/linux/mpi/intel64/lib -Wl,-rpath,/sw/rhel6-x64/intel/impi/2017.1.132/compilers_and_libraries/linux/mpi/intel64/lib -lmpifort -lmpi -lmpigi -lmpipgf
# FFLAGS=-O3 -g -r8
FFLAGS=-c -Kieee -g  -O0 -Mchkstk -Mchkfpstk -Mchkptr -mcmodel=medium -Mbounds
# -mcmodel=medium was added after suggestion by Dragan on Tue, Jan 8, 2019 13:41 h
# -Mbounds was added after suggestion by Lucci on Mon, Jan 14, 2019 18:10 h
### <-Ktrap=fp> was suggested to be off - suggestion by Lucci on Wed, Jan 30, 2019 17:38 h

# MYLIBPATH=/work/k20200/k202082/usersupport/m300707/gef_trunk/lib
# BACIO_LIB=${MYLIBPATH}/libbacio_4.a
# W3_LIB=${MYLIBPATH}/libw3emc_4.a ${MYLIBPATH}/libw3nco_4.a

# for intel

#FC=ifort
#FCMPI=mpif90
#FFLAGS=-O0 -g -traceback -check all -fpe0 -fp-model strict
# <-fp-model strict> is a way to treat floating-point operations.

MYLIBPATH=/pf/b/b380797/GEF-reta/GEF_tempo/gef_trunk/LIB_NEW
BACIO_LIB=${MYLIBPATH}/libbacio_4.a
W3_LIB=${MYLIBPATH}/libw3emc_4.a ${MYLIBPATH}/libw3nco_4.a
