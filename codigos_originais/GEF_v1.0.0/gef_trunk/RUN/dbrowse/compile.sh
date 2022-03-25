#!/bin/sh
set -aeux

NETCDF=/apps/netcdf/4.1.3-intel

ifort -g -traceback -I$NETCDF/include combine_topo.f90       -o combine_topo       $NETCDF/lib/libnetcdff.a $NETCDF/lib/libnetcdf.a /apps/hdf5/1.8.8-intel/lib/libhdf5_hl.a /apps/hdf5/1.8.8-intel/lib/libhdf5.a -lz
ifort -g -traceback -I$NETCDF/include combine_out_netcdf.f90 -o combine_out_netcdf $NETCDF/lib/libnetcdff.a $NETCDF/lib/libnetcdf.a /apps/hdf5/1.8.8-intel/lib/libhdf5_hl.a /apps/hdf5/1.8.8-intel/lib/libhdf5.a -lz
ifort -g -traceback -I$NETCDF/include combine_in_netcdf.f90  -o combine_in_netcdf  $NETCDF/lib/libnetcdff.a $NETCDF/lib/libnetcdf.a /apps/hdf5/1.8.8-intel/lib/libhdf5_hl.a /apps/hdf5/1.8.8-intel/lib/libhdf5.a -lz
