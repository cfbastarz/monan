# Passos para instalação do modelo MPAS (Model for Prediction Across Scales) com Spack

## Documento Técnico - DT - Rev. Beta 0.1                         <u>*(em preparação)*</u>

##### Autores: Roberto Pinto Souto, ...

### 1. Introdução

##### Preparação do ambiente no clusTer *minerva* da DELL

```bash
[rpsouto.incc@archer ~]$ ssh minerva
[rpsouto.incc@mlogin01 ~]$ wd
[rpsouto.incc@mlogin01 rpsouto.incc]$ module load hpc
[rpsouto.incc@mlogin01 rpsouto.incc]$ module load openmpi/gcc84/4.1.0
[rpsouto.incc@mlogin01 rpsouto.incc]$ module load python37
```

##### Instalação do gerenciador de pacotes Spack ( https://spack.io/ )

```bash
$ git clone https://github.com/spack/spack.git -b v0.17.1_minerva_mlogin
$ cd v0.17.1_minerva_mlogin
$ git checkout tags/v0.17.1 -b branch_v0.17.1
```

##### Configuração do Spack

```bash
$ wd
$ mkdir -p .spack/v0.17.1_minerva_mlogin
$ cd .spack/v0.17.1_minerva_mlogin
```

Gerar arquivo `env_spack.sh` no diretório `.spack/v0.17.1_minerva_mlogin` com este conteúdo:

```bash
#!/bin/bash

workdir=/work/${USER}
version=v0.17.1
cluster=minerva
node=mlogin
spackdir=${workdir}/spack/${version}_${cluster}_${node}

. ${spackdir}/share/spack/setup-env.sh
export SPACK_USER_CONFIG_PATH=${workdir}/.spack/${version}_${cluster}_${node}
export SPACK_USER_CACHE_PATH=${SPACK_USER_CONFIG_PATH}/tmp
export TMP=${SPACK_USER_CACHE_PATH}
export TMPDIR=${SPACK_USER_CACHE_PATH}
```

Gerar arquivo `env_minerva.sh` no diretório `.spack/v0.17.1_minerva_mlogin` com este conteúdo:

```bash
#!/bin/bash

module load python37
module load hpc
module load cmake/3.22.1
module load gcc/8.4.0
```

Carregar no ambiente as configurações do spack e dos módulos ativados

```bash
$ source env_minerva.sh
$ source env_spack.sh
```

Fazer o Spack descobrir os pacotes cmake e openmpi carregados no ambiente

```bash
$ spack external find cmake
==> The following specs have been detected on this system and added 
to /work/rpsouto.incc/.spack/v0.17.1_minerva_mlogin/packages.yaml
cmake@3.22.1
```

Arquivo packages.yaml com o pacote `cmake` descobertos pelo Spack.
Acrescentar manualmente também o pacote `perl` do próprio ambiente, necessário para instalação de algumas das bibliotecas utilizadsa pelo `mpas-model`

```bash
packages:
  cmake:
    externals:
    - spec: cmake@3.22.1
      prefix: /home/modules/hpc/cmake/3.22.1
  perl:
    externals:
    - spec: perl@5.26.3~cpanm+shared+threads
      prefix: /usr
```

Adicionando novos compilares (se forem enconrados) no Spack:

```bash
$ spack compiler find
$ spack compiler list
$ spack compiler list
==> Available compilers
-- gcc rhel8-x86_64 ---------------------------------------------
gcc@8.4.0  gcc@8.3.1
```

Mostra as dependências de `mpas-model`.

```bash
$ spack spec mpas-model%gcc@8.4.0 ^parallelio+pnetcdf
Input spec
--------------------------------
mpas-model%gcc@8.4.0
    ^parallelio+pnetcdf

Concretized
--------------------------------
mpas-model@7.1%gcc@8.4.0 arch=linux-rhel8-zen
    ^openmpi@4.1.1%gcc@8.4.0~atomics~cuda~cxx~cxx_exceptions+gpfs~internal-hwloc~java~legacylaunchers~lustre~memchecker~pmi~pmix~singularity~sqlite3+static~thread_multiple+vt+wrapper-rpath fabrics=none schedulers=none arch=linux-rhel8-zen
        ^hwloc@2.6.0%gcc@8.4.0~cairo~cuda~gl~libudev+libxml2~netloc~nvml~opencl+pci~rocm+shared arch=linux-rhel8-zen
            ^libpciaccess@0.16%gcc@8.4.0 arch=linux-rhel8-zen
                ^libtool@2.4.6%gcc@8.4.0 arch=linux-rhel8-zen
                    ^m4@1.4.19%gcc@8.4.0+sigsegv patches=9dc5fbd0d5cb1037ab1e6d0ecc74a30df218d0a94bdd5a02759a97f62daca573,bfdffa7c2eb01021d5849b36972c069693654ad826c1a20b53534009a4ec7a89 arch=linux-rhel8-zen
                        ^libsigsegv@2.13%gcc@8.4.0 arch=linux-rhel8-zen
                ^pkgconf@1.8.0%gcc@8.4.0 arch=linux-rhel8-zen
                ^util-macros@1.19.3%gcc@8.4.0 arch=linux-rhel8-zen
            ^libxml2@2.9.12%gcc@8.4.0~python arch=linux-rhel8-zen
                ^libiconv@1.16%gcc@8.4.0 libs=shared,static arch=linux-rhel8-zen
                ^xz@5.2.5%gcc@8.4.0~pic libs=shared,static arch=linux-rhel8-zen
                ^zlib@1.2.11%gcc@8.4.0+optimize+pic+shared arch=linux-rhel8-zen
            ^ncurses@6.2%gcc@8.4.0~symlinks+termlib abi=none arch=linux-rhel8-zen
        ^libevent@2.1.12%gcc@8.4.0+openssl arch=linux-rhel8-zen
            ^openssl@1.1.1l%gcc@8.4.0~docs certs=system arch=linux-rhel8-zen
                ^perl@5.26.3%gcc@8.4.0~cpanm+shared+threads patches=8cf4302ca8b480c60ccdcaa29ec53d9d50a71d4baf469ac8c6fca00ca31e58a2 arch=linux-rhel8-zen
        ^numactl@2.0.14%gcc@8.4.0 patches=4e1d78cbbb85de625bad28705e748856033eaafab92a66dffd383a3d7e00cc94,62fc8a8bf7665a60e8f4c93ebbd535647cebf74198f7afafec4c085a8825c006,ff37630df599cfabf0740518b91ec8daaf18e8f288b19adaae5364dc1f6b2296 arch=linux-rhel8-zen
            ^autoconf@2.69%gcc@8.4.0 patches=35c449281546376449766f92d49fc121ca50e330e60fefcfc9be2af3253082c2,7793209b33013dc0f81208718c68440c5aae80e7a1c4b8d336e382525af791a7,a49dd5bac3b62daa0ff688ab4d508d71dbd2f4f8d7e2a02321926346161bf3ee arch=linux-rhel8-zen
            ^automake@1.16.3%gcc@8.4.0 arch=linux-rhel8-zen
        ^openssh@8.7p1%gcc@8.4.0 arch=linux-rhel8-zen
            ^libedit@3.1-20210216%gcc@8.4.0 arch=linux-rhel8-zen
    ^parallelio@2_5_4%gcc@8.4.0~ipo+pnetcdf build_type=RelWithDebInfo arch=linux-rhel8-zen
        ^cmake@3.22.1%gcc@8.4.0~doc+ncurses+openssl+ownlibs~qt build_type=Release arch=linux-rhel8-zen
        ^netcdf-c@4.8.1%gcc@8.4.0~dap~fsync~hdf4~jna+mpi~parallel-netcdf+pic+shared arch=linux-rhel8-zen
            ^hdf5@1.10.7%gcc@8.4.0~cxx~fortran+hl~ipo~java+mpi+shared~szip~threadsafe+tools api=default build_type=RelWithDebInfo arch=linux-rhel8-zen
        ^netcdf-fortran@4.5.3%gcc@8.4.0~doc+pic+shared arch=linux-rhel8-zen
        ^parallel-netcdf@1.12.2%gcc@8.4.0~burstbuffer+cxx+fortran+pic+shared arch=linux-rhel8-zen
```

Instala o MPAS via Spack:

```bash
$ spack install mpas-model%gcc@8.4.0 ^parallelio+pnetcdf

[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/libsigsegv-2.13-ga2kptcpxz5uwkvcags2eqxiw34gylku
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/pkgconf-1.8.0-f6vlz5akdfna2ml4vhkfjqolixbxzkft
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/util-macros-1.19.3-6yxj6tb2jgn63y3b25xfhcj3r7jixu74
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/libiconv-1.16-nby7u4hbaomv24acpixu3ufjg7zghjql
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/xz-5.2.5-sqkwuovup7q3dnnepghuwog5tuyx5otl
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/zlib-1.2.11-qlcclwpc4bwbqaawtq5r72k3kwwiwqvr
[+] /usr (external perl-5.26.3-fwxqoy7ji5pp2upudvjnv7jnf36ujpdz)
[+] /home/modules/hpc/cmake/3.22.1 (external cmake-3.22.1-mfucdqwhoaaoh35alsgzwayopwwqha4a)
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/m4-1.4.19-ckwoi5vdaiihnspywnalk3fw2ggyn36m
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/ncurses-6.2-35nxhftu2gptybucdib6stmdnuqksbz2
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/libxml2-2.9.12-ob6o7shxfsawitcwgkvm25ohfdzp52tt
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/openssl-1.1.1l-yvtu7blzuwu7v3phdbjuffoihbrmeusj
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/libtool-2.4.6-mn4g2dgquzkgnuu3choz47uu2qjkj2xz
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/autoconf-2.69-3hyzatvwxntupn5zd4al4snvtijbgo5x
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/libedit-3.1-20210216-nv5gnjt5x6og42k75gozisnfufoi3llz
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/libevent-2.1.12-ace4gs66dgrgg6l47h577i3joiwap3bb
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/libpciaccess-0.16-gbcnrspmhzaq5vywarkw5b76ey7h5sjl
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/automake-1.16.3-cjjtqke77obc22stfpy6ymolmjfxzs63
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/openssh-8.7p1-jc2m2733zxklfbljfjw7otsjq4atp2fn
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/hwloc-2.6.0-owjevgwedwee6vimx3l7z7b7f6xw6ry4
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/numactl-2.0.14-i4jz4qpuvrxsvn5oj3h4326qiyywglfr
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/openmpi-4.1.1-lwuqgv4kxorg4ndszkdhbcjhwepvdxmq
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/parallel-netcdf-1.12.2-jnzqzicokmvmogm5zthffgvo27ozoq3c
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/hdf5-1.10.7-wqmja6jzzra4fixcpdoywncunv35k3r3
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/netcdf-c-4.8.1-cgxru3taqkwr2eguhpzi6ld5pk3fdhbk
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/netcdf-fortran-4.5.3-q5mxd2ukuesrmclt2vdulyyvzykkdnab
[+] /work/rpsouto.incc/spack/v0.17.1_minerva_mlogin/opt/spack/linux-rhel8-zen/gcc-8.4.0/parallelio-2_5_4-xbzx6sbupydf35wfo2fkfqs5rxrums4y

==> Installing mpas-model-7.1-fqhb42t2k47mjalypmms4nfjr4jpt73h
==> No binary for mpas-model-7.1-fqhb42t2k47mjalypmms4nfjr4jpt73h found: installing from source
==> Fetching https://mirror.spack.io/_source-cache/archive/9b/9b5c181b7d0163ae33d24d7a79ede6990495134b58cf4500ba5c8c94192102bc.tar.gz
==> Moving resource stage
    source : /work/rpsouto.incc/.spack/v0.17.1_minerva_mlogin/tmp/spack-stage/resource-MPAS-Data-fqhb42t2k47mjalypmms4nfjr4jpt73h/spack-src/
    destination : /work/rpsouto.incc/.spack/v0.17.1_minerva_mlogin/tmp/spack-stage/spack-stage-mpas-model-7.1-fqhb42t2k47mjalypmms4nfjr4jpt73h/spack-src/MPAS-Data
==> No patches needed for mpas-model
==> mpas-model: Executing phase: 'edit'
==> mpas-model: Executing phase: 'build'
==> mpas-model: Executing phase: 'install'
==> mpas-model: Successfully installed mpas-model-7.1-fqhb42t2k47mjalypmms4nfjr4jpt73h
  Fetch: 5.67s.  Build: 3m 16.54s.  Total: 3m 22.21s.
```

```bash
$ spack find
==> 27 installed packages
-- linux-rhel8-zen / gcc@8.4.0 ----------------------------------
autoconf@2.69         libevent@2.1.12    libxml2@2.9.12  netcdf-fortran@4.5.3  parallel-netcdf@1.12.2  xz@5.2.5
automake@1.16.3       libiconv@1.16      m4@1.4.19       numactl@2.0.14        parallelio@2_5_4        zlib@1.2.11
hdf5@1.10.7           libpciaccess@0.16  mpas-model@7.1  openmpi@4.1.1         perl@5.26.3
hwloc@2.6.0           libsigsegv@2.13    ncurses@6.2     openssh@8.7p1         pkgconf@1.8.0
libedit@3.1-20210216  libtool@2.4.6      netcdf-c@4.8.1  openssl@1.1.1l        util-macros@1.19.3
```

```bash
$ spack find -d mpas-model
==> 1 installed package
-- linux-rhel8-zen / gcc@8.4.0 ----------------------------------
mpas-model@7.1
    openmpi@4.1.1
        hwloc@2.6.0
            libpciaccess@0.16
            libxml2@2.9.12
                libiconv@1.16
                xz@5.2.5
                zlib@1.2.11
            ncurses@6.2
        libevent@2.1.12
            openssl@1.1.1l
        numactl@2.0.14
        openssh@8.7p1
            libedit@3.1-20210216
    parallelio@2_5_4
        netcdf-c@4.8.1
            hdf5@1.10.7
                pkgconf@1.8.0
        netcdf-fortran@4.5.3
        parallel-netcdf@1.12.2
```
