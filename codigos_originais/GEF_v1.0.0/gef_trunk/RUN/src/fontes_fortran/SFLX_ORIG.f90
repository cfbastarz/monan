      subroutine sflx (  &
       ice,dt,z,nsoil,sldpth,  &
       lwdn,soldn,sfcprs,prcp,sfctmp,th2,q2,sfcspd,q2sat,dqsdt2,  &
       vegtyp,soiltyp,slopetyp,  &
       shdfac,ptu,tbot,alb,snoalb,  &
       cmc,t1,stc,smc,sh2o,snowh,sneqv,albedo,ch,cm,  &
       etp,eta,h,s,runoff1,runoff2,q1,snmax,  &
       soilw,soilm, smcwlt,smcdry,smcref,smcmax )

      use mppstaff
!
    USE RITE
!
      implicit none
!c
! ----------------------------------------------------------------------
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c purpose:  sub-driver for "noah/osu lsm" family of physics subroutines
!c           for a soil/veg/snowpack land-surface model to update soil 
!c           moisture, soil ice, soil temperature, skin temperature, 
!c           snowpack water content, snowdepth, and all terms
!c           of the surface energy balance and surface water
!c           balance (excluding input atmospheric forcings of 
!c           downward radiation and precip)
!c
!c  version 2.2 07 february 2001
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
! ----------------------------------------------------------------------
! ------------    frozen ground version     ----------------------------
!     added states: sh2o(nsoil) - unfrozen soil moisture
!                   snowh       - snow depth
!
! ----------------------------------------------------------------------
!
! note on snow state variables:
!   snowh = actual physical snow depth in m
!   sneqv = liquid water-equivalent snow depth in m
!            (time-dependent snow density is obtained from sneqv/snowh)
!
! note on albedo fractions:
!   input:
!     alb    = baseline snow-free albedo, for julian day of year 
!                  (usually from temporal interpolation of monthly mean values)
!                  (calling prog may or may not include diurnal sun angle effect)
!     snoalb = upper bound on maximum albedo over deep snow
!                  (e.g. from robinson and kukla, 1985, j. clim. & appl. meteor.)
!   output:
!     albedo = computed albedo with snowcover effects 
!                 (computed using alb, snoalb, sneqv, and shdfac->green veg frac)
!
!                    argument list in the call to sflx
!
! ----------------------------------------------------------------------
! 1. calling statement
!
!     subroutine sflx
!    i (ice,dt,z,nsoil,sldpth,
!    i lwdn,soldn,sfcprs,prcp,sfctmp,th2,q2,q2sat,dqsdt2,
!    i vegtyp,soiltyp,slopetyp,
!    i shdfac,ptu,tbot,alb,snoalb,
!    i sfcspd,
!    2 cmc,t1,stc,smc,sh2o,snowh,sneqv,ch,cm,
!    o etp,eta,h,s,runoff1,runoff2,q1,snmax,albedo,
!    o soilw,soilm,smcwlt,smcdry,smcref,smcmax)
!
! 2. input (denoted by "i" in column six of argument list at top of routine)
!                  ### general parameters ###
!
!          ice: sea-ice flag  (=1: sea-ice, =0: land)
!           dt: timestep (sec)
!               (dt should not exceed 3600 secs, recommend 1800 secs or less)
!            z: height (m) above ground of atmospheric forcing variables
!        nsoil: number of soil layers  
!              (at least 2, and not greater than parameter nsold set below)
!       sldpth: the thickness of each soil layer (m) 
!
!                  ### atmospheric variables ###
!
!         lwdn: lw downward radiation (w m-2; positive, not net longwave)
!        soldn: solar downward radiation (w m-2; positive, not net shortwave)
!       sfcprs: pressure at height z above ground (pascals)
!         prcp: precip rate (kg m-2 s-1) (note, this is a rate)
!       sfctmp: air temperature (k) at height z above ground 
!          th2: air potential temperature (k) at height z above ground 
!           q2: mixing ratio at height z above ground (kg kg-1)
!       sfcspd: wind speed (m s-1) at height z above ground
!        q2sat: sat mixing ratio at height z above ground (kg kg-1)
!       dqsdt2: slope of sat specific humidity curve at t=sfctmp (kg kg-1 k-1)
!
!                  ### canopy/soil characteristics ###
!
!       vegtyp: vegetation type (integer index)
!       soiltyp: soil type (integer index)
!     slopetyp: class of sfc slope (integer index)
!       shdfac: areal fractional coverage of green vegetation (range 0.0-1.0)
!          ptu: photo thermal unit (plant phenology for annuals/crops)
!              (not yet used, but passed to redprm for future use in veg parms)
!         tbot: bottom soil temperature (local yearly-mean sfc air temperature)
!          alb: backround snow-free surface albedo (fraction)
!       snoalb: albedo upper bound over deep snow (fraction)
!
! 3. state variables: both input and output
!                         (note: output usually modified from input by physics)
!
!      (denoted by "2" in column six of argument list at top of routine)
!
!       !!! ########### state variables ##############  !!!
!
!         cmc: canopy moisture content (m)
!          t1: ground/canopy/snowpack) effective skin temperature (k)
!
!  stc(nsoil): soil temp (k)
!  smc(nsoil): total soil moisture content (volumetric fraction)
! sh2o(nsoil): unfrozen soil moisture content (volumetric fraction)
!               note: frozen soil moisture = smc - sh2o
!
!       snowh: snow depth (m)
!       sneqv: water-equivalent snow depth (m)
!               note: snow density = sneqv/snowh
!      albedo: surface albedo including snow effect (unitless fraction)
!          ch: sfc exch coef for heat and moisture (m s-1)
!          cm: sfc exch coef for momentum (m s-1)
!              note: ch and cm are technically conductances since they
!              have been multiplied by the wind speed.
!
! 4. output (denoted by "o" in column six of argument list at top of routine)
!
!        note-- sign convention of sfc energy fluxes below is: negative if
!            sink of energy to surface
!
!          etp: potential evaporation (w m-2)
!          eta: actual latent heat flux (w m-2: negative, if up from surface)
!            h: sensible heat flux (w m-2: negative, if upward from surface)
!            s: soil heat flux (w m-2: negative, if downward from surface)
!      runoff1: surface runoff (m s-1), not infiltrating the surface
!      runoff2: subsurface runoff (m s-1), drainage out bottom of last soil lyr
!           q1: effective mixing ratio at grnd sfc (kg kg-1)
!               (note: q1 is numerical expendiency for expressing eta
!                     equivalently in a bulk aerodynamic form)
!        snmax: snow melt (m) (water equivalent)
!        soilw: available soil moisture in root zone (unitless fraction between
!               soil saturation and wilting point)
!        soilm: total soil column moisture content (m) (frozen + unfrozen)
!
!           for diagnostic purposes, return some primary parameters next
!                        (set in routine redprm)
!
!       smcwlt: wilting point (volumetric)
!       smcdry: dry soil moisture threshold where direct evap frm top lyr ends
!       smcref: soil moisture threshold where transpiration begins to stress
!       smcmax: porosity, i.e. saturated value of soil moisture

      integer nsold
      parameter (nsold = 20)
!
      logical snowng
      logical frzgra
      logical saturated
!
      integer k
      integer kz
      integer ice
      integer nsoil,vegtyp,soiltyp,nroot
      integer slopetyp
!
      real albedo
      real alb
      real b
      real cfactr
!..................ch is sfc exchange coef for heat/moist
!..................cm is sfc momentum drag (not needed in sflx)
      real ch
      real cm
!
      real cmc
      real cmcmax
      real cp
      real csnow
      real csoil
      real czil
      real df1
      real df1p
      real dksat
      real dt
      real dwsat
      real dqsdt2
      real dsoil
      real dtot
      real expsno
      real expsoi
      real epsca
      real eta
      real etp
      real edir1
      real ec1
      real ett1
      real f
      real f1
      real fxexp
      real frzx
      real h
      real hs
      real kdt
      real lwdn
      real lvh2o
      real pc
      real prcp
      real ptu
      real prcp1
      real psisat
      real q1
      real q2
      real q2sat
      real quartz
      real r
      real rch
      real refkdt
      real rr
      real rtdis (nsold)
      real runoff1
      real runoff2
      real rgl
      real rsmax
      real rc
      real rcmin
      real rsnow
      real sndens
      real sncond 
      real s
      real sbeta
      real sfcprs
      real sfcspd
      real sfctmp
      real shdfac
      real sh2o(nsoil)
      real sldpth(nsoil)
      real smcdry
      real smcmax
      real smcref
      real smcwlt
      real smc(nsoil)
      real sneqv
      real snowh
      real snofac
      real sn_new
      real slope
      real snup
      real salp
      real snoalb
      real stc(nsoil)
      real soldn
      real snmax
      real soilm
      real soilw
      real soilwm
      real soilww
      real t1
      real t1v
      real t24
      real t2v
      real tbot
      real th2
      real th2v
      real topt
      real tfreez
      real xlai
      real z
      real zbot
      real z0
      real zsoil(nsold)
!
      parameter ( tfreez = 273.15      )
      parameter ( lvh2o  = 2.501000e+6 )
      parameter ( r      = 287.04      )
      parameter ( cp     = 1004.5      )
      
!
! common blk "rite" carries diagnostic quantities for printout,
! but is not involved in model physics and is not present in
! parent model that calls sflx

!   initialization

      runoff1 = 0.0
      runoff2 = 0.0
      runoff3 = 0.0
      snmax = 0.0
!
!  the variable "ice" is a flag denoting sea-ice case 

      if(ice .eq. 1) then

! sea-ice layers are equal thickness and sum to 3 meters
        do kz = 1, nsoil
          zsoil(kz)=-3.*float(kz)/float(nsoil)
        end do

      else

! calculate depth (negative) below ground from top skin sfc to 
! bottom of each soil layer.
! note:!!! sign of zsoil is negative (denoting below ground)
        zsoil(1)=-sldpth(1)
        do kz = 2, nsoil
          zsoil(kz)=-sldpth(kz)+zsoil(kz-1)
        end do

      endif
         
! ----------------------------------------------------------------------
!c
!c   next is crucial call to set the land-surface parameters, 
!c   including soil-type and veg-type dependent parameters.
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
        call redprm(vegtyp,soiltyp, slopetyp,   &
          cfactr, cmcmax, rsmax, topt, refkdt, kdt, sbeta,  &
          shdfac, rcmin, rgl, hs, zbot, frzx, psisat, slope,   &
          snup, salp, b, dksat, dwsat, smcmax, smcwlt, smcref,  &
          smcdry, f1, quartz, fxexp, rtdis, sldpth, zsoil,  &
          nroot, nsoil, z0, czil, xlai, csoil, ptu)
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c  next call routine sfcdif to calculate 
!c    the sfc exchange coef (ch) for heat and moisture
!c
!c  note  note  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!c    
!c          comment out call sfcdif, if sfcdif already called
!c          in calling program (such as in coupled atmospheric model)
!c
!c  note !!  do not call sfcdif until after above call to redprm, 
!c             in case alternative values of roughness length (z0) and 
!c              zilintinkevich coef (czil) are set there via namelist i/o
!c
!c   note !! routine sfcdif returns a ch that represents the wind spd
!c          times the "original" nondimensional "ch" typical in literature.
!c          hence the ch returned from sfcdif has units of m/s.
!c          the important companion coefficient of ch, carried here as "rch",
!c          is the ch from sfcdif times air density and parameter "cp".
!c         "rch" is computed in "call penman". rch rather than ch is the 
!          coeff usually invoked later in eqns.
!c
!c   note !! sfcdif also returns the surface exchange coefficient for
!            momentum, cm, also known as the surface drage coefficient,
!            but cm is not used here
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
    
! ----------------------------------------------------------------------
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! calc virtual temps and virtual potential temps needed by 
! subroutines sfcdif and penman
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      t2v  = sfctmp * (1.0 + 0.61 * q2 )
! comment out below 2 lines if call sfcdif is commented out, i.e. in
! the coupled model
!      t1v  =     t1 * (1.0 + 0.61 * q2 )
!      th2v =    th2 * (1.0 + 0.61 * q2 )
!
!      call sfcdif ( z, z0, t1v, th2v, sfcspd, czil, cm, ch )

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  initialize misc variables.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      snowng = .false.
      frzgra = .false.

! if sea-ice case,        assign default water-equiv snow on top
      if(ice .eq. 1) then
        sneqv = 0.01
        snowh = 0.05
      endif
!
! if input snowpack is nonzero, then compute snow density "sndens"
! and snow thermal conductivity "sncond"
! (note that csnow is a function subroutine)
!
      if(sneqv .eq. 0.0) then
        sndens = 0.0
        snowh = 0.0
        sncond = 1.0
      else
        sndens=sneqv/snowh
        sncond = csnow (sndens) 
      endif

! ----------------------------------------------------------------------
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     determine if it's precipitating and what kind of precip it is.
!     if it's prcping and the air temp is colder than 0 c, it's snowing!
!     if it's prcping and the air temp is warmer than 0 c, but the grnd
!     temp is colder than 0 c, freezing rain is presumed to be falling.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( prcp .gt. 0.0 ) then
        if ( sfctmp .le. tfreez ) then
          snowng = .true.
        else
          if ( t1 .le. tfreez ) frzgra = .true.
        endif
      endif

! ----------------------------------------------------------------------
! if either prcp flag is set, determine new snowfall (converting prcp
! rate from kg m-2 s-1 to a liquid equiv snow depth in meters) and add
! it to the existing snowpack.
! note that since all precip is added to snowpack, no precip infiltrates
! into the soil so that prcp1 is set to zero.
      if ( ( snowng ) .or. ( frzgra ) ) then
        sn_new = prcp * dt * 0.001
        sneqv = sneqv + sn_new
        prcp1 = 0.0
! ----------------------------------------------------------------------
! update snow density based on new snowfall, using old and new snow.
      call snow_new (sfctmp,sn_new,snowh,sndens)
! --- debug ------------------------------------------------------------
!      sndens = 0.2
!      snowh = sneqv/sndens
! --- debug ------------------------------------------------------------
! ----------------------------------------------------------------------
! update snow thermal conductivity
      sncond = csnow (sndens) 
! ----------------------------------------------------------------------

      else
!
! precip is liquid (rain), hence save in the precip variable that
! later can wholely or partially infiltrate the soil (along with 
! any canopy "drip" added to this later)
!
        prcp1 = prcp

      endif

! ----------------------------------------------------------------------
! update albedo, except over sea-ice
      if (ice .eq. 0) then

! ----------------------------------------------------------------------
! next is time-dependent surface albedo modification due to 
! time-dependent snowdepth state and time-dependent canopy greenness
      
!      if ( (sneqv .eq. 0.0) .or. (alb .ge. snoalb) ) then
        if (sneqv .eq. 0.0) then
          albedo = alb

        else
! ----------------------------------------------------------------------
! snup is veg-class dependent snowdepth threshhold (set in routine
! redprm)where max snow albedo effect is first attained
          if (sneqv .lt. snup) then
            rsnow = sneqv/snup
            snofac = 1. - ( exp(-salp*rsnow) - rsnow*exp(-salp))
          else
            snofac = 1.0
          endif
! ----------------------------------------------------------------------
! snoalb is argument representing maximum albedo over deep snow,
! as passed into sflx, and adapted from the satellite-based maximum 
! snow albedo fields provided by d. robinson and g. kukla 
! (1985, jcam, vol 24, 402-411)

          albedo = alb + (1.0-shdfac)*snofac*(snoalb-alb) 
          if (albedo .gt. snoalb) albedo=snoalb
        endif

      else
! ----------------------------------------------------------------------
! albedo over sea-ice
          albedo = 0.60
          snofac = 1.0
      endif

! ----------------------------------------------------------------------
! thermal conductivity for sea-ice case
      if (ice .eq. 1) then
        df1=2.2
      else
!
! next calculate the subsurface heat flux, which first requires
! calculation of the thermal diffusivity.  treatment of the
! latter follows that on pages 148-149 from "heat transfer in 
! cold climates", by v. j. lunardini (published in 1981 
! by van nostrand reinhold co.) i.e. treatment of two contiguous 
! "plane parallel" mediums (namely here the first soil layer 
! and the snowpack layer, if any). this diffusivity treatment 
! behaves well for both zero and nonzero snowpack, including the 
! limit of very thin snowpack.  this treatment also eliminates
! the need to impose an arbitrary upper bound on subsurface 
! heat flux when the snowpack becomes extremely thin.
!
! ----------------------------------------------------------------------
! first calculate thermal diffusivity of top soil layer, using
! both the frozen and liquid soil moisture, following the 
! soil thermal diffusivity function of peters-lidard et al.
! (1998,jas, vol 55, 1209-1224), which requires the specifying
! the quartz content of the given soil class (see routine redprm)
!
        call tdfcnd ( df1, smc(1),quartz,smcmax,sh2o(1) )
! ----------------------------------------------------------------------
! next add subsurface heat flux reduction effect from the 
! overlying green canopy, adapted from section 2.1.2 of 
! peters-lidard et al. (1997, jgr, vol 102(d4))
!
        df1 = df1 * exp(sbeta*shdfac)
      endif
! ----------------------------------------------------------------------
! finally "plane parallel" snowpack effect following 
! v.j. linardini reference cited above. note that dtot is
! combined depth of snowdepth and thickness of first soil layer
!
      dsoil = -(0.5 * zsoil(1))

      if (sneqv .eq. 0.) then
        s = df1 * (t1 - stc(1) ) / dsoil
      else
        dtot = snowh + dsoil
        expsno = snowh/dtot
        expsoi = dsoil/dtot
! 1. harmonic mean (series flow)
!     df1 = (sncond*df1)/(expsoi*sncond+expsno*df1)
! 2. arithmetic mean (parallel flow)
!     df1 = expsno*sncond + expsoi*df1
      df1p = expsno*sncond + expsoi*df1
! 3. geometric mean (intermediate between 
!                     harmonic and arithmetic mean)
!        df1 = (sncond**expsno)*(df1**expsoi)
! mbek, 16 jan 2002
! weigh df by snow fraction, use parallel flow
        df1 = df1p*snofac + df1*(1.0-snofac)

! ----------------------------------------------------------------------
! calculate subsurface heat flux, s, from final thermal diffusivity
! of surface mediums, df1 above, and skin temperature and top 
! mid-layer soil temperature
        s = df1 * (t1 - stc(1) ) / dtot
      endif

! ----------------------------------------------------------------------
!  calculate total downward radiation (solar plus longwave)
!  needed in penman ep subroutine that follows
          
          f = soldn*(1.0-albedo) + lwdn

! ----------------------------------------------------------------------
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     call penman subroutine to calculate potential evaporation (etp)
!     (and other partial products and sums save in common/rite for 
!       later calculations)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       call penman ( sfctmp,sfcprs,ch,t2v,th2,prcp,f,t24,s,q2,  &
                    q2sat,etp,rch,epsca,rr,snowng,frzgra,dqsdt2)
!
! following old constraint is disabled
!.....if(saturated) etp = 0.0

! ----------------------------------------------------------------------
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     call canres to calculate the canopy resistance and convert it 
!     into pc if more than trace amount of canopy greenness fraction
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!      if(shdfac .gt. 1.e-6) then
! make this threshold consistent with the one in smflx for transp
! and ec(anopy)
      if(shdfac .gt. 0.) then
      
!  frozen ground extension: total soil water "smc" was replaced 
!  by unfrozen soil water "sh2o" in call to canres below
!      
        call canres(soldn,ch,sfctmp,q2,sfcprs,sh2o,zsoil,nsoil,  &
                  smcwlt,smcref,rcmin,rc,pc,nroot,q2sat,dqsdt2,  &
                  topt,rsmax,rgl,hs,xlai)

      endif

! ----------------------------------------------------------------------
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      now decide major pathway branch to take depending on whether
!      snowpack exists or not
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        if ( sneqv .eq. 0.0 ) then

          call nopac ( etp, eta, prcp, smc, smcmax, smcwlt,  &
                       smcref,smcdry, cmc, cmcmax, nsoil, dt, shdfac,  &
                       sbeta,q1,q2,t1,sfctmp,t24,th2,f,f1,s,stc,  &
                       epsca, b, pc, rch, rr,  cfactr,  &
                       sh2o, slope, kdt, frzx, psisat, zsoil,  &
                       dksat, dwsat, tbot, zbot, runoff1,runoff2,  &
                       runoff3, edir1, ec1, ett1,nroot,ice,rtdis,  &
                       quartz, fxexp,csoil)
        else

          call snopac ( etp,eta,prcp,prcp1,snowng,smc,smcmax,smcwlt,  &
                      smcref, smcdry, cmc, cmcmax, nsoil, dt,   &
                      sbeta,q1,df1,  &
                      q2,t1,sfctmp,t24,th2,f,f1,s,stc,epsca,sfcprs, &
!     &                b, pc, rch, rr, cfactr, salp, sneqv,  &
                      b, pc, rch, rr, cfactr, snofac, sneqv,sndens,  &
                      snowh, sh2o, slope, kdt, frzx, psisat, snup,  &
                      zsoil, dwsat, dksat, tbot, zbot, shdfac,runoff1,  &
                      runoff2,runoff3,edir1,ec1,ett1,nroot,snmax,ice,  &
                      rtdis,quartz, fxexp,csoil)
        
        endif


! ----------------------------------------------------------------------
!   prepare sensible heat (h) for return to parent model
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          h = -(ch * cp * sfcprs)/(r * t2v) * ( th2 - t1 )
          
! ----------------------------------------------------------------------
!  convert units and/or sign of total evap (eta), potential evap (etp),
!  subsurface heat flux (s), and runoffs for what parent model expects
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
!  convert eta from kg m-2 s-1 to w m-2
!
      eta = eta*lvh2o
      etp = etp*lvh2o

! convert the sign of soil heat flux so that:
!         s>0: warm the surface  (night time)
!         s<0: cool the surface  (day time)

      s=-1.0*s      
!
!  convert runoff3 (internal layer runoff from supersat) from m to m s-1
!  and add to subsurface runoff/drainage/baseflow
!
      runoff3 = runoff3/dt
      runoff2 = runoff2+runoff3
!
! total column soil moisture in meters (soilm) and root-zone 
! soil moisture availability (fraction) relative to porosity/saturation

      soilm=-1.0*smc(1)*zsoil(1)
      
      do k = 2, nsoil
        soilm=soilm+smc(k)*(zsoil(k-1)-zsoil(k))
      end do
      soilwm=-1.0*(smcmax-smcwlt)*zsoil(1)
      soilww=-1.0*(smc(1)-smcwlt)*zsoil(1)
      do k = 2, nroot
        soilwm=soilwm+(smcmax-smcwlt)*(zsoil(k-1)-zsoil(k))
        soilww=soilww+(smc(k)-smcwlt)*(zsoil(k-1)-zsoil(k))
      end do
      soilw=soilww/soilwm
!
      return
      end




      subroutine canres(solar,ch,sfctmp,q2,sfcprs,smc,zsoil,nsoil,  &
                        smcwlt,smcref,rcmin,rc,pc,nroot,q2sat,dqsdt2,   &
                        topt,rsmax,rgl,hs,xlai)

      implicit none

! ######################################################################
!                        subroutine canres
!                        -----------------
!       this routine calculates the canopy resistance which depends on
!       incoming solar radiation, air temperature, atmospheric water
!       vapor pressure deficit at the lowest model level, and soil
!       moisture (preferably unfrozen soil moisture rather than total)
! ----------------------------------------------------------------------
!        source:  jarvis (1976), jacquemin and noilhan (1990 blm)
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!        input:  solar: incoming solar radiation
!                ch:     surface exchange coefficient for heat and moisture
!                sfctmp: air temperature at 1st level above ground
!                q2:     air humidity at 1st level above ground
!                q2sat:  saturation air humidity at 1st level above ground
!                dqsdt2: slope of saturation humidity function wrt temp
!                sfcprs: surface pressure
!                smc:    volumetric soil moisture 
!                zsoil:  soil depth (negative sign, as it is below ground)
!                nsoil:  no. of soil layers
!                nroot:  no. of soil layers in root zone (1.le.nroot.le.nsoil)
!                xlai:   leaf area index
!                smcwlt: wilting point
!                smcref: reference soil moisture
!                        (where soil water deficit stress sets in)
!
! rcmin, rsmax, topt, rgl, hs: canopy stress parameters set in subr redprm
!
!  (see eqns 12-14 and table 2 of sec. 3.1.2 of 
!       chen et al., 1996, jgr, vol 101(d3), 7251-7268)               
!
!        output:  pc: plant coefficient
!                 rc: canopy resistance
! ----------------------------------------------------------------------
! ######################################################################

      integer   nsold
      parameter (nsold = 20)

      integer k
      integer nroot
      integer nsoil

      real sigma, rd, cp, slv
      real solar, ch, sfctmp, q2, sfcprs 
      real smc(nsoil), zsoil(nsoil), part(nsold) 
      real smcwlt, smcref, rcmin, rc, pc, q2sat, dqsdt2
      real topt, rsmax, rgl, hs, xlai, rcs, rct, rcq, rcsoil, ff
      real p, qs, gx, tair4, st1, slvcp, rr, delta

      parameter (sigma=5.67e-8, rd=287.04, cp=1004.5, slv=2.501000e6)

      rcs = 0.0
      rct = 0.0
      rcq = 0.0
      rcsoil = 0.0
      rc = 0.0

! ----------------------------------------------------------------------
! contribution due to incoming solar radiation
! ----------------------------------------------------------------------

!c/98/01/05/..disgard old version assuming fixed lai=1
!c...........ff = 0.55*2.0*solar/rgl

      ff = 0.55*2.0*solar/(rgl*xlai)
      rcs = (ff + rcmin/rsmax) / (1.0 + ff)
      rcs = max(rcs,0.0001)

! ----------------------------------------------------------------------
! contribution due to air temperature at first model level above ground
! ----------------------------------------------------------------------

      rct = 1.0 - 0.0016*((topt-sfctmp)**2.0)
      rct = max(rct,0.0001)

! ----------------------------------------------------------------------
! contribution due to vapor pressure deficit at first model level.
! ----------------------------------------------------------------------

!      p = sfcprs
      qs = q2sat
! rcq expression from ssib 
      rcq = 1.0/(1.0+hs*(qs-q2))
      rcq = max(rcq,0.01)

! ----------------------------------------------------------------------
! contribution due to soil moisture availability.
! determine contribution from each soil layer, then add them up.
! ----------------------------------------------------------------------

      gx = (smc(1) - smcwlt) / (smcref - smcwlt)
      if (gx .gt. 1.) gx = 1.
      if (gx .lt. 0.) gx = 0.

!####   using soil depth as weighting factor
      part(1) = (zsoil(1)/zsoil(nroot)) * gx

!#### using root distribution as weighting factor
!c      part(1) = rtdis(1) * gx
      
      do k = 2, nroot
        gx = (smc(k) - smcwlt) / (smcref - smcwlt)
        if (gx .gt. 1.) gx = 1.
        if (gx .lt. 0.) gx = 0.
!####   using soil depth as weighting factor        
        part(k) = ((zsoil(k)-zsoil(k-1))/zsoil(nroot)) * gx

!#### using root distribution as weighting factor
!c         part(k) = rtdis(k) * gx 
               
      end do

      do k = 1, nroot
        rcsoil = rcsoil+part(k)
      end do

      rcsoil = max(rcsoil,0.0001)

! ----------------------------------------------------------------------
!         determine canopy resistance due to all factors.
!         convert canopy resistance (rc) to plant coefficient (pc).
! ----------------------------------------------------------------------

!c/98/01/05/........rc = rcmin/(rcs*rct*rcq*rcsoil)
      rc = rcmin/(xlai*rcs*rct*rcq*rcsoil)
          
      tair4 = sfctmp**4.
      st1 = (4.*sigma*rd)/cp
      slvcp = slv/cp
      rr = st1*tair4/(sfcprs*ch) + 1.0
      delta = slvcp*dqsdt2
      
      pc = (rr+delta)/(rr*(1.+rc*ch)+delta)
      
      return
      end







      function csnow ( dsnow )

      implicit none

      real c
      real dsnow
      real csnow
      real unit

      parameter ( unit=0.11631 ) 
                                         
!   ####  simulation of termal snow conductivity                   
!   ####  simulation units of csnow is cal/(cm*hr* c) 
!   ####  and it will be returnd in w/(m* c)
!   ####  basic version is dyachkova equation                                

! #####   dyachkova equation (1960), for range 0.1-0.4

      c=0.328*10**(2.25*dsnow)
      csnow=unit*c

! #####    de vaux equation (1933), in range 0.1-0.6
!       csnow=0.0293*(1.+100.*dsnow**2)
      
!     #####   e. andersen from flerchinger
!     csnow=0.021+2.51*dsnow**2        
      
      return                                                      
      end







      function devap ( etp1, smc, zsoil, shdfac, smcmax, b,  &
                       dksat, dwsat, smcdry, smcref, smcwlt, fxexp)

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    name:  direct evaporation (devap) function  version: n/a
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real b
      real devap
      real dksat
      real dwsat
      real etp1
      real fx
      real fxexp
      real shdfac
      real smc
      real smcdry
      real smcmax
      real zsoil
      real smcref
      real smcwlt
      real sratio

! ----------------------------------------------------------------------
! direct evap a function of relative soil moisture availability, linear
! when fxexp=1.
! fx > 1 represents demand control
! fx < 1 represents flux control
! ----------------------------------------------------------------------
      sratio = (smc - smcdry) / (smcmax - smcdry)
      if (sratio .gt. 0.) then
        fx = sratio**fxexp
        fx = max ( min ( fx, 1. ) ,0. )
      else
        fx = 0.
      endif
!
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     allow for the direct-evap-reducing effect of shade
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      devap = fx * ( 1.0 - shdfac ) * etp1

      return
      end






      function frh2o(tkelv,smc,sh2o,smcmax,b,psis)

      implicit none

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c  purpose:  calculate amount of supercooled liquid soil water content
!c  if temperature is below 273.15k (t0).  requires newton-type iteration
!c  to solve the nonlinear implicit equation given in eqn 17 of
!c  koren et al. (1999, jgr, vol 104(d16), 19569-19585).
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! new version (june 2001): much faster and more accurate newton iteration
! achieved by first taking log of eqn cited above -- less than 4
! (typically 1 or 2) iterations achieves convergence.  also, explicit
! 1-step solution option for special case of parameter ck=0, which reduces
! the original implicit equation to a simpler explicit form, known as the
! ""flerchinger eqn". improved handling of solution in the limit of
! freezing point temperature t0.
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! input:
!
!   tkelv.........temperature (kelvin)
!   smc...........total soil moisture content (volumetric)
!   sh2o..........liquid soil moisture content (volumetric)
!   smcmax........saturation soil moisture content (from redprm)
!   b.............soil type "b" parameter (from redprm)
!   psis..........saturated soil matric potential (from redprm)
!
! output:
!   frh2o.........supercooled liquid water content.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real b
      real blim
      real bx
      real ck
      real denom
      real df
      real dh2o
      real dice
      real dswl
      real error
      real fk
      real frh2o
      real gs
      real hlice
      real psis
      real sh2o
      real smc
      real smcmax
      real swl
      real swlk
      real tkelv
      real t0

      integer nlog
      integer kcount

      parameter (ck=8.0)
!      parameter (ck=0.0)
      parameter (blim=5.5)
!      parameter (blim=7.0)
      parameter (error=0.005)

      parameter (hlice=3.335e5)
      parameter (gs = 9.80616)
      parameter (dice=920.0)
      parameter (dh2o=1000.0)
      parameter (t0=273.15)

!  ###   limits on parameter b: b < 5.5  (use parameter blim)  ####
!  ###   simulations showed if b > 5.5 unfrozen water content  ####
!  ###   is non-realistically high at very low temperatures    ####
!##################################################################
!
      bx = b
      if ( b .gt. blim ) bx = blim
!------------------------------------------------------------------

! initializing iterations counter and iterative solution flag.
      nlog=0
      kcount=0

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  if temperature not significantly below freezing (t0), sh2o = smc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if (tkelv .gt. (t0 - 1.e-3)) then

        frh2o=smc

      else

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       if (ck .ne. 0.0) then

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccc option 1: iterated solution for nonzero ck ccccccccccc
!ccccccccccc in koren et al, jgr, 1999, eqn 17 ccccccccccccccccc
!
! initial guess for swl (frozen content)
        swl = smc-sh2o
! keep within bounds.
         if (swl .gt. (smc-0.02)) swl=smc-0.02
         if(swl .lt. 0.) swl=0.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  start of iterations
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        do while (nlog .lt. 10 .and. kcount .eq. 0)
         nlog = nlog+1
         df = alog(( psis*gs/hlice ) * ( ( 1.+ck*swl )**2. ) *  &
              ( smcmax/(smc-swl) )**bx) - alog(-(tkelv-t0)/tkelv)
         denom = 2. * ck / ( 1.+ck*swl ) + bx / ( smc - swl )
         swlk = swl - df/denom
! bounds useful for mathematical solution.
         if (swlk .gt. (smc-0.02)) swlk = smc - 0.02
         if(swlk .lt. 0.) swlk = 0.
! mathematical solution bounds applied.
         dswl=abs(swlk-swl)
         swl=swlk
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c if more than 10 iterations, use explicit method (ck=0 approx.)
!c when dswl less or eq. error, no more iterations required.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         if ( dswl .le. error )  then
           kcount=kcount+1
         end if
        end do
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  end of iterations
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! bounds applied within do-block are valid for physical solution.
        frh2o = smc - swl
!
!ccccccccccccccccccccccc end option 1 ccccccccccccccccccccccccccc

       endif

       if (kcount .eq. 0) then

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccc option 2: explicit solution for flerchinger eq. i.e. ck=0 cccccccc
!cccccccccccc in koren et al., jgr, 1999, eqn 17  ccccccccccccccc
!
        fk=(((hlice/(gs*(-psis)))*((tkelv-t0)/tkelv))**(-1/bx))*smcmax
! apply physical bounds to flerchinger solution
        if (fk .lt. 0.02) fk = 0.02
        frh2o = min ( fk, smc )
!
!cccccccccccccccccccccccc end option 2 cccccccccccccccccccccccccc

       endif

      endif

      return
      end







      subroutine hrt ( rhsts,stc,smc,smcmax,nsoil,zsoil,yy,zz1,  &
                       tbot, zbot, psisat, sh2o, dt, b,  &
                       f1, df1, quartz, csoil)
!
    USE ABCI

      implicit none

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate the right hand side of the time tendency
!c    =======   term of the soil thermal diffusion equation.  also to
!c              compute ( prepare ) the matrix coefficients for the
!c              tri-diagonal matrix of the implicit time scheme.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      integer i
      integer k
      integer nsoil

! declare work arrays needed in tri-diagonal implicit solver


! declare specific heat capacities

      real cair
      real ch2o
      real cice
      real csoil

      real ddz
      real ddz2
      real denom
      real df1
      real df1n
      real df1k
      real dtsdz
      real dtsdz2
      real f1
      real hcpct
      real quartz
      real qtot
      real rhsts ( nsoil )
      real s
      real smc   ( nsoil )

      real sh2o  ( nsoil )
      real smcmax
            
      real stc   ( nsoil )
      real tbot
      real zbot
      real yy
      real zsoil ( nsoil )
      real zz1

      real t0, tsurf, psisat, dt, b, sice, tbk, tsnsr, tbk1

      real snksrc
!
      parameter ( t0   = 273.15  )

! set specific heat capacities of air, water, ice, soil mineral       

      parameter ( cair =1004.0   )
      parameter ( ch2o = 4.2e6   )
      parameter ( cice = 2.106e6 )
!.....parameter ( csoil=1.26e6   )
!.....
! note: csoil now set in routine redprm and passed in

!+++++++++++++ begin section for top soil layer +++++++++++++++++++++

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the heat capacity of the top soil layer
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      hcpct = sh2o(1)*ch2o + (1.0-smcmax)*csoil + (smcmax-smc(1))*cair  &
              + ( smc(1) - sh2o(1) )*cice

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the matrix coefficients ai, bi, and ci for the top layer
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz = 1.0 / ( -0.5 * zsoil(2) )
      ai(1) = 0.0
      ci(1) =  ( df1 * ddz ) / ( zsoil(1) * hcpct )
      bi(1) = -ci(1) + df1 / ( 0.5 * zsoil(1) * zsoil(1)*hcpct*zz1)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the vertical soil temp gradient btwn the 1st and 2nd soil
!     layers.  then calculate the subsurface heat flux. use the temp
!     gradient and subsfc heat flux to calc "right-hand side tendency
!     terms", or "rhsts", for top soil layer.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
      dtsdz = ( stc(1) - stc(2) ) / ( -0.5 * zsoil(2) )
      s = df1 * ( stc(1) - yy ) / ( 0.5 * zsoil(1) * zz1 )
      rhsts(1) = ( df1 * dtsdz - s ) / ( zsoil(1) * hcpct )

! next, set temp "tsurf" at top of soil column (for use in freezing
! soil physics later in function subroutine snksrc). if snowpack 
! content is zero, then expression below gives tsurf = skin temp.
! if snowpack is nonzero (hence argument zz1=1), then expression
! below yields soil column top temperature under snowpack.
!
      tsurf = ( yy + ( zz1 - 1 ) * stc(1) ) / zz1
!
! next capture the vertical difference of the heat flux at top 
! and bottom of first soil layer for use in heat flux constraint 
! applied to potential soil freezing/thawing in routine snksrc
!
      qtot = s - df1*dtsdz

!
! calculate temperature at bottom interface of 1st soil layer 
! for use later in fcn subroutine snksrc
!
      call tbnd ( stc(1), stc(2), zsoil, zbot, 1, nsoil,tbk)
!
! calculate frozen water content in 1st soil layer. 
!
      sice = smc(1) - sh2o(1)
!
! if frozen water present or any of layer-1 mid-point or bounding
! interface temperatures below freezing, then call snksrc to
! compute heat source/sink (and change in frozen water content)
! due to possible soil water phase change
!

      if ( (sice .gt. 0.) .or. (tsurf .lt. t0) .or.  &
           (stc(1) .lt. t0) .or. (tbk .lt. t0) ) then
 
       tsnsr = snksrc ( tsurf, stc(1),tbk, smc(1), sh2o(1),   &
                 zsoil, nsoil, smcmax, psisat, b, dt, 1, qtot )

       rhsts(1) = rhsts(1) - tsnsr / ( zsoil(1) * hcpct )
         if(rhsts(1) .lt. -.1) then
!           print *,"ci",rhsts(1),tsnsr,zsoil(1),hcpct,tsnsr / ( zsoil(1) * hcpct )
!           print *,sice,tsurf,stc(1),tbk, t0,sh2o(1)
         endif
      endif

 
! ++++++++++++++ this ends section for top soil layer ++++++++++++++
            
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     initialize ddz2
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz2 = 0.0

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     loop thru the remaining soil layers, repeating the above process
!(except subsfc or "ground" heat flux not repeated in lower layers)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      df1k = df1
      do k = 2, nsoil

!       calc this soil layer's heat capacity

        hcpct = sh2o(k)*ch2o +(1.0-smcmax)*csoil +(smcmax-smc(k))*cair  &
              + ( smc(k) - sh2o(k) )*cice
!
        if ( k .ne. nsoil ) then

!+++++++ this section for layer 2 or greater, but not last layer +++++

! calculate thermal diffusivity for this layer

           call tdfcnd ( df1n, smc(k),quartz,smcmax,sh2o(k))

! calc the vertical soil temp gradient thru this layer

           denom = 0.5 * ( zsoil(k-1) - zsoil(k+1) )
           dtsdz2 = ( stc(k) - stc(k+1) ) / denom

! calc the matrix coef, ci, after calc'ng its partial product

           ddz2 = 2. / (zsoil(k-1) - zsoil(k+1))
           ci(k) = -df1n * ddz2 / ((zsoil(k-1) - zsoil(k)) * hcpct)

! calculate temp at bottom of layer

           call tbnd ( stc(k),stc(k+1),zsoil,zbot,k,nsoil,tbk1 )

        else
!+++++++++++++ special case of bottom soil layer +++++++++++++++++++++

! calculate thermal diffusivity for this layer

           call tdfcnd ( df1n, smc(k),quartz,smcmax,sh2o(k))

! calc the vertical soil temp gradient thru this layer

           denom = .5 * (zsoil(k-1) + zsoil(k)) - zbot
           dtsdz2 = (stc(k)-tbot) / denom

!....set matrix coef, ci to zero if bottom layer 

           ci(k) = 0.

! calculate temp at bottom of last layer

           call tbnd ( stc(k), tbot, zsoil, zbot, k, nsoil,tbk1 )

        end if
!+++++++++++++ this ends special code for bottom layer +++++++++

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc rhsts for this layer after calc'ng a partial product
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        denom = ( zsoil(k) - zsoil(k-1) ) * hcpct
        rhsts(k) = ( df1n * dtsdz2 - df1k * dtsdz ) / denom

        qtot = -1.0*denom*rhsts(k)

        sice = smc(k) - sh2o(k)

      if ( (sice .gt. 0.) .or. (tbk .lt. t0) .or.  &
           (stc(k) .lt. t0) .or. (tbk1 .lt. t0) ) then

       tsnsr = snksrc ( tbk, stc(k),tbk1, smc(k), sh2o(k),   &
                 zsoil, nsoil, smcmax, psisat, b, dt, k, qtot)

       rhsts(k) = rhsts(k) - tsnsr / denom

      endif 
! -------------------------------------------------------------------
      
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc matrix coefs, ai, and bi for this layer.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ai(k) = - df1 * ddz / ((zsoil(k-1) - zsoil(k)) * hcpct)
        bi(k) = -(ai(k) + ci(k))

! reset values of df1, dtsdz, ddz, and tbk for loop to next soil lyr

        tbk   = tbk1
        df1k  = df1n
        dtsdz = dtsdz2
        ddz   = ddz2
!   
      end do

      return
      end



      subroutine hrtice (rhsts,stc,nsoil,zsoil,yy,zz1,df1)
!
    USE ABCI

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate the right hand side of the time tendency
!c    =======   term of the soil thermal diffusion equation in the case
!c              of sea-ice pack.  also to compute ( prepare ) the
!c              matrix coefficients for the tri-diagonal matrix of 
!c              the implicit time scheme.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      integer k
      integer nsoil

      real ddz
      real ddz2
      real denom
      real df1
      real dtsdz
      real dtsdz2
      real hcpct
      real rhsts ( nsoil )
      real s
      real stc   ( nsoil )
      real tbot
      real yy
      real zbot
      real zsoil ( nsoil )
      real zz1
!
! the input argument df1 a universally constant value of
! sea-ice thermal diffusivity, set in routine snopac as
!  df1 = 2.2

! set lower boundary depth and boundary temperature of 
! unfrozen sea water at bottom of sea ice pack.  assume 
! ice pack is of nsoil layers spanning a uniform constant
! ice pack thickness as defined in routine sflx

      zbot = zsoil(nsoil)
      tbot = 271.16

! set a nominal universal value of the sea-ice specific heat capacity
      
      hcpct=1880.0*917.0

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the matrix coefficients ai, bi, and ci for the top layer
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz = 1.0 / ( -0.5 * zsoil(2) )
      ai(1) = 0.0
      ci(1) =  ( df1 * ddz ) / ( zsoil(1) * hcpct )
      bi(1) = -ci(1) + df1/( 0.5 * zsoil(1) * zsoil(1) * hcpct * zz1)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the vertical soil temp gradient btwn the top and 2nd soil
!     layers.  recalc/adjust the soil heat flux.  use the gradient
!     and flux to calc rhsts for the top soil layer.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      dtsdz = ( stc(1) - stc(2) ) / ( -0.5 * zsoil(2) )
      s = df1 * ( stc(1) - yy ) / ( 0.5 * zsoil(1) * zz1 )
      rhsts(1) = ( df1 * dtsdz - s ) / ( zsoil(1) * hcpct )

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     initialize ddz2
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz2 = 0.0

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     loop thru the remaining soil layers, repeating the above process
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 2, nsoil

        if ( k .ne. nsoil ) then

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         calc the vertical soil temp gradient thru this layer.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          denom = 0.5 * ( zsoil(k-1) - zsoil(k+1) )
          dtsdz2 = ( stc(k) - stc(k+1) ) / denom

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         calc the matrix coef, ci, after calc'ng its partial product
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ddz2 = 2. / (zsoil(k-1) - zsoil(k+1))
          ci(k) = -df1 * ddz2 / ((zsoil(k-1) - zsoil(k)) * hcpct)

        else

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         calc the vertical soil temp gradient thru the lowest layer
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          dtsdz2 = (stc(k)-tbot)/(.5 * (zsoil(k-1) + zsoil(k))-zbot)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         set matrix coef, ci to zero
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

          ci(k) = 0.
        end if

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc rhsts for this layer after calc'ng a partial product
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        denom = ( zsoil(k) - zsoil(k-1) ) * hcpct
        rhsts(k) = ( df1 * dtsdz2 - df1 * dtsdz ) / denom

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc matrix coefs, ai, and bi for this layer.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        ai(k) = - df1 * ddz / ((zsoil(k-1) - zsoil(k)) * hcpct)
        bi(k) = -(ai(k) + ci(k))

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       reset values of dtsdz and ddz for loop to next soil lyr
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        dtsdz = dtsdz2
        ddz   = ddz2

      end do

      return
      end




      subroutine hstep ( stcout, stcin, rhsts, dt, nsoil )
!
    USE ABCI

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate/update the soil temperature field.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      integer k
      integer nsoil
!
      real ciin  ( nsold )
      real dt
      real rhsts   ( nsoil )
      real rhstsin ( nsoil )
      real stcout  ( nsoil )
      real stcin   ( nsoil )
!

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     create finite difference values for use in rosr12 routine
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 1 , nsoil
        rhsts(k) = rhsts(k) * dt
        ai(k) = ai(k) * dt
        bi(k) = 1. + bi(k) * dt
        ci(k) = ci(k) * dt
      end do

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     copy values for input variables before call to rosr12
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      do k = 1 , nsoil
         rhstsin(k) = rhsts(k)
      end do
      do k = 1 , nsold
         ciin(k) = ci(k)
      end do
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     solve the tri-diagonal matrix equation
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call rosr12 ( ci,ai,bi,ciin,rhstsin,rhsts,nsoil )

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc/update the soil temps using matrix solution
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 1 , nsoil
         stcout(k) = stcin(k) + ci(k)
      end do

      return
      end








      subroutine nopac ( etp, eta, prcp, smc, smcmax, smcwlt,  &
                         smcref,smcdry,cmc,cmcmax, nsoil, dt, shdfac,  &
                         sbeta,  &
                         q1, q2, t1, sfctmp, t24, th2, f, f1, s, stc,  &
                         epsca, b, pc, rch, rr,  cfactr,   &
                         sh2o, slope, kdt, frzfact, psisat, zsoil,  &
                         dksat, dwsat, tbot, zbot, runoff1, runoff2,  &
                         runoff3, edir1, ec1, ett1, nroot, ice,rtdis,  &
                         quartz, fxexp,csoil)


      use MPPSTAFF
!
    USE RITE    , RUNOXX3 => RUNOFF3

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate soil moisture and heat flux values and update
!c    =======   soil moisture content and soil heat content values for
!c              the case when no snow pack is present.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer ice
      integer nroot
      integer nsoil

      real b
      real cfactr
      real cmc
      real cmcmax
      real cp
      real csoil
      real df1
      real dksat
      real dt
      real dwsat
      real epsca
      real eta
      real eta1
      real etp
      real etp1
      real f
      real f1
      real fxexp
      real kdt
      real pc
      real prcp
      real prcp1
      real q2
      real rch
      real rr
      real rtdis (nsoil)
      real runoff
      real s
      real sbeta
      real sfctmp
      real shdfac
      real sigma
      real smc   ( nsoil )
      real sh2o  ( nsoil )
      real smcdry
      real smcmax
      real smcref
      real smcwlt
      real stc   ( nsoil )
      real t1
      real t24
      real tbot
      real zbot
      real th2
      real yy
      real yynum
      real zsoil ( nsoil )
      real zz1

      real q1, slope, frzfact, psisat, runoff1, runoff2,runoff3
      real edir1, ec1, ett1, quartz

      parameter(cp=1004.5, sigma=5.67e-8)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     executable code begins here.....
!     convert etp from kg m-2 s-1 to ms-1 and initialize dew.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      prcp1 = prcp * 0.001
      etp1 = etp * 0.001
      dew = 0.0

      if ( etp .gt. 0.0 ) then

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       convert prcp from  kg m-2 s-1  to  m s-1
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

           call smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,  &
                sh2o, slope, kdt, frzfact,  &
                smcmax,b,pc,smcwlt,dksat,dwsat,smcref,shdfac,  &
                cmcmax,smcdry,cfactr, runoff1,runoff2, runoff3,   &
                edir1, ec1, ett1, sfctmp,q2,nroot,rtdis, fxexp)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       convert modeled evapotranspiration fm  m s-1  to  kg m-2 s-1
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        eta = eta1 * 1000.0

      else

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       if etp < 0, assume dew forms (transform etp1 into dew
!       and reinitialize etp1 to zero)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        dew = -etp1
        etp1 = 0.0

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       convert prcp from  kg m-2 s-1  to  m s-1  and add dew amt
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        prcp1 = prcp1 + dew
!
      call smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,  &
                sh2o, slope, kdt, frzfact,  &
                smcmax,b,pc,smcwlt,dksat,dwsat,smcref,shdfac,  &
                cmcmax,smcdry,cfactr, runoff1,runoff2, runoff3,   &
                edir1, ec1, ett1, sfctmp, q2, nroot,rtdis, fxexp)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       convert modeled evapotranspiration fm  m s-1  to  kg m-2 s-1
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        eta = eta1 * 1000.0

      endif

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     based on etp and e values, determine beta
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( etp .le. 0.0 ) then
        beta = 0.0
        if ( etp .lt. 0.0 ) then
          beta = 1.0
          eta = etp
        endif
      else
        beta = eta / etp
      endif

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!    get soil thermal diffuxivity/conductivity for top soil lyr,
!    calc. adjusted top lyr soil temp and adjusted soil flux, then
!    call shflx to compute/update soil heat flux and soil temps.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call tdfcnd ( df1, smc(1),quartz,smcmax,sh2o(1) )

! vegetation greenness fraction reduction in subsurface heat flux 
! via reduction factor, which is convenient to apply here to thermal 
! diffusivity that is later used in hrt to compute sub sfc heat flux
! (see additional comments on veg effect sub-sfc heat flx in 
!  routine sflx)

      df1 = df1 * exp(sbeta*shdfac)

! compute intermediate terms passed to routine hrt (via routine 
! shflx below) for use in computing subsurface heat flux in hrt

      yynum = f - sigma * t24
      yy = sfctmp + (yynum/rch+th2-sfctmp-beta*epsca) / rr
      zz1 = df1 / ( -0.5 * zsoil(1) * rch * rr ) + 1.0
      call shflx ( s,stc,smc,smcmax,nsoil,t1,dt,yy,zz1,zsoil,tbot,  &
                   zbot, smcwlt, psisat, sh2o,  &
                   b,f1,df1, ice,   &
                   quartz,csoil)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     set flx1, and flx3 to zero since they are not used.  flx2
!     was similarly initialized in the penman routine.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      flx1 = 0.0
      flx3 = 0.0
!
      return
      end






      subroutine penman(sfctmp,sfcprs,ch,t2v,th2,prcp,f,t24,s,q2,  &
                        q2sat,etp,rch,epsca,rr,snowng,frzgra,dqsdt2)
!
    USE RITE    , RUNOXX3 => RUNOFF3

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate potential evaporation for the current point.
!c    =======   various partial sums/products are also calculated and
!c              passed back to the calling routine for later use.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      logical snowng
      logical frzgra

      real a
      real ch
      real cp
      real cph2o
      real cpice
      real delta
      real elcp
      real epsca
      real etp
      real f
      real fnet
      real lsubc
      real lsubf
      real prcp
      real q2
      real q2sat
      real r
      real rad
      real rch
      real rho
      real rr
      real runoff,runoff3
      real s
      real sfcprs
      real sfctmp
      real sigma
      real t24
      real t2v
      real th2
      real dqsdt2
!
      parameter(cp=1004.6,cph2o=4.218e+3,cpice=2.106e+3,r=287.04,  &
         elcp=2.4888e+3,lsubf=3.335e+5,lsubc=2.501000e+6,sigma=5.67e-8)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     executable code begins here...
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      flx2 = 0.0

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     prepare partial quantities for penman equation.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      delta = elcp * dqsdt2
      t24 = sfctmp * sfctmp * sfctmp * sfctmp
      rr = t24 * 6.48e-8 / ( sfcprs * ch ) + 1.0
      rho = sfcprs / ( r * t2v )
      rch = rho * cp * ch

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     adjust the partial sums / products with the latent heat
!     effects caused by falling precipitation.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( .not. snowng ) then
        if ( prcp .gt. 0.0 ) rr = rr + cph2o * prcp / rch
      else
        rr = rr + cpice * prcp / rch
      endif

      fnet = f - sigma * t24 - s

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     include the latent heat effects of frzng rain converting to
!     ice on impact in the calculation of flx2 and fnet.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if ( frzgra ) then
        flx2 = -lsubf * prcp
        fnet = fnet - flx2
      endif

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     finish penman equation calculations.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      rad = fnet / rch + th2 - sfctmp
      a = elcp * ( q2sat - q2 )
      epsca = ( a * rr + rad * delta ) / ( delta + rr )
      etp = epsca * rch / lsubc

      return
      end







      subroutine redprm(vegtyp, soiltyp, slopetyp,  &
           cfactr, cmcmax, rsmax, topt, refkdt, kdt, sbeta,  &
           shdfac, rcmin, rgl, hs, zbot, frzx, psisat, slope,  &
           snup, salp, b, dksat, dwsat, smcmax, smcwlt, smcref,  &
           smcdry, f1, quartz, fxexp, rtdis, sldpth, zsoil,  &
           nroot, nsoil, z0, czil, lai, csoil, ptu)


      implicit none

!  this subroutine internally sets (defaults), or optionally reads-in
!  via namelist i/o, all the soil and vegetation parameters
!  required for the execusion of the noah - lsm
! 
! optional non-default parameters can be read in, accommodating up
!  to 30 soil, veg, or slope classes, if the default max number of 
!  soil, veg, and/or slope types is reset.

! future upgrades of routine redprm must expand to incorporate some
! of the empirical parameters of the frozen soil and snowpack physics
! (such as in routines frh2o, snowpack, and snow_new) not yet set in 
!  this redprm routine, but rather set in lower level subroutines

!  set maximum number of soil-, veg-, and slopetyp in data statement

      integer max_soiltyp
      integer max_vegtyp
      integer max_slopetyp
      parameter (max_soiltyp  = 30)
      parameter (max_vegtyp   = 30)
      parameter (max_slopetyp = 30)

!  number of defined soil-, veg-, and slopetyps used

      integer defined_veg
      integer defined_soil
      integer defined_slope
      data defined_veg/13/
      data defined_soil/9/
      data defined_slope/9/

!  set-up soil parameters for given soil type
!  input: soltyp: soil type (integer index)
!  output: soil parameters:

!    maxsmc: max soil moisture content (porosity)
!    refsmc: reference soil moisture (onset of soil moisture
!            stress in transpiration)
!    wltsmc: wilting pt soil moisture contents
!    drysmc: air dry soil moist content limits
!    satpsi: saturated soil potential
!    satdk:  saturated soil hydraulic conductivity
!    bb:     the 'b' parameter
!    satdw:  saturated soil diffusivity
!    f11:    used to compute soil diffusivity/conductivity
!    quartz:  soil quartz content
!
! soil types   zobler (1986)      cosby et al (1984) (quartz cont.(1))
!  1        coarse            loamy sand         (0.82)
!  2        medium            silty clay loam    (0.10)
!  3        fine              light clay         (0.25)
!  4        coarse-medium     sandy loam         (0.60)
!  5        coarse-fine       sandy clay         (0.52)
!  6        medium-fine       clay loam          (0.35)
!  7        coarse-med-fine   sandy clay loam    (0.60)
!  8        organic           loam               (0.40)
!  9        glacial land ice  loamy sand         (na using 0.82)

      real bb(max_soiltyp)
      real drysmc(max_soiltyp)
      real f11(max_soiltyp)
      real maxsmc(max_soiltyp)
      real refsmc(max_soiltyp)
      real satpsi(max_soiltyp)
      real satdk(max_soiltyp)
      real satdw(max_soiltyp)
      real wltsmc(max_soiltyp)
      real qtz(max_soiltyp)

      real b
      real dksat
      real dwsat
      real smcmax
      real smcwlt
      real smcref
      real smcdry
      real ptu
      real f1
      real quartz
      real refsmc1
      real wltsmc1

      data maxsmc/0.421, 0.464, 0.468, 0.434, 0.406, 0.465,  &
                  0.404, 0.439, 0.421, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data satpsi/0.04, 0.62, 0.47, 0.14, 0.10, 0.26,  &
                  0.14, 0.36, 0.04, 0.00, 0.00, 0.00,  &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,  &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,  &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/
      data satdk /1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,  &
                  0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5, 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      data bb    /4.26,  8.72, 11.55, 4.74, 10.73,  8.17,  &
                  6.77,  5.25,  4.26, 0.00,  0.00,  0.00,  &
                  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,  &
                  0.00,  0.00,  0.00, 0.00,  0.00,  0.00,  &
                  0.00,  0.00,  0.00, 0.00,  0.00,  0.00/
      data qtz   /0.82, 0.10, 0.25, 0.60, 0.52, 0.35,  &
                  0.60, 0.40, 0.82, 0.00, 0.00, 0.00,  &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,  &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00,  &
                  0.00, 0.00, 0.00, 0.00, 0.00, 0.00/

! the following 5 parameters are derived later in redprm.f 
! from the soil data, and are just given here for reference 
! and to force static storage allocation
! dag lohmann, feb. 2001

      data refsmc/0.283, 0.387, 0.412, 0.312, 0.338, 0.382,  &
                  0.315, 0.329, 0.283, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data wltsmc/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,  &
                  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data drysmc/0.029, 0.119, 0.139, 0.047, 0.100, 0.103,  &
                  0.069, 0.066, 0.029, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                  0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data satdw /5.71e-6, 2.33e-5, 1.16e-5, 7.95e-6, 1.90e-5,  &
                  1.14e-5, 1.06e-5, 1.46e-5, 5.71e-6, 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00,  &
                  0.00   , 0.00   , 0.00   , 0.00   , 0.00/
      data f11  /-0.999, -1.116, -2.137, -0.572, -3.201, -1.302,  &
                 -1.519, -0.329, -0.999,  0.000,  0.000,  0.000,  &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,  &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000,  &
                  0.000,  0.000,  0.000,  0.000,  0.000,  0.000/

!#######################################################################

!  set-up vegetation parameters for a given vegetaion type
!
!  input: vegtyp = vegetation type (integer index)
!  ouput: vegetation parameters
!         shdfac: vegetation greenness fraction
!         rcmin:  mimimum stomatal resistance
!         rgl:    parameter used in solar rad term of
!                 canopy resistance function
!         hs:     parameter used in vapor pressure deficit term of
!                 canopy resistance function
!         snup:   threshold snow depth (in water equivalent m) that
!                 implies 100% snow cover
!
!  ssib vegetation types (dorman and sellers, 1989; jam)
!
!   1:   broadleaf-evergreen trees  (tropical forest)
!   2:   broadleaf-deciduous trees
!   3:   broadleaf and needleleaf trees (mixed forest)
!   4:   needleleaf-evergreen trees
!   5:   needleleaf-deciduous trees (larch)
!   6:   broadleaf trees with groundcover (savanna)
!   7:   groundcover only (perennial)
!   8:   broadleaf shrubs with perennial groundcover
!   9:   broadleaf shrubs with bare soil
!  10:   dwarf trees and shrubs with groundcover (tundra)
!  11:   bare soil
!  12:   cultivations (the same parameters as for type 7)
!  13:   glacial (the same parameters as for type 11)

      integer nroot_data(max_vegtyp)
      real    rsmtbl(max_vegtyp)
      real    rgltbl(max_vegtyp)
      real    hstbl(max_vegtyp)
      real    snupx(max_vegtyp)
      real    z0_data(max_vegtyp)
      real    lai_data(max_vegtyp)

      integer nroot
      real    shdfac
      real    rcmin
      real    rgl
      real    hs
      real    frzfact
      real    psisat
      real    snup
      real    z0
      real    lai

      data nroot_data /4,4,4,4,4,4,3,3,3,2,3,3,2,0,0,  &
                       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data rsmtbl /150.0, 100.0, 125.0, 150.0, 100.0, 70.0,  &
                    40.0, 300.0, 400.0, 150.0, 400.0, 40.0,  &
                   150.0,   0.0,   0.0,   0.0,   0.0,  0.0,  &
                     0.0,   0.0,   0.0,   0.0,   0.0,  0.0,  &
                     0.0,   0.0,   0.0,   0.0,   0.0,  0.0/
      data rgltbl /30.0,  30.0,  30.0,  30.0,  30.0,  65.0,  &
                  100.0, 100.0, 100.0, 100.0, 100.0, 100.0,  &
                  100.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &
                    0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &
                    0.0,   0.0,   0.0,   0.0,   0.0,   0.0/
      data hstbl /41.69, 54.53, 51.93, 47.35,  47.35, 54.53,  &
                  36.35, 42.00, 42.00, 42.00,  42.00, 36.35,  &
                  42.00,  0.00,  0.00,  0.00,   0.00,  0.00,  &
                   0.00,  0.00,  0.00,  0.00,   0.00,  0.00,  &
                   0.00,  0.00,  0.00,  0.00,   0.00,  0.00/
      data snupx  /0.080, 0.080, 0.080, 0.080, 0.080, 0.080,  &
                   0.040, 0.040, 0.040, 0.040, 0.025, 0.040,  &
                   0.025, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                   0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
      data z0_data /2.653, 0.826, 0.563, 1.089, 0.854, 0.856,  &
                    0.035, 0.238, 0.065, 0.076, 0.011, 0.035,  &
                    0.011, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  &
                    0.000, 0.000, 0.000, 0.000, 0.000, 0.000/
!      data lai_data /3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
!     *               3.0, 3.0, 3.0, 3.0, 3.0, 3.0,
!     *               3.0, 0.0, 0.0, 0.0, 0.0, 0.0,
!     *               0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
!     *               0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
      data lai_data /4.0, 4.0, 4.0, 4.0, 4.0, 4.0,  &
                     4.0, 4.0, 4.0, 4.0, 4.0, 4.0,  &
                     4.0, 0.0, 0.0, 0.0, 0.0, 0.0,  &
                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0,  &
                     0.0, 0.0, 0.0, 0.0, 0.0, 0.0/

!#######################################################################

!  class parameter 'slopetyp' was included to estimate
!  linear reservoir coefficient 'slope' to the baseflow runoff
!  out of the bottom layer. lowest class (slopetyp=0)means
!  highest slope parameter= 1
!  definition of slopetyp from 'zobler' slope type
!  slope class      percent slope
!  1                0-8
!  2                8-30
!  3                > 30
!  4                0-30
!  5                0-8 & > 30
!  6                8-30 & > 30
!  7                0-8, 8-30, > 30
!  9                glacial ice
!  blank            ocean/sea
!  note:  class 9 from 'zobler' file should be replaced by 8
!  and 'blank'  9

      real slope
      real slope_data(max_slopetyp)
      data slope_data /0.1,  0.6, 1.0, 0.35, 0.55, 0.8,  &
                       0.63, 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0,  &
                       0.0 , 0.0, 0.0, 0.0,  0.0,  0.0/

!#######################################################################

!  set namelist file name

      character*50 namelist_name

!#######################################################################

! set universal parameters (not dependent on soil, veg, slope type)

      integer vegtyp
      integer soiltyp
      integer slopetyp

      integer nsoil
      integer i

      integer bare
      data    bare /11/

      logical lparam
      data    lparam /.true./

      logical lfirst
      data    lfirst /.true./

!  parameter used to calculate roughness length of heat
      real czil, czil_data
      data czil_data /0.2/

!  parameter used to caluculate vegetation effect on soil heat flux
      real sbeta, sbeta_data
      data sbeta_data /-2.0/

! bare soil evaporation exponent used in devap

      real fxexp, fxexp_data
      data fxexp_data /2.0/

! soil heat capacity [j/m^3/k]

      real csoil, csoil_data
      data csoil_data /1.26e+6/

!  specify snow distribution shape parameter
!  salp   - shape parameter of distribution function
!  of snow cover. from anderson's data (hydro-17)
!  best fit is when salp = 2.6
      real salp, salp_data
      data salp_data /2.6/

!  kdt is defined by reference refkdt and dksat
!  refdk=2.e-6 is the sat. dk. value for the soil type 2
      real refdk, refdk_data
      data refdk_data /2.0e-6/

      real refkdt, refkdt_data
      data refkdt_data /3.0/

      real kdt
      real frzx

!  frozen ground parameter, frzk, definition
!  frzk is ice content threshold above which frozen soil is impermeable
!  reference value of this parameter for the light clay soil (type=3)
!  frzk = 0.15 m
      real frzk, frzk_data
      data frzk_data /0.15/

      real rtdis(nsoil)
      real sldpth(nsoil)
      real zsoil(nsoil)

!  set two canopy water parameters
      real cfactr, cfactr_data
      real cmcmax, cmcmax_data
      data cfactr_data /0.5/
      data cmcmax_data /0.5e-3/

!  set max. stomatal resistance
      real rsmax, rsmax_data
      data rsmax_data /5000.0/

!  set optimum transpiration air temperature
      real topt, topt_data
      data topt_data /298.0/

!  specify depth[m] of lower boundary soil temperature
      real zbot, zbot_data
      data zbot_data /-3.0/

!#######################################################################

!  namelist definition

      namelist /soil_veg/ slope_data, rsmtbl, rgltbl, hstbl, snupx,  &
           bb, drysmc, f11, maxsmc, refsmc, satpsi, satdk, satdw,  &
           wltsmc, qtz, lparam, zbot_data, salp_data, cfactr_data,  &
           cmcmax_data, sbeta_data, rsmax_data, topt_data,  &
           refdk_data, frzk_data, bare, defined_veg, defined_soil,  &
           defined_slope, fxexp_data, nroot_data, refkdt_data, z0_data,  &
           czil_data, lai_data, csoil_data

!  read namelist file to override default parameters
!  only once.
!
!  7/6/01 : e. rogers commented out read of unit 58 since
!           nco does not allow hardwired file names in the code.

      if (lfirst) then
!        open(58, file = 'namelist_filename.txt')
! namelist_name must be 50 characters or less.
!        read(58,'(a)') namelist_name
!        close(58)
!         write(*,*) 'namelist filename is ', namelist_name
!        open(59, file = namelist_name)
 50      continue
!hai            read(59, soil_veg, end=100)
!         if (lparam) goto 50
 100     continue
!        close(59)
!         write(*,nml=soil_veg)
         lfirst = .false.
         if (defined_soil .gt. max_soiltyp) then
            write(*,*) 'warning: defined_soil too large in namelist'
            stop 222
         end if
         if (defined_veg .gt. max_vegtyp) then
            write(*,*) 'warning: defined_veg too large in namelist'
            stop 222
         end if
         if (defined_slope .gt. max_slopetyp) then
            write(*,*) 'warning: defined_slope too large in namelist'
            stop 222
         end if

         do i = 1, defined_soil
            satdw(i)  = bb(i)*satdk(i)*(satpsi(i)/maxsmc(i))
            f11(i)    = alog10(satpsi(i)) + bb(i)*alog10(maxsmc(i)) + 2.0
            refsmc1   = maxsmc(i)*(5.79e-9/satdk(i))  &
                                          **(1.0/(2.0*bb(i)+3.0))
            refsmc(i) = refsmc1 + (maxsmc(i)-refsmc1) / 3.0
            wltsmc1   = maxsmc(i) * (200.0/satpsi(i))**(-1.0/bb(i))
            wltsmc(i) = wltsmc1 - 0.5 * wltsmc1
! current version drysmc values that equate to wltsmc
! future version could let drysmc be independently set via namelist 
            drysmc(i) = wltsmc(i)
         end do

      end if

      if (soiltyp .gt. defined_soil) then
         write(*,*) 'warning: too many soil types'
         stop 333
      end if
      if (vegtyp .gt. defined_veg) then
         write(*,*) 'warning: too many veg types'
         stop 333
      end if
      if (slopetyp .gt. defined_slope) then
         write(*,*) 'warning: too many slope types'
         stop 333
      end if

!  set-up universal parameters 
! (not dependent on soiltyp, vegtyp or slopetyp)
      zbot   = zbot_data
      salp   = salp_data
      cfactr = cfactr_data
      cmcmax = cmcmax_data
      sbeta  = sbeta_data
      rsmax  = rsmax_data
      topt   = topt_data
      refdk  = refdk_data
      frzk   = frzk_data
      fxexp  = fxexp_data
      refkdt = refkdt_data
      czil   = czil_data
      csoil  = csoil_data

!  set-up soil parameters
      b       = bb(soiltyp)
      smcdry  = drysmc(soiltyp)
      f1      = f11(soiltyp)
      smcmax  = maxsmc(soiltyp)
      smcref  = refsmc(soiltyp)
      psisat  = satpsi(soiltyp)
      dksat   = satdk(soiltyp)
      dwsat   = satdw(soiltyp)
      smcwlt  = wltsmc(soiltyp)
      quartz  = qtz(soiltyp)
      frzfact = (smcmax / smcref) * (0.412 / 0.468)
      kdt     = refkdt * dksat/refdk

!  to adjust frzk parameter to actual soil type: frzk * frzfact

      frzx = frzk * frzfact

!  set-up vegetation parameters
      nroot = nroot_data(vegtyp)
      snup  = snupx(vegtyp)
      rcmin = rsmtbl(vegtyp)
      rgl   = rgltbl(vegtyp)
      hs    = hstbl(vegtyp)
      z0    = z0_data(vegtyp)
      lai   = lai_data(vegtyp)
      if(vegtyp .eq. bare) shdfac = 0.0

      if (nroot .gt. nsoil) then
         write(*,*) 'warning: too many root layers'
         stop 333
      end if

!  calculate root distribution
!  present version assumes uniform distribution based on soil layers

      do i=1,nroot
         rtdis(i) = -sldpth(i)/zsoil(nroot)
      end do

!  set-up slope parameter
      slope = slope_data(slopetyp)
!
      return
      end






      subroutine rosr12 ( p, a, b, c, d, delta, nsoil )

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to invert (solve) the tri-diagonal matrix problem shown
!c    =======   below:
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer k
      integer kk
      integer nsoil
      
      real p     (nsoil)
      real a     (nsoil)
      real b     (nsoil)
      real c     (nsoil)
      real d     (nsoil)
      real delta (nsoil)
      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     initialize eqn coef c for the lowest soil layer.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      c(nsoil) = 0.0

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     solve the coefs for the 1st soil layer
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      p(1) = -c(1) / b(1)
      delta(1) = d(1) / b(1)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     solve the coefs for soil layers 2 thru nsoil
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 2 , nsoil
        p(k) = -c(k) * ( 1.0 / (b(k) + a (k) * p(k-1)) )
        delta(k) = (d(k)-a(k)*delta(k-1))*(1.0/(b(k)+a(k)*p(k-1)))
      end do

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     set p to delta for lowest soil layer.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      p(nsoil) = delta(nsoil)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     adjust p for soil layers 2 thru nsoil
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 2 , nsoil
         kk = nsoil - k + 1
         p(kk) = p(kk) * p(kk+1) + delta(kk)
      end do

      return
      end









      subroutine shflx(s,stc,smc,smcmax,nsoil,t1,dtk,yy,zz1,zsoil,tbot,  &
           zbot, smcwlt, psisat, sh2o, b,f1,df1,ice,quartz,csoil)
      
!      implicit none

      USE MPPSTAFF
      USE CTLBLK
      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  update the temperature state of the soil column based on
!c              the thermal diffusion equation and update the frozen soil
!c              moisture content based on the temperature.
!c      
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsold
      parameter ( nsold = 20 )

      integer i
      integer ice
      integer ifrz
      integer nsoil

      real b
      real df1
      real csoil
      real dtk
      real f1
      real psisat
      real quartz
      real rhsts ( nsold )
      real s
      real smc   ( nsoil )
      real sh2o  ( nsoil )
      real smcmax
      real smcwlt
      real stc        (nsoil)
      real stcf        (nsold)
      real t0
      real t1
      real tbot
      real zbot
      real yy
      real zsoil ( nsoil )
      real zz1

      parameter ( t0 = 273.15)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     hrt routine calcs the right hand side of the soil temp dif eqn
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      if(ice.eq.1) then

!..sea-ice case

         call hrtice(rhsts,stc,nsoil,zsoil,yy,zz1,df1)

         call hstep (stcf,stc,rhsts,dtk,nsoil)
         
      else

!..land-mass case

         call hrt(rhsts,stc,smc,smcmax,nsoil,zsoil,yy,zz1,tbot,  &
              zbot, psisat, sh2o, dtk,  &
              b,f1,df1,quartz,csoil)
         call hstep(stcf,stc,rhsts,dtk,nsoil)
      endif

      do i = 1,nsoil
         stc(i)  = stcf(i)
      end do
      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     in the no snowpack case (via routine nopac branch,) update the
!     grnd (skin) temperature here in response to the updated soil 
!     temperature profile above.
! (note: inspection of routine snopac shows that t1 below is a dummy
!     variable only, as skin temperature is updated differently
!     in routine snopac) 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      t1 = (yy + (zz1 - 1.0) * stc(1)) / zz1
!m      if(t1 .lt. 0) then 
!m        print *,"t1k:",t1,yy,zz1,stc(1),zz1,ice
!m      endif

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the sfc soil heat flux
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!      s = df1 * (stc(1) - t1) / (0.5 * zsoil(1))

      return
      end




      subroutine smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,  &
           sh2o, slope, kdt, frzfact,  &
           smcmax,b,pc,smcwlt,dksat,dwsat,smcref,shdfac,cmcmax,  &
           smcdry,cfactr, runoff1,runoff2, runoff3, edir1, ec1,   &
           ett1, sfctmp,q2,nroot,rtdis, fxexp)
!
    USE RITE    , RUNOXX3 => RUNOFF3
    USE ABCI, ONLY: NSOLD


      implicit none

! ------------    frozen ground version    --------------------------
!   new states added: sh2o, and frozen groud correction factor, frzfact
!   and parameter slope 
!

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate soil moisture flux.  the soil moisture
!c    =======   content (smc - a per unit volume measurement) is a
!c              dependent variable that is updated with prognostic eqns.
!c              the canopy moisture content (cmc) is also updated.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer k
      integer nsoil
      real b
      real cfactr
      real cmc
      real cmcmax
      real dksat
      real dt
      real et     ( nsold )
      real eta1
      real etp1
      real excess
      real fxexp
      real kdt
      real pc
      real dwsat
      real pcpdrp
      real prcp1
      real rhsct
      real rhstt  ( nsold )
      real rtdis (nsoil)
      real runoff,runoff3
      real shdfac
      real smc    ( nsoil )

! ---------------    frozen ground version     ---------------------
      
      real sh2o   ( nsoil )
      real sice   ( nsold )
      real sh2oa  ( nsold )
      real sh2ofg ( nsold )
! -------------------------------------------------------------------
           
      real smcdry
      real smcmax
      real smcref
      real smcwlt
      real trhsct
      real zsoil  ( nsoil )

! temperature criteria for snowfall tfreez should have 
! same value as in sflx.f
      real tfreez
      parameter (tfreez = 273.15)

      real slope, frzfact, runoff1, runoff2, edir1, ec1
      real ett1, sfctmp, q2, dummy, cmc2ms, devap

      integer nroot, i

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     executable code begins here....if the potential evapotrans-
!     piration is greater than zero...
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dummy=0.
      edir = 0.
      ec = 0.
      ett = 0.
      do k = 1, nsoil
         et ( k ) = 0.
      end do
      
! ----------------------------------------------------------------------
      if ( etp1 .gt. 0.0 ) then

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       retrieve direct evaporation from soil surface
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! ----------------------------------------------------------------------
! call this function only if veg cover not complete
! --------------     frozen ground version     ---------------------
!   smc states were replaced by sh2o states
!
        if (shdfac .lt. 1.) then
          edir = devap ( etp1, sh2o(1), zsoil(1), shdfac, smcmax,  &
            b, dksat, dwsat, smcdry,smcref, smcwlt, fxexp)
        endif
! ----------------------------------------------------------------------
!       initialize plant total transpiration, retrieve plant
!       transpiration, and accumulate it for all soil layers.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!        ett = 0.
         
        if(shdfac.gt.0.0) then
        
! ----------------------------------------------------------------------
! --------------     frozen ground version     ---------------------
!   smc states were replaced by sh2o states
!
          call transp ( et,nsoil,etp1,sh2o,cmc,zsoil,shdfac,smcwlt,  &
            cmcmax,pc,cfactr,smcref,sfctmp,q2,nroot,rtdis)
          
          do k = 1 , nsoil
            ett = ett + et ( k )
          end do
! move this endif after canopy evap calcs since cmc=0 for shdfac=0
!        endif

! ----------------------------------------------------------------------
!       calculate canopy evaporation
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cc if statements to avoid tangent linear problems near cmc=zero
          if (cmc .gt. 0.0) then
            ec = shdfac * ( ( cmc / cmcmax ) ** cfactr ) * etp1
          else
            ec = 0.0
          endif
! ----------------------------------------------------------------------
!########  ec should be limited by the total amount of available
!          water on the canopy. modified by f.chen on 10/18/94
!########
          cmc2ms = cmc / dt
          ec = min ( cmc2ms, ec )
        endif
      endif

! ----------------------------------------------------------------------
!     total up evap and transp types to obtain actual evapotransp
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      edir1=edir
      ec1=ec
      ett1=ett
      
      eta1 = edir + ett + ec
      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     compute the right hand side of the canopy eqn term ( rhsct )
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      rhsct = shdfac * prcp1 - ec

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     convert rhsct (a rate) to trhsct (an amt) and add it to existing
!     cmc. if resulting amt exceeds max capacity, it becomes drip
!     and will fall to the grnd.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      drip = 0.
      trhsct = dt * rhsct
      excess =  cmc + trhsct
      if ( excess .gt. cmcmax ) drip = excess - cmcmax

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     pcpdrp is the combined prcp1 and drip (from cmc) that
!     goes into the soil
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      pcpdrp = (1. - shdfac) * prcp1 + drip / dt

!      print*,' ################ smlx ##################'
!      print*,' pcpdrp=', pcpdrp, ' edir=', edir,' et=', et,
!     *      'smc(1)=', smc(1), 'smc(2)=', smc(2), ' prcp1=', prcp1,
!     *      'drip = ', drip / dt

! ---------------     frozen ground version     --------------------
!    store ice content at each soil layer before calling srt & sstep
!
      do i = 1,nsoil
         sice(i) = smc(i) - sh2o(i)
      end do
! ------------------------------------------------------------------
            
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     call subroutines srt and sstep to solve the soil moisture
!     tendency equations. 
!
!  if the infiltrating precip rate is nontrivial,
!
!    (we consider nontrivial to be a precip total over the time step 
!     exceeding one one-thousandth of the water holding capacity of 
!     the first soil layer)
! 
!  then call the srt/sstep subroutine pair twice in the manner of 
!    time scheme "f" (implicit state, averaged coefficient)
!    of section 2 of kalnay and kanamitsu (1988, mwr, vol 116, 
!    pages 1945-1958)to minimize 2-delta-t oscillations in the 
!    soil moisture value of the top soil layer that can arise because
!    of the extreme nonlinear dependence of the soil hydraulic 
!    diffusivity coefficient and the hydraulic conductivity on the
!    soil moisture state
!
!  otherwise call the srt/sstep subroutine pair once in the manner of
!    time scheme "d" (implicit state, explicit coefficient) 
!    of section 2 of kalnay and kanamitsu
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! pcpdrp is units of kg/m**2/s or mm/s, zsoil is negative depth in m 
!......if ( pcpdrp .gt. 0.0 ) then

      if ( (pcpdrp*dt) .gt. (0.001*1000.0*(-zsoil(1))*smcmax) ) then

! ---------------    frozen ground version       ---------------------
!    smc states replaced by sh2o states in srt subr.
!    sh2o & sice states included in sstep subr.
!    frozen ground correction factor, frzfact, added
!    all water balance calculations using unfrozen water
!
         call srt ( rhstt,runoff,edir,et,sh2o,sh2o,nsoil,pcpdrp,zsoil,  &
              dwsat,dksat,smcmax, b, runoff1,   &
              runoff2,dt,smcwlt,slope,kdt,frzfact, sice)

         call sstep ( sh2ofg,sh2o,dummy,rhstt,rhsct,dt,nsoil,smcmax,  &
              cmcmax, runoff3, zsoil, smc, sice )
         
         do k = 1, nsoil
            sh2oa(k) = ( sh2o(k) + sh2ofg(k) ) * 0.5
         end do
        
         call srt ( rhstt,runoff,edir,et,sh2o,sh2oa,nsoil,pcpdrp,zsoil,  &
              dwsat,dksat,smcmax, b, runoff1,  &
              runoff2,dt,smcwlt,slope,kdt,frzfact, sice)
         
         call sstep ( sh2o,sh2o,cmc,rhstt,rhsct,dt,nsoil,smcmax,  &
              cmcmax, runoff3, zsoil,smc,sice)
         
      else
         
         call srt ( rhstt,runoff,edir,et,sh2o,sh2o,nsoil,pcpdrp,zsoil,  &
              dwsat,dksat,smcmax, b, runoff1,  &
              runoff2,dt,smcwlt,slope,kdt,frzfact, sice)
         
         
         call sstep ( sh2o,sh2o,cmc,rhstt,rhsct,dt,nsoil,smcmax,  &
              cmcmax, runoff3, zsoil,smc,sice)
         
      endif

     
      runof = runoff
      return
      end
      function snksrc ( tup,tm,tdn, smc, sh2o, zsoil,nsoil,  &
           smcmax, psisat, b, dt, k, qtot) 
      
      implicit none
      
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate sink/source term of the termal diffusion
!c    =======   equation. (sh2o) is available liqued water.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer  k
      integer  nsoil
      
      real b
      real df
      real dfh2o
      real dfice
      real dh2o
      real dt
      real dz
      real dzh
      real free
      real frh2o
      real hlice
      real psisat
      real qtot
      real sh2o
      real smc
      real smcmax
      real snksrc
      real t0
      real tavg
      real tdn
      real tm
      real tup
      real tz
      real x0
      real xdn
      real xh2o
      real xup
      real zsoil (nsoil)

      parameter (hlice=3.3350e5)
      parameter (dh2o =1.0000e3)
      parameter (  t0 =2.7315e2)
      
      if(k.eq.1) then
        dz=-zsoil(1)
      else
        dz=zsoil(k-1)-zsoil(k)
      endif

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calculate potential reduction of liqued water content
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      xh2o=qtot*dt/(dh2o*hlice*dz) + sh2o
     
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     estimate unfrozen water at temperature tavg,
!     and check if calculated water content is reasonable 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
        
!  ####   new calculation of average temperature (tavg)   ##########
!  ####   in freezing/thawing layer using up, down, and middle   ###
!  ####   layer temperatures (tup, tdn, tm)               ##########
   
      dzh=dz*0.5

      if (tup .lt. t0) then

        if (tm .lt. t0) then

          if (tdn .lt. t0) then

!           *** tup, tm, tdn < t0 ***

            tavg = (tup + 2.0*tm + tdn)/ 4.0
            
          else

!           *** tup & tm < t0,  tdn >= t0 ***

            x0 = (t0 - tm) * dzh / (tdn - tm)
            tavg = 0.5 * (tup*dzh+tm*(dzh+x0)+t0*(2.*dzh-x0)) / dz
                       
          endif      

        else
        
          if (tdn .lt. t0) then

!           *** tup < t0, tm >= t0, tdn < t0 ***

            xup  = (t0-tup) * dzh / (tm-tup)
            xdn  = dzh - (t0-tm) * dzh / (tdn-tm)
            tavg = 0.5 * (tup*xup+t0*(2.*dz-xup-xdn)+tdn*xdn) / dz

          else

!           *** tup < t0, tm >= t0, tdn >= t0 ***

            xup  = (t0-tup) * dzh / (tm-tup)
            tavg = 0.5 * (tup*xup+t0*(2.*dz-xup)) / dz
                      
          endif   
        
        endif

      else

        if (tm .lt. t0) then

          if (tdn .lt. t0) then

!           *** tup >= t0, tm < t0, tdn < t0 ***

            xup  = dzh - (t0-tup) * dzh / (tm-tup)
            tavg = 0.5 * (t0*(dz-xup)+tm*(dzh+xup)+tdn*dzh) / dz
                      
          else

!           *** tup >= t0, tm < t0, tdn >= t0 ***

            xup  = dzh - (t0-tup) * dzh / (tm-tup)
            xdn  = (t0-tm) * dzh / (tdn-tm)
            tavg = 0.5 * (t0*(2.*dz-xup-xdn)+tm*(xup+xdn)) / dz
                                   
          endif   

        else

          if (tdn .lt. t0) then

!           *** tup >= t0, tm >= t0, tdn < t0 ***

            xdn  = dzh - (t0-tm) * dzh / (tdn-tm)
            tavg = (t0*(dz-xdn)+0.5*(t0+tdn)*xdn) / dz
                 
          else

!           *** tup >= t0, tm >= t0, tdn >= t0 ***

            tavg = (tup + 2.0*tm + tdn) / 4.0
                      
          endif           

        endif

      endif                      

      free=frh2o(tavg, smc, sh2o, smcmax, b, psisat )

      if ( xh2o .lt. sh2o .and. xh2o .lt. free) then 
         if ( free .gt. sh2o ) then
              xh2o = sh2o
          else
              xh2o = free
          endif
      endif
              
      if ( xh2o .gt. sh2o .and. xh2o .gt. free )  then
         if ( free .lt. sh2o ) then
              xh2o = sh2o
          else
              xh2o = free
          endif
      endif 

      if(xh2o .lt. 0. ) xh2o=0.
      if(xh2o .gt. smc) xh2o=smc

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calculate sink/source term and replace previous water content 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     
      snksrc=-dh2o*hlice*dz*(xh2o-sh2o)/dt
      sh2o=xh2o
      
77    return
      end



      subroutine snopac (etp,eta,prcp,prcp1,snowng,smc,smcmax,smcwlt,  &
        smcref, smcdry, cmc, cmcmax, nsoil, dt, sbeta, q1, df1,  &
        q2,t1,sfctmp,t24,th2,f,f1,s,stc,epsca,sfcprs,  &
!     &  b, pc, rch, rr, cfactr, salp, esd,  &
        b, pc, rch, rr, cfactr, sncover, esd, sndens,  &
        snowh, sh2o, slope, kdt, frzfact, psisat,snup,  &
        zsoil, dwsat, dksat, tbot, zbot, shdfac, runoff1,  &
        runoff2,runoff3,edir1,ec1,ett1,nroot,snmax,ice,  &
        rtdis,quartz, fxexp,csoil)
!
    USE RITE    , RUNOXX3 => RUNOFF3
!
      implicit none

! ----------------------------------------------------------------------
!c    purpose:  to calculate soil moisture and heat flux values & update
!c    =======   soil moisture content and soil heat content values for
!c              the case when a snow pack is present.
! ----------------------------------------------------------------------

      integer ice
      integer nroot
      integer nsoil

      logical snowng

      real b
      real cfactr
      real cmc
      real cmcmax
      real cp
      real cph2o
      real cpice
      real csoil
      real denom

      real df1
      real dksat

      real dsoil
      real dtot
      real dt
      real dwsat


      real epsca
      real esd
      real expsno
      real expsoi
      real eta
      real eta1
      real etp
      real etp1
      real etp2

      real ex
      real expfac
      real f
      real fxexp

      real f1
      real kdt
      real lsubf
      real lsubc
      real lsubs
      real pc
      real prcp
      real prcp1
      real q1
      real q2
      real rch

      real rr
      real rtdis   ( nsoil )
      real runoff
      real s
      real sbeta
      real s1
      real sfctmp
      real shdfac
      real sigma
      real smc     ( nsoil )
      real sh2o    ( nsoil )
      real smcdry
      real smcmax
      real smcref
      real smcwlt
      real snmax
      real snowh
      real stc     ( nsoil )
      real t1
      real t11
      real t12
      real t12a
      real t12b
      real t24
      real tbot
      real zbot
      real th2
      real yy
      real zsoil( nsoil )
      real zz1
!
      real  salp, sfcprs, slope, frzfact, psisat, snup
      real runoff1, runoff2, runoff3
      real edir1, ec1, ett1, quartz
      real sndens, sncond, rsnow, sncover, qsat, etp3, seh, t14
      real csnow
     
      parameter(cp=1004.5,cph2o=4.218e+3,cpice=2.106e+3,  &
        lsubf=3.335e+5,lsubc=2.501000e+6,lsubs=2.83e+6,sigma=5.67e-8)

      real,parameter::  tfreez = 273.15

! ----------------------------------------------------------------------
! executable code begins here...
! convert potential evap (etp) from kg m-2 s-1 to m s-1 and then to an
! amount (m) given timestep (dt) and call it an effective snowpack
! reduction amount, etp2 (m).  this is the amount the snowpack would be
! reduced due to evaporation from the snow sfc during the timestep.
! evaporation will proceed at the potential rate unless the snow depth
! is less than the expected snowpack reduction.
! if seaice (ice=1), beta remains=1.
! ----------------------------------------------------------------------
      prcp1 = prcp1*0.001

      etp2 = etp * 0.001 * dt
      beta = 1.0
      if(ice .ne. 1) then
        if (esd .lt. etp2) then
          beta = esd / etp2
        endif
      endif

! ----------------------------------------------------------------------
! if etp<0 (downward) then dewfall (=frostfall in this case).
! ----------------------------------------------------------------------
      dew = 0.0
      if (etp .lt. 0.0) then
        dew = -etp * 0.001
      endif

! ----------------------------------------------------------------------
! if precip is falling, calculate heat flux from snow sfc to newly
! accumulating precip.  note that this reflects the flux appropriate for
! the not-yet-updated skin temperature (t1).  assumes temperature of the
! snowfall striking the gound is =sfctmp (lowest model level air temp).
! ----------------------------------------------------------------------
      flx1 = 0.0
      if ( snowng ) then
        flx1 = cpice * prcp * ( t1 - sfctmp )
      else
        if (prcp .gt. 0.0) flx1 = cph2o * prcp * (t1 - sfctmp)
      endif
      dsoil = -(0.5 * zsoil(1))
      dtot = snowh + dsoil

! ----------------------------------------------------------------------
! calculate an 'effective snow-grnd sfc temp' (t12) based on heat fluxes
! between the snow pack and the soil and on net radiation.
! include flx1 (precip-snow sfc) and flx2 (freezing rain latent heat)
! fluxes.  flx1 from above, flx2 brought in via commom block rite.
! flx2 reflects freezing rain latent heat flux using t1 calculated in
! penman.
! ----------------------------------------------------------------------
      denom = 1.0 + df1 / ( dtot * rr * rch )
      t12a = ((f - flx1 - flx2 - sigma * t24) /  &
             rch+th2-sfctmp-beta*epsca) / rr
      t12b = df1 * stc(1) / ( dtot * rr * rch )
      t12 = (sfctmp + t12a + t12b ) / denom      

! ----------------------------------------------------------------------
! if the 'effective snow-grnd sfc temp' is at or below freezing, no snow
! melt will occur.  set the skin temp to this effective temp and set the
! effective precip to zero.
! ----------------------------------------------------------------------
      if (t12 .le. tfreez) then
        esd = max(0.0, esd-etp2)

!ggg    update snow depth.
        snowh = esd / sndens
!ggg

        t1 = t12
! ----------------------------------------------------------------------
! update soil heat flux (s) using new skin temperature (t1)
        s = df1 * ( t1 - stc(1) ) / ( dtot )
        flx3 = 0.0
        ex = 0.0
        snmax = 0.0

! ----------------------------------------------------------------------
! if the 'effective snow-grnd sfc temp' is above freezing, snow melt
! will occur.  call the snow melt rate,ex and amt, snmax.  revise the
! effective snow depth.  revise the skin temp because it would have chgd
! due to the latent heat released by the melting. calc the latent heat
! released, flx3. set the effective precip, prcp1 to the snow melt rate,
! ex for use in smflx.  adjustment to t1 to account for snow patches.
! ----------------------------------------------------------------------
      else
!        if ( (snup .gt. 0.0) .and. (esd .lt. snup) ) then
! turn off this block below since sncover is calculated (as snofac) in
! sflx and now passed to snopac
!        if (esd .lt. snup) then
!          rsnow = esd / snup
!          sncover = 1.- (exp(-salp*rsnow)-rsnow*exp(-salp))
!        else
!          sncover = 1.
!        endif  
        t1 = tfreez * sncover + t12 * ( 1.0 - sncover )
        qsat = (0.622*6.11e2)/(sfcprs-0.378*6.11e2)
        etp = rch*(qsat-q2)/cp
        etp2 = etp*0.001*dt
        beta = 1.0
        
! ----------------------------------------------------------------------
! if potential evap (sublimation) greater than depth of snowpack.
! beta<1
! ----------------------------------------------------------------------
        if ( esd .le. etp2 ) then
          beta = esd / etp2
          esd = 0.0

!ggg      snow pack has sublimated, set depth to zero
          snowh = 0.0
!ggg

          snmax = 0.0
          ex = 0.0
! ----------------------------------------------------------------------
! update soil heat flux (s) using new skin temperature (t1)
          s = df1 * ( t1 - stc(1) ) / ( dtot )
          
! ----------------------------------------------------------------------
! potential evap (sublimation) less than depth of snowpack, beta=1.
! snowpack (esd) reduced by pot evap rate
! etp3 (convert to flux)
! update soil heat flux because t1 previously changed.
! snowmelt reduction depending on snow cover
! if snow cover less than 5% no snowmelt reduction
! ----------------------------------------------------------------------
        else
!          esd = max(0.0, esd-etp2)
          esd = esd-etp2

!ggg      snow pack reduced by sublimation, reduce snow depth
          snowh = esd / sndens
!ggg

          etp3 = etp*lsubc
          s = df1 * ( t1 - stc(1) ) / ( dtot )
          seh = rch*(t1-th2)
          t14 = t1*t1
          t14 = t14*t14
          flx3 = f - flx1 - flx2 - sigma*t14 - s - seh - etp3
          if(flx3.le.0.0) flx3=0.0
          ex = flx3*0.001/lsubf
! ----------------------------------------------------------------------
! does below fail to match the melt water with the melt energy?
          if ( sncover .gt. 0.05) ex = ex * sncover
          snmax = ex * dt
        endif
        
! ----------------------------------------------------------------------
! snmax.lt.esd
! else
! ----------------------------------------------------------------------
!        if(snmax.lt.esd) then
! the 1.e-6 value represents a snowpack depth threshold value (0.1 mm)
! below which we choose not to retain any snowpack, and instead include
! it in snowmelt.
        if(snmax.lt.esd-1.e-6) then
          esd = esd - snmax

!ggg      snow melt reduced snow pack, reduce snow depth
          snowh = esd / sndens
!ggg

        else
          ex = esd/dt
          snmax = esd
          esd = 0.0

!ggg      snow melt exceeds snow depth
          snowh = 0.0
!ggg

          flx3 = ex*1000.0*lsubf
        endif
        prcp1 = prcp1 + ex

      endif
         
! ----------------------------------------------------------------------
! set the effective potnl evapotransp (etp1) to zero since snow case so
! surface evap not calculated from edir, ec, or ett in smflx (below).
! if seaice (ice=1) skip call to smflx.
! smflx returns soil moisture values and preliminary values of
! evapotranspiration.  in this, the snow pack case, the prelim values
! (eta1) are not used in subsequent calculation of evap.
! new states added: sh2o, and frozen ground correction factor
! evap equals potential evap unless beta<1.
! ----------------------------------------------------------------------

      etp1 = 0.0
      if (ice .ne. 1) then
        call smflx ( eta1,smc,nsoil,cmc,etp1,dt,prcp1,zsoil,  &
          sh2o, slope, kdt, frzfact,  &
          smcmax,b,pc,smcwlt,dksat,dwsat,  &
          smcref,shdfac,cmcmax,smcdry,cfactr,runoff1,runoff2,  &
          runoff3, edir1, ec1, ett1,sfctmp,q2,nroot,rtdis,  &
          fxexp)

      endif

      eta = beta*etp

! ----------------------------------------------------------------------
! the 'adjusted top soil lyr temp' (yy) and the 'adjusted soil heat
! flux' (zz1) are set to the top soil lyr temp, and 1, respectively.
! these are close-enough approximations because the sfc heat flux to be
! computed in shflx will effectively be the flux at the snow top
! surface.  t11 is a dummy arguement since we will not use its value as
! revised by shflx.
! ----------------------------------------------------------------------
      zz1 = 1.0
      yy = stc(1)-0.5*s*zsoil(1)*zz1/df1
      t11 = t1

! ----------------------------------------------------------------------
! shflx will calc/update the soil temps.  note:  the sub-sfc heat flux 
! (s1) and the skin temp (t11) output from this shflx call are not used 
! in any subsequent calculations. rather, they are dummy variables here 
! in the snopac case, since the skin temp and sub-sfc heat flux are 
! updated instead near the beginning of the call to snopac.
! ----------------------------------------------------------------------

      call shflx(s1,stc,smc,smcmax,nsoil,t11,dt,yy,zz1,zsoil,tbot,  &
        zbot, smcwlt, psisat, sh2o,  &
        b,f1,df1,ice,   &
        quartz,csoil)
      
! ----------------------------------------------------------------------
! snow depth and density adjustment based on snow compaction.
! yy is assumed to be the soil temperture at the top of the soil column.
! ----------------------------------------------------------------------
      if (esd .gt. 0.) then
! --- debug ------------------------------------------------------------
!     write(6,*) 'snopac1:esd,snowh,sndens=',esd,snowh,sndens
! --- debug ------------------------------------------------------------
        call snowpack(esd,dt,snowh,sndens,t1,yy)
! --- debug ------------------------------------------------------------
!        sndens = 0.2
!        snowh = esd/sndens
! --- debug ------------------------------------------------------------
! --- debug ------------------------------------------------------------
!     write(6,*) 'snopac2:esd,snowh,sndens=',esd,snowh,sndens
! --- debug ------------------------------------------------------------
      else
        esd = 0.
        snowh = 0.
        sndens = 0.
        sncond = 1.
      endif

! ----------------------------------------------------------------------
      return
      end
      subroutine snowpack ( w,dts,hc,ds,tsnow,tsoil )

      implicit none

! ##############################################################
! ##  subroutine to calculate compaction of snowpack  under  ###
! ##  conditions of increasing snow density, as obtained     ###
! from an approximate solution of e. anderson's differential ###
!     equation (3.29), noaa technical report nws 19,         ###
!                 by   victor koren   03/25/95               ###
! ##############################################################

! ##############################################################
!  w      is a water equivalent of snow, in m                ###
!  dts    is a time step, in sec                             ###
!  hc     is a snow depth, in m                              ###
!  ds     is a snow density, in g/cm3                        ###
!  tsnow  is a snow surface temperature, k                   ###
!  tsoil  is a soil surface temperature, k                   ###
!      subroutine will return new values of h and ds         ###
! ##############################################################

      integer ipol
      integer j

      real c1, c2, hc, w, dts, ds, tsnow, tsoil, h, wx
      real dt, tsnowx, tsoilx, tavg, b, dsx, dw
      real pexp
      real wxx

      parameter (c1=0.01, c2=21.0)

! ##  conversion into simulation units   ######################### 

      h=hc*100.
      wx=w*100.
      dt=dts/3600.
      tsnowx=tsnow-273.15
      tsoilx=tsoil-273.15

! ##  calculating of average temperature of snow pack              ###

      tavg=0.5*(tsnowx+tsoilx)                                    

! ##  calculating of snow depth and density as a result of compaction
!              ds=ds0*(exp(b*w)-1.)/(b*w)
!              b=dt*c1*exp(0.08*tavg-c2*ds0)
! note: b*w in ds eqn above has to be carefully treated 
! numerically below
! ##  c1 is the fractional increase in density (1/(cm*hr)) 
! ##  c2 is a constant (cm3/g) kojima estimated as 21 cms/g

      if(wx .gt. 1.e-2) then
        wxx = wx
      else
        wxx = 1.e-2
      endif
      b=dt*c1*exp(0.08*tavg-c2*ds)

!.........dsx=ds*((dexp(b*wx)-1.)/(b*wx))
!--------------------------------------------------------------------
!  the function of the form (e**x-1)/x imbedded in above expression
!  for dsx was causing numerical difficulties when the denominator "x"
!  (i.e. b*wx) became zero or approached zero (despite the fact that
!  the analytical function (e**x-1)/x has a well defined limit as 
!  "x" approaches zero), hence below we replace the (e**x-1)/x 
!  expression with an equivalent, numerically well-behaved 
!  polynomial expansion.
! 
!  number of terms of polynomial expansion, and hence its accuracy, 
!  is governed by iteration limit "ipol".
!       ipol greater than 9 only makes a difference on double
!             precision (relative errors given in percent %).
!        ipol=9, for rel.error <~ 1.6 e-6 % (8 significant digits)
!        ipol=8, for rel.error <~ 1.8 e-5 % (7 significant digits)
!        ipol=7, for rel.error <~ 1.8 e-4 % ...

      ipol = 4
      pexp = 0.
      do j = ipol,1,-1
!        pexp = (1. + pexp)*b*wx/real(j+1) 
        pexp = (1. + pexp)*b*wxx/real(j+1) 
      end do 
      pexp = pexp + 1.
!
      dsx=ds*(pexp)
!                     above line ends polynomial substitution

      if(dsx .gt. 0.40) dsx=0.40
! ----------------------------------------------------------------------
! mbek - april 2001
! set lower limit on snow density, rather than just previous value.
!         if(dsx .lt. 0.05) dsx=ds
      if(dsx .lt. 0.05) dsx=0.05

      ds=dsx

! ##  update of snow depth and density depending on liquid water
! ##  during snowmelt. assumed that 13% of liquid water can be stored
! ##  in snow per day during snowmelt till snow density 0.40

!         if((tsnowx .ge. 0.) .and. (h .ne. 0.)) then
      if (tsnowx .ge. 0.) then
        dw=0.13*dt/24.
        ds=ds*(1.-dw)+dw
        if(ds .gt. 0.40) ds=0.40
      endif
! ----------------------------------------------------------------------
! calculate snow depth (cm) from snow water equivalent and snow density.
      h=wx/ds
! ----------------------------------------------------------------------
! change snow depth units to meters
      hc=h*0.01

      return
      end
      subroutine snow_new ( t,p,hc,ds )

      implicit none
      
! ----------------------------------------------------------------------
! calculating snow depth and densitity to account for the new snowfall
! t - air temperature, k
! p - new snowfall, m
! hc - snow depth, m
! ds - snow density
! new values of snow depth & density will be returned
      real hc
      real t 
      real p
      real ds
      real h
      real px
      real tx
      real ds0
      real hnew
!
      real esd
      
! ----------------------------------------------------------------------
! conversion into simulation units      
      h=hc*100.
      px=p*100.
      tx=t-273.15
      
! ----------------------------------------------------------------------
! calculating new snowfall density depending on temperature
! equation from gottlib l. 'a general runoff model for snowcovered
! and glacierized basin', 6th nordic hydrological conference,
! vemadolen, sweden, 1980, 172-177pp.
!-----------------------------------------------------------------------
      if(tx .le. -15.) then
        ds0=0.05
      else                                                      
        ds0=0.05+0.0017*(tx+15.)**1.5
      endif
      
! ----------------------------------------------------------------------
! adjustment of snow density depending on new snowfall      
      hnew=px/ds0
      ds=(h*ds+hnew*ds0)/(h+hnew)
      h=h+hnew
      hc=h*0.01
      
! ----------------------------------------------------------------------
      return
      end



      subroutine srt (rhstt,runoff,edir,et,sh2o,sh2oa,nsoil,pcpdrp,  &
                       zsoil,dwsat,dksat,smcmax,b, runoff1,   &
                       runoff2,dt,smcwlt,slope,kdt,frzx,sice)      

    USE ABCI
!
      implicit none

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate the right hand side of the time tendency
!c    =======   term of the soil water diffusion equation.  also to
!c              compute ( prepare ) the matrix coefficients for the
!c              tri-diagonal matrix of the implicit time scheme.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer cvfrz      
      integer ialp1
      integer iohinf
      integer j
      integer jj      
      integer k
      integer ks
      integer nsoil
      real b
      real dmax   ( nsold )
      real ddz
      real ddz2
      real denom
      real denom2
      real dksat
      real dsmdz
      real dsmdz2
      real dwsat
      real edir
      real et     ( nsoil )
      real infmax
      real kdt
      real mxsmc
      real mxsmc2
      real numer
      real pcpdrp
      real pddum
      real rhstt  ( nsoil )
      real runoff
      
      real sh2o   ( nsoil )
      real sh2oa  ( nsoil )
      real sice   ( nsoil )
      real sicemax
      
      real smcmax
      real wcnd
      real wcnd2
      real wdf
      real wdf2
      real zsoil  ( nsoil )

      real runoff1, runoff2, dt, smcwlt, slope, frzx, dt1
      real smcav, dice, dd, val, ddt, px, fcr, acrt, sum
      real sstt, slopx

! -----------     frozen ground version    -------------------------
!   reference frozen ground parameter, cvfrz, is a shape parameter of
!   areal distribution function of soil ice content which equals 1/cv.
!   cv is a coefficient of spatial variation of soil ice content. 
!   based on field data cv depends on areal mean of frozen depth, and it
!   close to constant = 0.6 if areal mean frozen depth is above 20 cm.
!   that is why parameter cvfrz = 3 (int{1/0.6*0.6})  
!
!   current logic doesn't allow cvfrz be bigger than 3
        parameter ( cvfrz = 3 )
! ------------------------------------------------------------------
     
!      print*,'in srt, declaration -----------------------'
!      print*,'nsoil=' , nsoil
!      print*,'nsold=' , nsold
        
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     determine rainfall infiltration rate and runoff
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!
! ##include the infiltration formule from schaake and koren model
!
!c    modified by q duan
!c      
      iohinf=1

! let sicemax be the greatest, if any, frozen water content within 
! soil layers.
      sicemax = 0.0
      do ks=1,nsoil
       if (sice(ks) .gt. sicemax) sicemax = sice(ks)
      end do

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     determine rainfall infiltration rate and runoff
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      pddum = pcpdrp
      runoff1 = 0.0
      if ( pcpdrp .ne. 0.0 ) then

!c++  modified by q. duan, 5/16/94

!        if (iohinf .eq. 1) then
  
          dt1 = dt/86400.
          smcav = smcmax - smcwlt
          dmax(1)=-zsoil(1)*smcav

! -----------     frozen ground version    ------------------------
!
          dice = -zsoil(1) * sice(1)
!-------------------------------------------------------------------
          
          dmax(1)=dmax(1)*(1.0 - (sh2oa(1)+sice(1)-smcwlt)/smcav)
          dd=dmax(1)
      do ks=2,nsoil
          
! -----------     frozen ground version    ------------------------
!
           dice = dice + ( zsoil(ks-1) - zsoil(ks) ) * sice(ks)
!------------------------------------------------------------------- 
         
           dmax(ks)=(zsoil(ks-1)-zsoil(ks))*smcav
           dmax(ks)=dmax(ks)*(1.0 - (sh2oa(ks)+sice(ks)-smcwlt)/smcav)
           dd=dd+dmax(ks)
      end do
!c .....val = (1.-exp(-kdt*sqrt(dt1)))
! in below, remove the sqrt in above
          val = (1.-exp(-kdt*dt1))
          ddt = dd*val
          px = pcpdrp*dt  
          if(px.lt.0.0) px = 0.0
          infmax = (px*(ddt/(px+ddt)))/dt
          
! -----------     frozen ground version    --------------------------
!    reduction of infiltration based on frozen ground parameters
!
         fcr = 1. 
         if ( dice .gt. 1.e-2) then 
           acrt = cvfrz * frzx / dice 
           sum = 1.
           ialp1 = cvfrz - 1 
           do j = 1,ialp1
              k = 1
              do jj = j+1, ialp1
                k = k * jj
              end do   
              sum = sum + (acrt ** ( cvfrz-j)) / float (k) 
           end do 
           fcr = 1. - exp(-acrt) * sum 
         end if 
         infmax = infmax * fcr
! -------------------------------------------------------------------

! ############    correction of infiltration limitation    ##########
!     if infmax .le. hydrolic conductivity assign infmax the 
!     value of hydrolic conductivity
!
!         mxsmc = max ( sh2oa(1), sh2oa(2) ) 
        mxsmc = sh2oa(1)

!      print*,'srt, before wdfcnd - 1 ------------------------------'
!      print*,'mxsmc,smcmax=' , mxsmc,smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat

      call wdfcnd ( wdf,wcnd,mxsmc,smcmax,b,dksat,dwsat,  &
                     sicemax )

            infmax = max(infmax, wcnd)
            infmax= min(infmax,px)

!      print*,'srt, after wdfcnd - 1 ------------------------------'
!      print*,'wdf,wcnd=' , wdf,wcnd
!      print*,'mxsmc,smcmax=' , mxsmc,smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
!
          if ( pcpdrp .gt. infmax ) then
            runoff1 = pcpdrp - infmax
            pddum = infmax
          end if

      end if
!
! to avoid spurious drainage behavior identified by p. grunmann,
! former approach in line below replaced with new approach in 2nd line
!...mxsmc = max( sh2oa(1), sh2oa(2) )
        mxsmc =  sh2oa(1)

!      print*,'srt, before wdfcnd - 2'
!      print*,'mxsmc,smcmax=' , mxsmc,smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat

      call wdfcnd ( wdf,wcnd,mxsmc,smcmax,b,dksat,dwsat,  &
      sicemax )

!      print*,'srt, after wdfcnd - 2'
!      print*,'wdf,wcnd=' , wdf,wcnd
!      print*,'mxsmc,smcmax=' , mxsmc,smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the matrix coefficients ai, bi, and ci for the top layer
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz = 1. / ( -.5 * zsoil(2) )
      ai(1) = 0.0
      bi(1) = wdf * ddz / ( -zsoil(1) )
      ci(1) = -bi(1)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc rhstt for the top layer after calc'ng the vertical soil
!     moisture gradient btwn the top and next to top layers.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      dsmdz = ( sh2o(1) - sh2o(2) ) / ( -.5 * zsoil(2) )
      rhstt(1) = (wdf * dsmdz + wcnd - pddum + edir + et(1))/zsoil(1)
      sstt = wdf * dsmdz + wcnd + edir + et(1)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     initialize ddz2
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ddz2 = 0.0

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     loop thru the remaining soil layers, repeating the abv process
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 2 , nsoil
         denom2 = ( zsoil(k-1) - zsoil(k) )
         if ( k .ne. nsoil ) then
            slopx = 1.
!
! again, to avoid spurious drainage behavior identified by p. grunmann,
! former approach in line below replaced with new approach in 2nd line
!....mxsmc2 = max ( sh2oa(k), sh2oa(k+1) )
            mxsmc2 =  sh2oa(k)

!      print*,'srt, before wdfcnd - 3'
!      print*,'mxsmc2,smcmax=' , mxsmc2,smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat
!      print*,'k=' , k

            call wdfcnd ( wdf2,wcnd2,mxsmc2,smcmax,b,dksat,dwsat,  &
                 sicemax )

!      print*,'srt, after wdfcnd - 3'
!      print*,'wdf2,wcnd2=' , wdf2,wcnd2
!      print*,'mxsmc2,smcmax=' , mxsmc2,smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc some partial products for later use in calc'ng rhstt
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            denom = ( zsoil(k-1) - zsoil(k+1) )
            dsmdz2 = ( sh2o(k) - sh2o(k+1) ) / ( denom * 0.5 )

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         calc the matrix coef, ci, after calc'ng its partial product
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            ddz2 = 2.0 / denom
            ci(k) = -wdf2 * ddz2 / denom2
         else

!   slope of bottom layer is introduced     ############
!
            slopx = slope
!--------------------------------------------------------
          
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         retrieve the soil water diffusivity and hydraulic
!         conductivity for this layer
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


!      print*,'srt, before wdfcnd - 4'
!      print*,'sh2oa(nsoil),smcmax=' , sh2oa(nsoil),smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat
!      print*,'k=' , k
 
            call wdfcnd ( wdf2,wcnd2,sh2oa(nsoil),smcmax,  &
                 b,dksat,dwsat,sicemax )

!      print*,'srt, after wdfcnd - 4'
!      print*,'wdf2,wcnd2=' , wdf2,wcnd2
!      print*,'sh2oa(nsoil),smcmax=' , sh2oa(nsoil),smcmax
!      print*,'b,dksat,dwsat=' , b,dksat,dwsat
 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         calc a partial product for later use in calc'ng rhstt
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            dsmdz2 = 0.0

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!         set matrix coef ci to zero
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

            ci(k) = 0.0
         end if

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc rhstt for this layer after calc'ng its numerator
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         numer = (wdf2 * dsmdz2) + slopx * wcnd2 - (wdf * dsmdz)   &
              - wcnd + et(k)
         rhstt(k) = numer / (-denom2)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc matrix coefs, ai, and bi for this layer
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         ai(k) = -wdf * ddz / denom2
         bi(k) = -( ai(k) + ci(k) )

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       reset values of wdf, wcnd, dsmdz, and ddz for loop to next lyr
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         if(k.eq.nsoil) then
!############### runoff2: ground water runoff ###########
            runoff2 = slopx * wcnd2
         endif

         if ( k .ne. nsoil ) then
            wdf = wdf2
            wcnd = wcnd2
            dsmdz = dsmdz2
            ddz = ddz2
         end if
      end do

!      print*,'srt, final runoff'
!      print*,'runoff1=' , runoff1
!      print*,'runoff2=' , runoff2
 
      return
      end




      subroutine sstep ( sh2oout, sh2oin, cmc, rhstt, rhsct, dt,  &
           nsoil, smcmax, cmcmax, runoff3, zsoil,smc,sice )
!
    USE ABCI


      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate/update the soil moisture content values
!c    =======   and the canopy moisture content values.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
      integer i
      integer k 
      integer kk11
      integer nsoil
      real ciin   ( nsold )
      real cmc
      real cmcmax
      real dt
      real rhsct
      real rhstt   ( nsoil )
      real rhsttin ( nsoil )
      real sh2oin  ( nsoil )
      real sh2oout ( nsoil )
      real sice    ( nsoil )
      real smc     ( nsoil )
      real smcmax
      real zsoil(nsoil)

      real runoff3, runofs, wplus, ddz, stot, wfree, dplus

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     create 'amount' values of variables to be input to the
!     tri-diagonal matrix routine.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 1 , nsoil
        rhstt(k) = rhstt(k) * dt
        ai(k) = ai(k) * dt
        bi(k) = 1. + bi(k) * dt
        ci(k) = ci(k) * dt
      end do

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     copy values for input variables before call to rosr12
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      do k = 1 , nsoil
         rhsttin(k) = rhstt(k)
      end do
      do k = 1 , nsold
         ciin(k) = ci(k)
      end do
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     call rosr12 to solve the tri-diagonal matrix
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      call rosr12 ( ci, ai, bi, ciin, rhsttin, rhstt, nsoil )

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     sum the previous smc value and the matrix solution to get a
!     new value.  min allowable value of smc will be 0.02.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!   ################## runoff3: runoff within soil layers #######

      runofs = 0.0
      wplus = 0.0
      runoff3 = 0.
      ddz = - zsoil(1)
      
      do k = 1 , nsoil
         if ( k .ne. 1 ) ddz = zsoil(k - 1) - zsoil(k)
         sh2oout(k) = sh2oin(k) + ci(k) + wplus / ddz
        
!      print*,'in sstep'
!      print*,'sh2oout=', sh2oout
        
         stot = sh2oout(k) + sice(k)
         if ( stot .gt. smcmax ) then
            if ( k .eq. 1 ) then
               ddz = -zsoil(1)
            else
               kk11 = k - 1
               ddz = -zsoil(k) + zsoil(kk11)
            end if
            wplus = ( stot - smcmax ) * ddz
         else
            wplus = 0.
         end if
         smc(k) = max ( min( stot, smcmax ), 0.02 )
         sh2oout(k) = max ( (smc(k) - sice(k)), 0.0 )
      end do


!  ###  v. koren   9/01/98    ######
!     water balance checking upward

      if(wplus .gt. 0.) then
       do i=nsoil-1,1,-1
        if(i .eq. 1) then
         ddz=-zsoil(1)
        else
         ddz=-zsoil(i)+zsoil(i-1)
        endif
        wfree=(smcmax-sh2oout(i)-sice(i))*ddz
          if(wfree .lt. 0.) then
!            print *,smcmax,sh2oout(i),sice(i)
            wfree=0
         endif

        dplus=wfree-wplus
        if(dplus .ge. 0.) then
         sh2oout(i)=sh2oout(i)+wplus/ddz
         smc(i)=sh2oout(i)+sice(i)
         wplus=0.
           
        else
         sh2oout(i)=sh2oout(i)+wfree/ddz
         smc(i)=sh2oout(i)+sice(i)
         wplus=-dplus
        endif
       end do
30     runoff3=wplus
      endif

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  update canopy water content/interception (cmc).  convert rhsct to 
!  an 'amount' value and add to previous cmc value to get new cmc.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      cmc = cmc + dt * rhsct
      if (cmc .lt. 1.e-20) cmc=0.0
      cmc = min(cmc,cmcmax)

      return
      end
      subroutine tbnd (tu, tb, zsoil, zbot, k, nsoil, tbnd1)

      implicit none

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c   purpose:   calculate temperature on the boundary of the layer
!c   =======    by interpolation of the middle layer temperatures
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer nsoil
      integer k

      real tbnd1
      real t0
      real tu
      real tb
      real zb
      real zbot
      real zup
      real zsoil (nsoil)

      parameter (t0=273.15)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c   use surface temperature on the top of the first layer
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      if(k .eq. 1) then
        zup=0.
      else
        zup=zsoil(k-1)
      endif

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c   use depth of the constant bottom temperature when interpolate
!c   temperature into the last layer boundary
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      if(k .eq. nsoil) then
        zb=2.*zbot-zsoil(k)
      else
        zb=zsoil(k+1)
      endif

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c   linear interpolation between the average layer temperatures
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      tbnd1 = tu+(tb-tu)*(zup-zsoil(k))/(zup-zb)
      
      return
      end
      subroutine tdfcnd ( df, smc, q,  smcmax, sh2o)

      implicit none

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate thermal diffusivity and conductivity of
!c    =======   the soil for a given point and time.
!c
!c    version:  peters-lidard approach (peters-lidard et al., 1998)
!c    =======
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       real df
       real gammd
       real thkdry
       real ake
       real thkice
       real thko
       real thkqtz
       real thksat
       real thks
       real thkw
       real q
       real satratio
       real sh2o
       real smc
       real smcmax
       real xu
       real xunfroz


! we now get quartz as an input argument (set in routine redprm):
!        data quartz /0.82, 0.10, 0.25, 0.60, 0.52, 
!     &              0.35, 0.60, 0.40, 0.82/

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     if the soil has any moisture content compute a partial sum/product
!     otherwise use a constant value which works well with most soils
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  
!
!  thkw ......water thermal conductivity
!  thkqtz ....thermal conductivity for quartz
!  thko ......thermal conductivity for other soil components
!  thks ......thermal conductivity for the solids combined(quartz+other)
!  thkice ....ice thermal conductivity
!  smcmax ....porosity (= smcmax)
!  q .........quartz content (soil type dependent)
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! use as in peters-lidard, 1998 (modif. from johansen, 1975).
!
!                                  pablo grunmann, 08/17/98
! refs.:
!      farouki, o.t.,1986: thermal properties of soils. series on rock 
!              and soil mechanics, vol. 11, trans tech, 136 pp.
!      johansen, o., 1975: thermal conductivity of soils. ph.d. thesis,
!              university of trondheim,
!      peters-lidard, c. d., et al., 1998: the effect of soil thermal 
!              conductivity parameterization on surface energy fluxes
!              and temperatures. journal of the atmospheric sciences,
!              vol. 55, pp. 1209-1224.
! 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!  needs parameters
! porosity(soil type):
!      poros = smcmax
! saturation ratio:
      satratio = smc/smcmax
!      print *, 'satratio=',satratio
!     parameters  w/(m.k)
      thkice = 2.2
      thkw = 0.57
      thko = 2.0
!      if (q .le. 0.2) thko = 3.0
      thkqtz = 7.7
!  solids' conductivity      
      thks = (thkqtz**q)*(thko**(1.- q))
!      print *, 'thks = ',thks
!  unfrozen fraction (from 1.0, i.e., 100%liquid, to 0.0 (100% frozen))
      xunfroz=(sh2o + 1.e-9)/(smc + 1.e-9)
!      print *, '   '
!      print *, 'xunfroz = ',xunfroz
!      print *, '    '
!  unfrozen volume for saturation (porosity*xunfroz)
      xu=xunfroz*smcmax 
!  saturated thermal conductivity
      thksat = thks**(1.-smcmax)*thkice**(smcmax-xu)*thkw**(xu)
!      print *, 'thksat = ',thksat
!  dry density in kg/m3
      gammd = (1. - smcmax)*2700.
!      print *, 'gammd = ',gammd
!  dry thermal conductivity in w.m-1.k-1
      thkdry = (0.135*gammd + 64.7)/(2700. - 0.947*gammd)
!      print *, 'thkdry = ',thkdry
! range of validity for the kersten number
      if ( satratio .gt. 0.1 ) then

!    kersten number (fine formula, at least 5% of particles<(2.e-6)m)
           if ( (xunfroz + 0.0005) .lt. smc ) then
!    frozen
              ake = satratio
           else
!    unfrozen
              ake = log10(satratio) + 1.0
           endif

      else
        
! use k = kdry
        ake = 0.0
!        print *, 'ake (else) = ',ake
      endif
!  thermal conductivity

       df = ake*(thksat - thkdry) + thkdry

      return
      end
      subroutine transp (et,nsoil,etp1,smc,cmc,zsoil,shdfac,smcwlt,  &
            cmcmax,pc,cfactr,smcref,sfctmp,q2,nroot,rtdis)

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate transpiration from the vegtyp for this pt.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer i
      integer k
      integer nsoil
      integer nroot

      real cfactr
      real cmc
      real cmcmax
      real et    ( nsoil )
      real etp1
      real etp1a
      real gx (7)
!.....real part ( nsoil )
      real pc
      real rtdis ( nsoil )
      real shdfac
      real smc   ( nsoil )
      real smcref
      real smcwlt
      real zsoil ( nsoil )

      real sfctmp, q2, sgx, denom, rtx

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       initialize  plant transp to zero for all soil layers.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do k = 1, nsoil
         et(k) = 0.
      end do

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!       calc an 'adjusted' potntl transpiration
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!cc if statements to avoid tangent linear problems near zero
      if (cmc .ne. 0.0) then
      etp1a = shdfac * pc * etp1 * (1.0 - (cmc /cmcmax) ** cfactr)
      else
      etp1a = shdfac * pc * etp1
      endif
      
      sgx = 0.0
      do i = 1, nroot
         gx(i) = ( smc(i) - smcwlt ) / ( smcref - smcwlt )
         gx(i) = max ( min ( gx(i), 1. ), 0. )
         sgx = sgx + gx (i)
      end do
      sgx = sgx / nroot
      
      denom = 0.
      do i = 1,nroot
         rtx = rtdis(i) + gx(i) - sgx
         gx(i) = gx(i) * max ( rtx, 0. )
         denom = denom + gx(i)
      end do   
      if ( denom .le. 0.0) denom = 1.
      
      do i = 1, nroot
         et(i) = etp1a * gx(i) / denom
      end do 

! above code assumes a vertically uniform root distribution
!
! code below tests a variable root distribution
!
!     et(1) = ( zsoil(1) / zsoil(nroot) ) * gx * etp1a
!        et(1) = ( zsoil(1) / zsoil(nroot) ) * etp1a
!
! ###  using root distribution as weighting factor
!     et(1) = rtdis(1) * etp1a
!         et(1) =  etp1a*part(1)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     loop down thru the soil layers repeating the operation above,
!     but using the thickness of the soil layer (rather than the
!     absolute depth of each layer) in the final calculation.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
!     do 10 k = 2, nroot
!     gx = ( smc(k) - smcwlt ) / ( smcref - smcwlt )
!     gx = max ( min ( gx, 1. ), 0. )
!     test canopy resistance
!     gx = 1.0
!     et(k) = ((zsoil(k)-zsoil(k-1))/zsoil(nroot))*gx*etp1a
!       et(k) = ((zsoil(k)-zsoil(k-1))/zsoil(nroot))*etp1a
!###  using root distribution as weighting factor
!       et(k) = rtdis(k) * etp1a
!         et(k) = etp1a*part(k)
!     10    continue
      
      return
      end
      subroutine wdfcnd ( wdf,wcnd,smc,smcmax,b,dksat,dwsat,  &
                               sicemax )

      implicit none

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c    purpose:  to calculate soil water diffusivity and soil
!c    =======   hydraulic conductivity.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real b
      real dksat
      real dwsat
      real expon
      real factr1
      real factr2
      real sicemax
      real smc
      real smcmax
      real vkwgt
      real wcnd
      real wdf

!      print*,'------------ in wdfcnd -------------------------------'
!      print*,'before wdfcnd'
!      print*,'b=',b
!      print*,'dksat=',dksat
!      print*,'dwsat=',dwsat
!      print*,'expon=',expon
!      print*,'factr2=',factr2
!      print*,'smc=',smc
!      print*,'smcmax=',smcmax
!      print*,'wcnd=',wcnd
!      print*,'wdf=',wdf
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     calc the ratio of the actual to the max psbl soil h2o content
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      smc = smc
      smcmax = smcmax
      factr1 = 0.2 / smcmax
      factr2 = smc / smcmax

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     prep an expntl coef and calc the soil water diffusivity
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      expon = b + 2.0
      wdf = dwsat * factr2 ** expon

! frozen soil hydraulic diffusivity.  very sensitive to the vertical
! gradient of unfrozen water. the latter gradient can become very
! extreme in freezing/thawing situations, and given the relatively 
! few and thick soil layers, this gradient sufferes serious 
! trunction errors yielding erroneously high vertical transports of
! unfrozen water in both directions from huge hydraulic diffusivity.  
! therefore, we found we had to arbitrarily constrain wdf 
!
! version d_10cm: ........  factr1 = 0.2/smcmax
! weighted approach...................... pablo grunmann, 09/28/99.
      if (sicemax .gt. 0.0)  then
      vkwgt=1./(1.+(500.*sicemax)**3.)
      wdf = vkwgt*wdf + (1.- vkwgt)*dwsat*factr1**expon
!      print*,'______________________________________________'
!      print*,'weighted approach:'
!      print*,'  sicemax       vkwgt              dwgt'
!      print*,sicemax,  vkwgt, 1.-vkwgt
!      print*,'______________________________________________'
      endif
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     reset the expntl coef and calc the hydraulic conductivity
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      expon = ( 2.0 * b ) + 3.0
      wcnd = dksat * factr2 ** expon

!      print*,' wdfcnd results --------------------------------'
!      print*,'b=',b
!      print*,'dksat=',dksat
!      print*,'dwsat=',dwsat
!      print*,'expon=',expon
!      print*,'factr2=',factr2
!      print*,'smc=',smc
!      print*,'smcmax=',smcmax
!      print*,'wcnd=',wcnd
!      print*,'wdf=',wdf
!      print*,' smc         wdf           wcnd             b'
!      print*,smc,wdf,wcnd,b

      return
      end
