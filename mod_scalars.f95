      MODULE mod_scalars
!
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!
        USE mod_param
!
        implicit none
!
!-----------------------------------------------------------------------
! Multiple grid structure.
!-----------------------------------------------------------------------
!
!    Cs_r          Set of S-curves used to stretch the vertical grid
!                    that follows the bathymetry at vertical RHO-points.
!    Cs_w          Set of S-curves used to stretch the vertical grid
!                    that follows the bathymetry at vertical W-points.
!    sc_r          S-coordinate independent variable, [-1 < sc < 0] at
!                    vertical RHO-points.
!    sc_w          S-coordinate independent variable, [-1 < sc < 0] at
!                    vertical W-points.
!
        TYPE T_SCALARS

          real(r8), pointer :: Cs_r(:)
          real(r8), pointer :: Cs_w(:)
          real(r8), pointer :: sc_r(:)
          real(r8), pointer :: sc_w(:)

        END TYPE T_SCALARS

        TYPE (T_SCALARS), allocatable :: SCALARS(:)
!
! Random ROMS stuff
!
        integer, parameter :: stdout = 6
        integer, parameter :: NoError = 0
        integer :: exit_flag
        integer :: ioerror
        logical, parameter :: Master = .true.
        character*256 :: SourceFile
        real(r8), parameter :: spval_check = 1.0E+35_r8
        logical :: spherical
        real(r8) :: Eradius = 6371315.0_r8
!
!  Interpolation scheme.
!
        integer, parameter :: linear = 0        ! linear interpolation
        integer, parameter :: cubic  = 1        ! cubic  interpolation
!
        integer :: InterpFlag = cubic          ! interpolation flag
!
!  Vertical coordinates transform.  Currently, there are two vertical
!  transformation equations (see set_scoord.F for details):
!
!    Original transform (Vtransform=1):
!
!         z_r(x,y,s,t) = Zo_r + zeta(x,y,t) * [1.0 + Zo_r / h(x,y)]
!
!                 Zo_r = hc * [s(k) - C(k)] + C(k) * h(x,y)
!
!    New transform (Vtransform=2):
!
!         z_r(x,y,s,t) = zeta(x,y,t) + [zeta(x,y,t)+ h(x,y)] * Zo_r
!
!                 Zo_r = [hc * s(k) + C(k) * h(x,y)] / [hc + h(x,y)]
!
        integer :: Vtransform(1)
!
!  Vertical grid stretching function flag:
!
!    Vstretcing = 1   Original function (Song and Haidvogel, 1994)
!               = 2   A. Shchepetkin (ROMS-UCLA) function
!               = 3   R. Geyer BBL function
!
        integer :: Vstretching(1)
!
!  Vertical grid stretching parameters.
!
!    Tcline        Width (m) of surface or bottom boundary layer in
!                    which higher vertical resolution is required
!                    during stretching.
!    hc            S-coordinate critical depth, hc=MIN(hmin,Tcline).
!    theta_s       S-coordinate surface control parameter.
!    theta_b       S-coordinate bottom control parameter.
!
        real(r8) :: Tcline(1)      ! m, positive
        real(r8) :: hc(1)          ! m, positive
        real(r8) :: theta_s(1)     ! 0 < theta_s < 20
        real(r8) :: theta_b(1)     ! 0 < theta_b < 1
!
!  Bathymetry range values.
!
        real(r8) :: hmin(1)        ! m, positive
        real(r8) :: hmax(1)        ! m, positive
!
        real(r8), parameter :: pi = 3.14159265358979323846_r8
        real(r8), parameter :: deg2rad = pi / 180.0_r8
        real(r8), parameter :: rad2deg = 180.0_r8 / pi
        real(r8), parameter :: day2sec = 86400.0_r8
        real(r8), parameter :: sec2day = 1.0_r8 / 86400.0_r8

      CONTAINS
!
      SUBROUTINE allocate_scalars
!
!=======================================================================
!
!
!  This routine allocates structure and  several variables in module
!
!  that depend on the number of nested grids.
!
!
!
!=======================================================================
!
!  Local variable declarations.
!
      integer :: ng
      real(r8), parameter :: IniVal = 0.0_r8
!
!-----------------------------------------------------------------------
!  Allocate and initialize variables in module structure.
!-----------------------------------------------------------------------
!
      Ngrids = 1
      allocate ( SCALARS(Ngrids) )

      DO ng=1,Ngrids
        allocate ( SCALARS(ng) % Cs_r(N(ng)) )
        SCALARS(ng) % Cs_r(1:N(ng)) = IniVal

        allocate ( SCALARS(ng) % Cs_w(0:N(ng)) )
        SCALARS(ng) % Cs_w(0:N(ng)) = IniVal

        allocate ( SCALARS(ng) % sc_r(N(ng)) )
        SCALARS(ng) % sc_r(1:N(ng)) = IniVal

        allocate ( SCALARS(ng) % sc_w(0:N(ng)) )
        SCALARS(ng) % sc_w(0:N(ng)) = IniVal
      END DO

      RETURN
      END SUBROUTINE allocate_scalars

      END MODULE mod_scalars
