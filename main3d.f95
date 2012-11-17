      program main
!
!=======================================================================
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This subroutine is the main driver for nonlinear ROMS/TOMS when     !
!  configurated as a full 3D baroclinic ocean model.  It  advances     !
!  forward the primitive equations for all  nested  grids, if any,     !
!  for the specified time interval (seconds), RunInterval.             !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_scalars
      USE mod_grid
      USE set_depth_mod
!
      implicit none
!
!  Local variable declarations.
!
      integer :: ng=1, tile=0
      integer :: LBi, UBi, LBj, UBj
!
!-----------------------------------------------------------------------
!  Read in required data, if any, from input NetCDF files.
!-----------------------------------------------------------------------
!
      Lm(ng) = 224
      Mm(ng) = 640
      N(ng) = 50
      LBi = 0
      LBj = 0
      UBi = Lm(ng)+1
      UBj = Mm(ng)+1

      theta_s = 7.0
      theta_b = 2.0
      tcline = 250.0
      Vtransform = 2
      Vstretching = 4

      CALL allocate_scalars
      CALL allocate_grid(ng, LBi, UBi, LBj, UBj)
      CALL initialize_grid(ng, tile)
      CALL get_grid(ng, 1)
      CALL set_scoord(ng)
      CALL set_depth(ng, tile)
      CALL read_fltpar('floats_NEP.in')
      CALL grid_coords(ng, 1)

      RETURN
      END program main
