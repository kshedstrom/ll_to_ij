      SUBROUTINE get_grid (ng, model)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This subroutine reads grid information from GRID NetCDF file.       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_grid
      USE mod_netcdf
      USE mod_scalars
!
      USE nf_fread2d_mod, ONLY : nf_fread2d
      USE strings_mod, ONLY : find_string
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
!
!  Local variable declarations.
!
      integer :: tile, LBi, UBi, LBj, UBj
      integer :: gtype, i, it, status, vindex, ncid
      integer :: Vsize(4)

      real(r8), parameter :: Fscl = 1.0_r8

      real(r8) :: Fmax, Fmin

      character (len=256) :: ncname
!
      SourceFile='get_grid.F'
!
!-----------------------------------------------------------------------
!  Inquire about the contents of grid NetCDF file:  Inquire about
!  the dimensions and variables.  Check for consistency.
!-----------------------------------------------------------------------
!
      IF (exit_flag.ne.NoError) RETURN
      ncname='/archive/u1/uaf/kate/gridpak/NEP6/NEP6_grd_depth_min_15m.nc'
!
!  Check grid file dimensions for consitency
!
      CALL netcdf_check_dim (ng, model, ncname)
      IF (exit_flag.ne.NoError) RETURN
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, ncname)
      IF (exit_flag.ne.NoError) RETURN
!
!-----------------------------------------------------------------------
!  Check if required variables are available.
!-----------------------------------------------------------------------
!
      IF (.not.find_string(var_name,n_var,'spherical',vindex)) THEN
        IF (Master) WRITE (stdout,10) 'spherical', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'h',vindex)) THEN
        IF (Master) WRITE (stdout,10) 'h', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'angle',vindex)) THEN
        IF (Master) WRITE (stdout,10) 'angle', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'mask_rho',vindex)) THEN
        IF (Master) WRITE (stdout,10) 'mask_rho', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
!
!  Open grid NetCDF file for reading.
!
        CALL netcdf_open (ng, model, ncname, 0, ncid)
        IF (exit_flag.ne.NoError) THEN
          WRITE (stdout,20) TRIM(ncname)
          RETURN
        END IF
!
!  Read in logical switch for spherical grid configuration.
!
      spherical=.TRUE.
!
!-----------------------------------------------------------------------
!  Read in grid variables.
!-----------------------------------------------------------------------
!
!  Set 2D arrays bounds.
!
      tile=-1
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!  Set Vsize to zero to deativate interpolation of input data to model
!  grid in "nf_fread2d".
!
      DO i=1,4
        Vsize(i)=0
      END DO
!
!  Scan the variable list and read in needed variables.
!
      DO it=1,n_var

        SELECT CASE (TRIM(ADJUSTL(var_name(it))))
!
!  Read in bathymetry.
!
          CASE ('h')
            gtype=2
            status=nf_fread2d(ng, model, ncname, ncid,          &
     &                        var_name(it), var_id(it),                 &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, hmin(ng), hmax(ng),                 &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % h)
            IF (status.ne.nf90_noerr) THEN
!             DO j=LBj,UBj
!               DO i=LBi,UBi
!                 IF (GRID(ng)%h(i,j) .eq. 0.0) GRID(ng) % h(i,j) = -1.
!               END DO
!             END DO
!           ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'h', TRIM(ncname)
              END IF
              exit_flag=2
              ioerror=status
              RETURN
            END IF
!
!  Read in Land/Sea masking at RHO-points.
!
          CASE ('mask_rho')
            gtype=2
            status=nf_fread2d(ng, model, ncname, ncid,          &
     &                        var_name(it), var_id(it),                 &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % rmask)
            IF (status.ne.nf90_noerr) THEN
              exit_flag=2
              ioerror=status
              EXIT
            END IF
!
!  Read in longitude at RHO-points.
!
          CASE ('lon_rho')
            IF (spherical) THEN
              gtype=2
              status=nf_fread2d(ng, model, ncname, ncid,        &
     &                          var_name(it), var_id(it),               &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,           &
     &                          GRID(ng) % rmask,                       &
     &                          GRID(ng) % lonr)
              IF (status.ne.nf90_noerr) THEN
                exit_flag=2
                ioerror=status
                EXIT
              END IF
            END IF
!
!  Read in latitude at RHO-points.
!
          CASE ('lat_rho')
            IF (spherical) THEN
              gtype=2
              status=nf_fread2d(ng, model, ncname, ncid,        &
     &                          var_name(it), var_id(it),               &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,           &
     &                          GRID(ng) % rmask,                       &
     &                          GRID(ng) % latr)
              IF (status.ne.nf90_noerr) THEN
                exit_flag=2
                ioerror=status
                EXIT
              END IF
            END IF
!
!  Read in angle (radians) between XI-axis and EAST at RHO-points.
!
          CASE ('angle')
            gtype=2
            status=nf_fread2d(ng, model, ncname, ncid,          &
     &                        var_name(it), var_id(it),                 &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % rmask,                         &
     &                        GRID(ng) % angler)
            IF (status.ne.nf90_noerr) THEN
              exit_flag=2
              ioerror=status
              EXIT
            END IF
          END SELECT
        END DO
!
! Close GRID NetCDF file.
!
      CALL netcdf_close (ng, model, ncid, ncname, .FALSE.)
      IF (exit_flag.ne.NoError) RETURN

  10  FORMAT (/,' GET_GRID - unable to find grid variable: ',a,         &
     &        /,12x,'in grid NetCDF file: ',a)
  20  FORMAT (/,' GET_GRID - unable to open grid NetCDF file: ',a)
  30  FORMAT (/,' GET_GRID - error while reading variable: ',a,         &
     &        /,12x,'in grid NetCDF file: ',a)
  40  FORMAT (/,' GET_GRID - Reading adjoint sensitivity scope arrays', &
     &        ' from file:',/12x,a)
      RETURN
      END SUBROUTINE get_grid
