      MODULE mod_grid
!
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  angler     Angle (radians) between XI-axis and true EAST at         !
!               RHO-points.                                            !
!  h          Bottom depth (m) at RHO-points.                          !
!  latp       Latitude (degrees_north) at PSI-points.                  !
!  latr       Latitude (degrees_north) at RHO-points.                  !
!  latu       Latitude (degrees_north) at U-points.                    !
!  latv       Latitude (degrees_north) at V-points.                    !
!  lonp       Longitude (degrees_east) at PSI-points.                  !
!  lonr       Longitude (degrees_east) at RHO-points.                  !
!  lonu       Longitude (degrees_east) at U-points.                    !
!  lonv       Longitude (degrees_east) at V-points.                    !
!               (0=Land, 1=Sea, 1-gamma2=boundary).                    !
!  rmask      Mask at RHO-points (0=Land, 1=Sea).                      !
!                                                                      !
!=======================================================================
!
        USE mod_kinds

        implicit none

        TYPE T_GRID
!
          real(r8), pointer :: angler(:,:)
          real(r8), pointer :: SinAngler(:,:)

          real(r8), pointer :: h(:,:)
          real(r8), pointer :: latp(:,:)
          real(r8), pointer :: latr(:,:)
          real(r8), pointer :: latu(:,:)
          real(r8), pointer :: latv(:,:)
          real(r8), pointer :: lonp(:,:)
          real(r8), pointer :: lonr(:,:)
          real(r8), pointer :: lonu(:,:)
          real(r8), pointer :: lonv(:,:)
          real(r8), pointer :: Hz(:,:,:)
          real(r8), pointer :: z_r(:,:,:)
          real(r8), pointer :: z_w(:,:,:)
          real(r8), pointer :: rmask(:,:)
        END TYPE T_GRID

        TYPE (T_GRID), allocatable :: GRID(:)

      CONTAINS

      SUBROUTINE allocate_grid (ng, LBi, UBi, LBj, UBj)
!
!=======================================================================
!                                                                      !
!  This routine allocates all variables in the module for all nested   !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Local variable declarations.
!
      integer, intent(in) :: ng, LBi, UBi, LBj, UBj
!
!-----------------------------------------------------------------------
!  Allocate and initialize module variables.
!-----------------------------------------------------------------------
!
      IF (ng.eq.1) allocate ( GRID(Ngrids) )
!
!  Nonlinear model state.
!
      allocate ( GRID(ng) % angler(LBi:UBi,LBj:UBj) )

      allocate ( GRID(ng) % h(LBi:UBi,LBj:UBj) )
      allocate ( GRID(ng) % latp(LBi:UBi,LBj:UBj) )
      allocate ( GRID(ng) % latr(LBi:UBi,LBj:UBj) )
      allocate ( GRID(ng) % latu(LBi:UBi,LBj:UBj) )
      allocate ( GRID(ng) % latv(LBi:UBi,LBj:UBj) )
      allocate ( GRID(ng) % lonp(LBi:UBi,LBj:UBj))
      allocate ( GRID(ng) % lonr(LBi:UBi,LBj:UBj))
      allocate ( GRID(ng) % lonu(LBi:UBi,LBj:UBj))
      allocate ( GRID(ng) % lonv(LBi:UBi,LBj:UBj))

      allocate ( GRID(ng) % Hz(LBi:UBi,LBj:UBj,N(ng)) )
      allocate ( GRID(ng) % z_r(LBi:UBi,LBj:UBj,N(ng)) )
      allocate ( GRID(ng) % z_w(LBi:UBi,LBj:UBj,0:N(ng)) )

      allocate ( GRID(ng) % rmask(LBi:UBi,LBj:UBj) )

      RETURN
      END SUBROUTINE allocate_grid

      SUBROUTINE initialize_grid (ng, tile)
!
!=======================================================================
!                                                                      !
!  This routine initialize all variables in the module using first     !
!  touch distribution policy. In shared-memory configuration, this     !
!  operation actually performs propagation of the  "shared arrays"     !
!  across the cluster, unless another policy is specified to           !
!  override the default.                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: Imin, Imax, Jmin, Jmax
      integer :: i, j, k

      real(r8), parameter :: IniVal = 0.0_r8
!
!  Set array initialization range.
!
        Imin=0
        Imax=Lm(ng)+1
        Jmin=0
        Jmax=Mm(ng)+1
!
!-----------------------------------------------------------------------
!  Initialize module variables.
!-----------------------------------------------------------------------
!
!  Nonlinear model state.
!
        DO j=Jmin,Jmax
          DO i=Imin,Imax
            GRID(ng) % angler(i,j) = IniVal

            GRID(ng) % h(i,j) = IniVal

            GRID(ng) % latp(i,j) = IniVal
            GRID(ng) % latr(i,j) = IniVal
            GRID(ng) % latu(i,j) = IniVal
            GRID(ng) % latv(i,j) = IniVal
            GRID(ng) % lonp(i,j) = IniVal
            GRID(ng) % lonr(i,j) = IniVal
            GRID(ng) % lonu(i,j) = IniVal
            GRID(ng) % lonv(i,j) = IniVal

            GRID(ng) % rmask(i,j) = IniVal

          END DO
        END DO
        DO j=Jmin,Jmax
          DO k=1,N(ng)
            DO i=Imin,Imax
              GRID(ng) % Hz(i,j,k) = IniVal
              GRID(ng) % z_r(i,j,k) = IniVal
            END DO
          END DO
          DO k=0,N(ng)
            DO i=Imin,Imax
              GRID(ng) % z_w(i,j,k) = IniVal
            END DO
          END DO
        END DO

      RETURN
      END SUBROUTINE initialize_grid

      END MODULE mod_grid
