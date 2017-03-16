       SUBROUTINE grid_coords (ng, model)
!
!svn $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine converts initial locations to fractional grid (I,J)    !
!  coordinates, if appropriate.                                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_floats
      USE mod_grid
      USE mod_scalars
!
      USE interpolate_mod
!
      implicit none
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
!
!  Local variable declarations.
!
      integer :: IstrR, Iend, JstrR, Jend
      integer :: LBi, UBi, LBj, UBj
      integer :: i, j, k, l, mc

      real(r8), parameter :: spv = 1.0e20_r8

      real(r8) :: Xstr, Xend, Ystr, Yend, zfloat
      real(r8) :: x, y, z, time
      real(r8), dimension(Nfloats(ng)) :: Iflt, Jflt
      real(r8), dimension(Nfloats(ng)) :: Kflt
!
!-----------------------------------------------------------------------
!  Determine searching model grid box and arrays bounds.
!-----------------------------------------------------------------------
!
      IstrR=0
      Iend =Lm(ng)
      JstrR=0
      Jend =Mm(ng)
      ! ARCTIC ONLY!!!
      Jend = 760
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)

!
      Xstr=REAL(LBi,r8)-0.5_r8
      Xend=REAL(Iend,r8)+0.5_r8
      Ystr=REAL(LBj,r8)-0.5_r8
      Yend=REAL(Jend,r8)+0.5_r8
!
!-----------------------------------------------------------------------
!  If applicable, convert initial floats locations (Flon,Flat) to
!  fractional grid coordinates.
!-----------------------------------------------------------------------
!
      IF (spherical) THEN
          mc=DRIFTER(ng)%Findex(0)
          IF (DRIFTER(ng)%Findex(0).gt.0) THEN
            CALL hindices (ng, LBi, UBi, LBj, UBj,                      &
     &                     IstrR, Iend+1, JstrR, Jend+1,                &
     &                     GRID(ng)%angler,                             &
     &                     GRID(ng)%lonr,                               &
     &                     GRID(ng)%latr,                               &
     &                     1, mc, 1, 1,                                 &
     &                     1, mc, 1, 1,                                 &
     &                     DRIFTER(ng)%Flon,                            &
     &                     DRIFTER(ng)%Flat,                            &
     &                     Iflt, Jflt, spv, .FALSE.)
            DO i=1,mc
              l=DRIFTER(ng)%Findex(i)
              DRIFTER(ng)%Tinfo(ixgrd,l)=Iflt(i) - 0.5
              DRIFTER(ng)%Tinfo(iygrd,l)=Jflt(i) - 0.5
            END DO
          END IF
      END IF
!-----------------------------------------------------------------------
!  Set float initial vertical level position, if inside application
!  grid.  If the initial float depth (in meters) is not found, release
!  float at the surface model level.
!-----------------------------------------------------------------------
!
      DO l=1,Nfloats(ng)
          DRIFTER(ng)%Fz0(l)=spv
          IF ((DRIFTER(ng)%Tinfo(ixgrd,l).ge.0.5_r8).and.               &
     &        (DRIFTER(ng)%Tinfo(iygrd,l).ge.0.5_r8).and.               &
     &        (DRIFTER(ng)%Tinfo(ixgrd,l).le.                           &
     &         REAL(Lm(ng),r8)+0.5_r8).and.                             &
     &        (DRIFTER(ng)%Tinfo(iygrd,l).le.                           &
     &         REAL(Mm(ng),r8)+0.5_r8)) THEN
            zfloat=DRIFTER(ng)%Tinfo(izgrd,l)
            DRIFTER(ng)%Fz0(l)=zfloat           ! Save original value
!            Kflt(l)=REAL(N(ng),r8)
            Kflt(l)=spv
            IF (zfloat.le.0.0_r8) THEN

              i=INT(DRIFTER(ng)%Tinfo(ixgrd,l)) ! Fractional positions
              j=INT(DRIFTER(ng)%Tinfo(iygrd,l)) ! are still in this cell
! Uncomment this to keep floats vertically in domain
!             IF (zfloat.lt.GRID(ng)%z_w(i,j,0)) THEN
!               zfloat=GRID(ng)%z_w(i,j,0)+5.0_r8
!               DRIFTER(ng)%Fz0(l)=zfloat
!             END IF
              DRIFTER(ng)%Tinfo(izgrd,l)=REAL(N(ng),r8)
              DO k=N(ng),1,-1
                IF ((GRID(ng)%z_w(i,j,k)-zfloat)*                       &
     &              (zfloat-GRID(ng)%z_w(i,j,k-1)).ge.0.0_r8) THEN
                  Kflt(l)=REAL(k-1,r8)+                                 &
     &                    (zfloat-GRID(ng)%z_w(i,j,k-1))/               &
     &                    GRID(ng)%Hz(i,j,k)
                END IF
              END DO
            END IF
          ELSE
            Kflt(l)=spv
          END IF
      END DO
      DO l=1,Nfloats(ng)
        DRIFTER(ng)%Tinfo(izgrd,l)=Kflt(l)
      END DO

! Print results
      DO l=1,Nfloats(ng)
        x=DRIFTER(ng)%Tinfo(ixgrd,l)
        y=DRIFTER(ng)%Tinfo(iygrd,l)
        z=DRIFTER(ng)%Tinfo(izgrd,l)
        time=INT(DRIFTER(ng)%Tinfo(itstr,l)/86400.)
        IF (z .ne. spv) write(stdout,10) x, y, z, 5, 0
      END DO
  10  FORMAT (3f10.2,2i6)
      RETURN
      END SUBROUTINE grid_coords
