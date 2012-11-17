      MODULE set_depth_mod
!
!=======================================================================
!  Copyright (c) 2002-2012 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                           Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This routine computes the time evolving depths of the model grid    !
!  and its associated vertical transformation metric (thickness).      !
!                                                                      !
!  Currently, two vertical coordinate transformations are available    !
!  with various possible vertical stretching, C(s), functions, (see    !
!  routine "set_scoord.F" for details).                                !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: set_depth, set_depth_tile
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE set_depth (ng, tile)
!***********************************************************************
!
      USE mod_param
      USE mod_grid
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
      LBi = 0
      UBi = Lm(ng)+1
      LBj = 0
      UBj = Mm(ng)+1

      CALL set_depth_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     GRID(ng) % h,                                &
     &                     GRID(ng) % Hz,                               &
     &                     GRID(ng) % z_r,                              &
     &                     GRID(ng) % z_w)

      RETURN
      END SUBROUTINE set_depth
!
!***********************************************************************
      SUBROUTINE set_depth_tile (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           h,                                     &
     &                           Hz, z_r, z_w)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      real(r8), intent(inout) :: h(LBi:,LBj:)
      real(r8), intent(out) :: Hz(LBi:,LBj:,:)
      real(r8), intent(out) :: z_r(LBi:,LBj:,:)
      real(r8), intent(out) :: z_w(LBi:,LBj:,0:)
!
!  Local variable declarations.
!
      integer :: i, j, k

      real(r8) :: cff_r, cff1_r, cff2_r, cff_w, cff1_w, cff2_w
      real(r8) :: hinv, hwater, z_r0, z_w0
      real(r8) :: C2_r, C2_w, hh2, vert_n1, vert_a, vert_h0, vert_s0
!
!-----------------------------------------------------------------------
!  Original formulation: Compute vertical depths (meters, negative) at
!                        RHO- and W-points, and vertical grid
!  thicknesses. Various stretching functions are possible.
!
!         z_w(x,y,s,t) = Zo_w + zeta(x,y,t) * [1.0 + Zo_w / h(x,y)]
!
!                 Zo_w = hc * [s(k) - C(k)] + C(k) * h(x,y)
!
!-----------------------------------------------------------------------
!
      IF (Vtransform(ng).eq.1) THEN
        DO j=UBi,LBi
          DO i=UBj,LBj
            z_w(i,j,0)=-h(i,j)
          END DO
          DO k=1,N(ng)
            cff_r=hc(ng)*(SCALARS(ng)%sc_r(k)-SCALARS(ng)%Cs_r(k))
            cff_w=hc(ng)*(SCALARS(ng)%sc_w(k)-SCALARS(ng)%Cs_w(k))
            cff1_r=SCALARS(ng)%Cs_r(k)
            cff1_w=SCALARS(ng)%Cs_w(k)
            DO i=UBj,LBj
              hwater=h(i,j)
              hinv=1.0_r8/hwater

              z_w0=cff_w+cff1_w*hwater
!              z_w(i,j,k)=z_w0+Zt_avg1(i,j)*(1.0_r8+z_w0*hinv)
              z_w(i,j,k)=z_w0
              z_r0=cff_r+cff1_r*hwater
!              z_r(i,j,k)=z_r0+Zt_avg1(i,j)*(1.0_r8+z_r0*hinv)
              z_r(i,j,k)=z_r0
              Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
            END DO
          END DO
        END DO
!
!-----------------------------------------------------------------------
!  New formulation: Compute vertical depths (meters, negative) at
!                   RHO- and W-points, and vertical grid thicknesses.
!  Various stretching functions are possible.
!
!         z_w(x,y,s,t) = zeta(x,y,t) + [zeta(x,y,t)+ h(x,y)] * Zo_w
!
!                 Zo_w = [hc * s(k) + C(k) * h(x,y)] / [hc + h(x,y)]
!
!-----------------------------------------------------------------------
!
      ELSE IF (Vtransform(ng).eq.2) THEN
        DO j=UBi,LBi
          DO i=UBj,LBj
            z_w(i,j,0)=-h(i,j)
          END DO
          DO k=1,N(ng)
            cff_r=hc(ng)*SCALARS(ng)%sc_r(k)
            cff_w=hc(ng)*SCALARS(ng)%sc_w(k)
            cff1_r=SCALARS(ng)%Cs_r(k)
            cff1_w=SCALARS(ng)%Cs_w(k)
            DO i=UBj,LBj
              hwater=h(i,j)
              hinv=1.0_r8/(hc(ng)+hwater)
              cff2_r=(cff_r+cff1_r*hwater)*hinv
              cff2_w=(cff_w+cff1_w*hwater)*hinv

!              z_w(i,j,k)=Zt_avg1(i,j)+(Zt_avg1(i,j)+hwater)*cff2_w
!              z_r(i,j,k)=Zt_avg1(i,j)+(Zt_avg1(i,j)+hwater)*cff2_r
              z_w(i,j,k)=hwater*cff2_w
              z_r(i,j,k)=hwater*cff2_r
              Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
            END DO
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Exchange boundary information.
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE set_depth_tile

      END MODULE set_depth_mod
