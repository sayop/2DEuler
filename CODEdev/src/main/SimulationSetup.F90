!> \file: SimulationSetup.F90
!> \author: Sayop Kim

MODULE SimulationSetup_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------!
  SUBROUTINE InitializeGridArrays()
!-----------------------------------------------------------------------------!
! imax: number of grid points in i-drection
! jmax: number of grid points in j-direction
    USE SimulationVars_m, ONLY: imax, jmax, ngl, imin, jmin, incell, jncell, &
                                ires, jres, XP

    imin = ngl + 1
    jmin = ngl + 1
    incell = 2*ngl + imax
    jncell = 2*ngl + jmax
    imax = ngl + imax
    jmax = ngl + jmax
    ires = imax - imin + 1
    jres = jmax - jmin + 1

    ! x, y, z
    ALLOCATE(XP(3,incell,jncell))

    ALLOCATE(U(4,incell,jncell))
    ALLOCATE(F(4,incell,jncell))
    ALLOCATE(G(4,incell,jncell))
    ALLOCATE(UP(4,incell,jncell))
    ALLOCATE(FP(4,incell,jncell))
    ALLOCATE(GP(4,incell,jncell))
    ALLOCATE(V(4,incell,jncell))

  END SUBROUTINE InitializeGridArrays

!-----------------------------------------------------------------------------!
  SUBROUTINE SetInitialConditions()
!-----------------------------------------------------------------------------!

  USE SimulationVars_m, ONLY: imin, jmin, imax, jmax


  END SUBROUTINE SetInitialConditions
END MODULE SimulationSetup_m
