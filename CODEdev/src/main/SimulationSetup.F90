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
                                XP

    imin = ngl
    jmin = ngl
    incell = 2*ngl + imax
    jncell = 2*ngl + jmax
    imax = ngl + imax
    jmax = ngl + jmax
    ALLOCATE(XP(3,incell,jncell))

  END SUBROUTINE InitializeGridArrays
END MODULE SimulationSetup_m
