!> \file: SimulationVars.F90
!> \author: Sayop Kim

MODULE SimulationVars_m
   USE parameters_m, ONLY : wp
   IMPLICIT NONE

   INTEGER :: IMAX, JMAX, NGL, IMIN, JMIN, INCELL, JNCELL
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: XP

END MODULE SimulationVars_m
