!> \file: SimulationVars.F90
!> \author: Sayop Kim

MODULE SimulationVars_m
   USE parameters_m, ONLY : wp
   IMPLICIT NONE

   INTEGER :: IMAX, JMAX
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: xp

END MODULE SimulationVars_m
