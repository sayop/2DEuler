!> \file: SimulationVars.F90
!> \author: Sayop Kim

MODULE SimulationVars_m
   USE parameters_m, ONLY : wp
   IMPLICIT NONE

   INTEGER :: IMAX, JMAX, NGL, IMIN, JMIN, INCELL, JNCELL, &
              IRES, JRES
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: XP, VEL
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:) :: RHO, PRES
   ! State vector: UU includes 'nelm' elements
   ! Transformed state vector: UP
   ! Transformed flux vectors: FP, GP
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: UU
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: UP, FP, GP

END MODULE SimulationVars_m
