!> \file: SimulationVars.F90
!> \author: Sayop Kim

MODULE SimulationVars_m
   USE parameters_m, ONLY : wp
   IMPLICIT NONE

   INTEGER :: IMAX, JMAX, NGL, IMIN, JMIN, INCELL, JNCELL, &
              IRES, JRES
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: XP, VEL
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:) :: RHO, PRES
   ! State and flux vectors: include 4 elements for 2D Euler
   ! Transformed state, flux vector: U, F, G
   ! Transformed state, flux vectors: UP, FP, GP
   ! Primative variable vector: V
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: U, F, G
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: UP, FP, GP
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: V

END MODULE SimulationVars_m
