!> \file: SimulationVars.F90
!> \author: Sayop Kim

MODULE SimulationVars_m
   USE parameters_m, ONLY : wp
   IMPLICIT NONE

   INTEGER :: NADV
   INTEGER :: IMAX, JMAX, NGL, IMIN, JMIN, INCELL, JNCELL, &
              IRES, JRES, IFINISH
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: XP, VEL
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:) :: RHO, PRES
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:) :: MACH
   REAL(KIND=wp) :: RMSerr, errLimit, RMS1err
   ! State and flux vectors: include 4 elements for 2D Euler
   ! Transformed state, flux vector: U, F, G
   ! Transformed state, flux vectors: UP, FP, GP
   ! Primative variable vector: V
   ! Total Stagnation enthalpy: H0
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: U, UO, F, G
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: UP, FP, GP
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: V
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:) :: H0

   ! Contravariant velocities: uCon
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: UCON

   ! flux update vector: DF
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:,:) :: DF

   ! Specific heat ratio: gamma
   REAL(KIND=wp) :: cgamma

   ! Time integration variables
   INTEGER :: NMAX
END MODULE SimulationVars_m
