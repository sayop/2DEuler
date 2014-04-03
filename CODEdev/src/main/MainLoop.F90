!> \file: MainLoop.F90
!> \author: Sayop Kim

MODULE MainLoop_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------!
  SUBROUTINE MainLoop()
!-----------------------------------------------------------------------------!
    USE TimeIntegration_m
    USE SimulationVars_m, ONLY: nadv, nmax
    USE SimulationSetup_m, ONLY: SetBoundaryConditions, SetTransformedVariables

    TimeLoop: DO nadv = 1, nmax
      CALL SetTransformedVariables()
      CALL SetBoundaryConditions()
      CALL SetTimeStep()
      CALL TimeIntegration()
      WRITE(*,*) 'NADV=',nadv,'T=',t, 'DT=',dt
    END DO TimeLoop

  END SUBROUTINE MainLoop
END MODULE MainLoop_m

