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
    USE SimulationVars_m, ONLY: nadv, nmax, imin, imax, jmin, jmax, V, U, RMSerr
    USE SimulationSetup_m, ONLY: SetBoundaryConditions, SetTransformedVariables, &
                                 SetPrimativeVariables

    INTEGER :: i,j
    TimeLoop: DO nadv = 1, nmax
      CALL SetBoundaryConditions()
      CALL SetTimeStep()
      CALL TimeIntegration()
      CALL SetPrimativeVariables()
      ! Update Contravariant velocities that need to be updated
      ! in SetBoundaryConditions
      !CALL SetTransformedVariables()
      WRITE(*,*) 'NADV=',nadv,'T=',t, 'DT=',dt, 'RMSerr=',RMSerr
      IF(CheckConvergence() .EQ. 1) THEN
        WRITE(*,*) 'NORMAL TERMINATION AT NADV=',nadv,'and RMSerr=',RMSerr
        return
      END IF
    END DO TimeLoop

  END SUBROUTINE MainLoop
END MODULE MainLoop_m

