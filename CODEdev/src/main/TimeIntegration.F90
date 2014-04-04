!> \file: TimeIntegration.F90
!> \author: Sayop Kim

MODULE TimeIntegration_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE
   ! Flux difference
   REAL(KIND=wp) :: CFL, T, DT
CONTAINS

!-----------------------------------------------------------------------------!
  SUBROUTINE SetTimeStep()
!-----------------------------------------------------------------------------!
    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, cgamma, V, UCON
    USE GridJacobian_m, ONLY: PIPX, PJPX, PIPY, PJPY
    INTEGER :: i, j
    ! Arbitrary variable for calculating junk of time step
    REAL(KIND=wp) :: c

    dt = 1.0e+9
    DO i = imin, imax
      DO j = jmin, jmax
        c = sqrt(cgamma * V(4,i,j) / V(1,i,j))
        c = c * sqrt(PIPX(i,j) ** 2 + PIPY(i,j) ** 2 + PJPX(i,j) ** 2 + &
                     PJPY(i,j) ** 2 + 2.0_wp * abs(PIPX(i,j) * PJPX(i,j) + &
                     PIPY(i,j) * PJPY(i,j)))
        c = c + abs(UCON(1,i,j)) + abs(UCON(2,i,j))
        c = CFL / c
        dt = min(dt, c)
      END DO
    END DO
    t = t + dt
  END SUBROUTINE SetTimeStep

!-----------------------------------------------------------------------------!
  SUBROUTINE TimeIntegration()
!-----------------------------------------------------------------------------!
    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, U, DF
    USE GridJacobian_m, ONLY: JACOBIAN
    USE AUSMPWplus_m, ONLY: SetAUSMPWplus

    INTEGER :: i, j, n
    DF = 0.0_wp

    CALL SetAUSMPWplus() 
    DO i = imin + 1, imax - 1
      DO j = jmin + 1, jmax - 1
        DO n = 1, 4
          U(n,i,j) = U(n,i,j) - DF(n,i,j) * dt * JACOBIAN(i,j)
        END DO
       ! write(*,*) 'sayop_',DF(1,i,j),DF(2,i,j),DF(3,i,j)
      END DO
    END DO
  END SUBROUTINE TimeIntegration

!-----------------------------------------------------------------------------!
  FUNCTION CheckConvergence() RESULT(ifinish)
!-----------------------------------------------------------------------------!
    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, U, nadv, RMSerr, &
                                errLimit, errSum

    INTEGER :: i, j, n, ifinish, nTotal
    REAL(KIND=wp) :: error
  
    ifinish = 0
    error = 0.0_wp
    nTotal = 0
    DO i = imin, imax
      DO j = jmin, jmax
        DO n = 1, 4
          nTotal = nTotal + 1
          error = error + U(n,i,j)
        END DO
      END DO
    END DO

    RMSerr = sqrt((error - errSum) ** 2 / nTotal)
    IF(RMSerr .LT. errLimit) ifinish = 1
    errSum = error
  END FUNCTION CheckConvergence
END MODULE TimeIntegration_m
