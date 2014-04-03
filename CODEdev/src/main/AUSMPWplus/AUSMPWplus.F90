!> \file: AUSMPWplus.F90
!> \author: Sayop Kim

MODULE AUSMPWplus_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

CONTAINS
!-----------------------------------------------------------------------------!
  SUBROUTINE SetAUSMPWplus()
!-----------------------------------------------------------------------------!
    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, incell, jncell, &
                                U, DF

    INTEGER i, j, n
    ! FPpHalf: Transformed flux in i-direction at (i+1/2,j)
    ! FPmHalf: Transformed flux in i-direction at (i-1/2,j)
    ! GPpHalf: Transformed flux in j-direction at (i,j+1/2)
    ! GPmHalf: Transformed flux in j-direction at (i,j-1/2)
    !REAL(KIND=wp), DIMENSION(4) :: FPpHalf, FPmHalf, GPpHalf, GPmHalf
    ! UL: Left-extrapolated state vector at (i+1/2,j) or (i,j+1/2)
    ! UR: Right-extrapolated state vector at (i+1/2,j) or (i,j+1/2)
    !REAL(KIND=wp), DIMENSION(4) :: UL, UR

    ! FPhalf: Transformed flux in i-direction at i+1/2
    ! GPhalf: Transformed flux in j-direction at j+1/2
    REAL(KIND=wp), DIMENSION(4,incell,jncell) :: FPhalf, GPhalf
    DO i = imin, imax - 1
      DO j = jmin, jmax - 1
        DO n = 1, 4
          
        END DO
      END DO
    END DO

    DO i = imin + 1, imax - 1
      DO j = jmin + 1, jmax - 1
        DO n = 1, 4
          DF(n,i,j) = FPhalf(n,i,j) - FPhalf(n,i-1,j) + &
                      GPhalf(n,i,j) - GPhalf(n,i,j-1)
        END DO
      END DO
    END DO

!    UL = LeftExtrapolateU('i',2,3)
  END SUBROUTINE SetAUSMPWplus

  FUNCTION LeftExtrapolateU(axis,i,j) RESULT(UL)
    USE SimulationVars_m, ONLY: U
    IMPLICIT NONE
    INTEGER :: i, j, n, im, jm
    CHARACTER(LEN=1) :: axis
    REAL(KIND=wp) :: limiter
    REAL(KIND=wp), DIMENSION(4) :: UL

    limiter = 1.0
    IF(axis .EQ. 'i') THEN
      im = -1
      jm = 0
    ELSEIF(axis .EQ. 'j') THEN
      im = 0
      jm = -1
    END IF
    DO n = 1, 4
      UL(n) = U(n,i,j) + limiter * (U(n,i,j) - U(n,i-im,j-jm)) * 0.5_wp
    END DO
  END FUNCTION

END MODULE AUSMPWplus_m
