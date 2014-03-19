!> \file: GridJacobian.F90
!> \author: Sayop Kim

MODULE GridJacobian_m
   USE Parameters_m, ONLY: wp
   IMPLICIT NONE
   REAL(KIND=wp), ALLOCATABLE, DIMENSION(:,:) :: PXPI, PXPJ, PYPI, PYPJ, &
                                                 JACOBIAN
CONTAINS

!-----------------------------------------------------------------------------!
  SUBROUTINE GridMetricsArrays()
!-----------------------------------------------------------------------------!
    USE SimulationVars_m, ONLY: IMIN, JMIN, IMAX, JMAX, INCELL, JNCELL, &
                                XP
    IMPLICIT NONE
    INTEGER :: I, J

    ALLOCATE(PXPI(INCELL, JNCELL))
    ALLOCATE(PXPJ(INCELL, JNCELL))
    ALLOCATE(PYPI(INCELL, JNCELL))
    ALLOCATE(PYPJ(INCELL, JNCELL))
    ALLOCATE(JACOBIAN(INCELL, JNCELL))
    PXPI = 0.0_wp
    PXPJ = 0.0_wp
    PYPI = 0.0_wp
    PYPJ = 0.0_wp

    DO I = IMIN + 1, IMAX - 1
      DO J = JMIN + 1, JMAX -1 
        PXPI(I,J) = 0.5_wp * (XP(1,I+1,J) - XP(1,I-1,J))
        PXPJ(I,J) = 0.5_wp * (XP(1,I,J+1) - XP(1,I,J-1))
        PYPI(I,J) = 0.5_wp * (XP(2,I+1,J) - XP(2,I-1,J))
        PYPJ(I,J) = 0.5_wp * (XP(2,I,J+1) - XP(2,I,J-1))
        JACOBIAN(I,J) = 1.0_wp / (PXPI(I,J) * PYPJ(I,J) - &
                                  PXPJ(I,J) * PYPI(I,J))
      END DO
    END DO


  END SUBROUTINE GridMetricsArrays
END MODULE GridJacobian_m
