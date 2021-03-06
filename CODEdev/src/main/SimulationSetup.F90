!> \file: SimulationSetup.F90
!> \author: Sayop Kim

MODULE SimulationSetup_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

CONTAINS

!-----------------------------------------------------------------------------!
  SUBROUTINE InitializeGridArrays()
!-----------------------------------------------------------------------------!
! imax: number of grid points in i-drection
! jmax: number of grid points in j-direction
    USE SimulationVars_m

    imin = ngl + 1
    jmin = ngl + 1
    incell = 2*ngl + imax
    jncell = 2*ngl + jmax
    imax = ngl + imax
    jmax = ngl + jmax
    ires = imax - imin + 1
    jres = jmax - jmin + 1

    ! x, y, z
    ALLOCATE(XP(3,incell,jncell))

    ALLOCATE(U(4,incell,jncell))
    ALLOCATE(UO(4,incell,jncell))
    ALLOCATE(F(4,incell,jncell))
    ALLOCATE(G(4,incell,jncell))
    ALLOCATE(UP(4,incell,jncell))
    ALLOCATE(FP(4,incell,jncell))
    ALLOCATE(GP(4,incell,jncell))
    ALLOCATE(V(4,incell,jncell))
    ALLOCATE(UCON(2,incell,jncell))
    ALLOCATE(H0(incell,jncell))
    ALLOCATE(DF(4,incell,jncell))
    ALLOCATE(MACH(incell,jncell))

  END SUBROUTINE InitializeGridArrays

!-----------------------------------------------------------------------------!
  SUBROUTINE SetInitialConditions()
!-----------------------------------------------------------------------------!

    USE SimulationVars_m, ONLY: V, cgamma, ifinish, U, UO
    USE io_m, ONLY: rhoinit, uinit, vinit, pinit, cgammainit

    ! Set initial primative vector
    V(1,:,:) = rhoinit
    V(2,:,:) = uinit
    V(3,:,:) = vinit
    V(4,:,:) = pinit
    !V(4,:,:) = 1.0_wp / cgammainit
    cgamma = cgammainit
    
    ! Set initial state vector elements
    !U(1,:,:) = rhoinit
    !U(2,:,:) = rhoinit * uinit
    !U(3,:,:) = rhoinit * vinit
    !U(4,:,:) = pinit / (cgammainit - 1.0_wp) + 0.5_wp * rhoinit * &
    !           (uinit ** 2 + vinit ** 2)
    ! To calculate initial H0 vector and MACH values
    CALL SetTransformedVariables()
    ! To calculate initial H0 vector and MACH values
    CALL SetPrimativeVariables()

    !Set ifinish variable
    ifinish = 0

    ! Set UO
    UO = U
  END SUBROUTINE SetInitialConditions

!-----------------------------------------------------------------------------!
  SUBROUTINE SetPrimativeVariables()
!-----------------------------------------------------------------------------!
! U -> V
    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, V, U, cgamma, H0, &
                                MACH
    INTEGER :: i, j
    REAL(KIND=wp) :: aa !Speed of sound
    REAL(KIND=wp) :: speed !Speed at each node

    DO i = imin, imax
      DO j = jmin, jmax
        ! density
        V(1,i,j) = U(1,i,j)
        ! u-velocity
        V(2,i,j) = U(2,i,j) / U(1,i,j)
        ! v-velocity
        V(3,i,j) = U(3,i,j) / U(1,i,j)
        ! pressure
        V(4,i,j) = (cgamma - 1.0_wp) * (&
                   U(4,i,j) - 0.5_wp * V(1,i,j) * (&
                   V(2,i,j) ** 2 + V(3,i,j) ** 2) )
        ! Stagnation enthalpy: H0
        H0(i,j) = (cgamma/(cgamma - 1.0_wp)) * V(4,i,j) / V(1,i,j) + &
                  0.5_wp * (V(2,i,j) ** 2 + V(3,i,j) ** 2)
        aa = sqrt(cgamma * V(4,i,j) / V(1,i,j))
        speed = sqrt(V(2,i,j) ** 2 + V(3,i,j) ** 2)
        MACH(i,j) = speed / aa
      END DO
    END DO
  END SUBROUTINE SetPrimativeVariables

!-----------------------------------------------------------------------------!
  SUBROUTINE SetTransformedVariables()
!-----------------------------------------------------------------------------!
! V -> U
    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, V, U, UCON, cgamma
    USE GridJacobian_m, ONLY: PIPX, PJPX, PIPY, PJPY, JACOBIAN
    INTEGER :: i, j

    DO i = imin, imax
      DO j = jmin, jmax
        U(1,i,j) = V(1,i,j)
        U(2,i,j) = V(1,i,j) * V(2,i,j)
        U(3,i,j) = V(1,i,j) * V(3,i,j)
        U(4,i,j) = V(4,i,j) / (cgamma - 1.0_wp) + 0.5_wp * V(1,i,j) *&
                   (V(2,i,j) ** 2 + V(3,i,j) ** 2)
        ! Set contravariant u velocity in i-direction
        UCON(1,i,j) = PIPX(i,j) * V(2,i,j) + PIPY(i,j) * V(3,i,j)
        ! Set contravariant v velocity in j-direction
        UCON(2,i,j) = PJPX(i,j) * V(2,i,j) + PJPY(i,j) * V(3,i,j)
      END DO
    END DO
  END SUBROUTINE SetTransformedVariables

!-----------------------------------------------------------------------------!
  SUBROUTINE SetBoundaryConditions()
!-----------------------------------------------------------------------------!

    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, V, U, UCON, H0, &
                                cgamma
    USE io_m, ONLY: rhoinit, uinit, vinit, pinit
    USE GridJacobian_m, ONLY: PIPX, PJPX, PIPY, PJPY, JACOBIAN
    INTEGER :: i, j
    REAL(KIND=wp) :: del2, del3

    ! Set inflow boundary condition
    i = imin
    DO j = jmin, jmax
      V(1,i,j) = rhoinit
      V(2,i,j) = uinit
      V(3,i,j) = vinit
      V(4,i,j) = pinit
    END DO

    ! Set outflow boundary condition
    i = imax
    DO j = jmin, jmax
      V(1,i,j) = V(1,i-1,j)
      V(2,i,j) = V(2,i-1,j)
      V(3,i,j) = V(3,i-1,j)
      V(4,i,j) = V(4,i-1,j)
    END DO

    ! Set bottom wall boundary condition
    j = jmin
    DO i = imin + 1, imax - 1
      !Set u, v velocities, density and pressure
      del2 = sqrt( PIPX(i,j) ** 2 + PIPY(i,j) ** 2 ) / &
             sqrt( PIPX(i,j+1) ** 2 + PIPY(i,j+1) ** 2)
      del3 = sqrt( PIPX(i,j) ** 2 + PIPY(i,j) ** 2 ) / &
             sqrt( PIPX(i,j+2) ** 2 + PIPY(i,j+2) ** 2)
      V(2,i,j) = PJPY(i,j) / JACOBIAN(i,j) * ( &
                 2.0_wp * UCON(1,i,j+1) * del2 - UCON(1,i,j+2) * del3)
      V(3,i,j) = -PJPX(i,j) / JACOBIAN(i,j) * ( &
                 2.0_wp * UCON(1,i,j+1) * del2 - UCON(1,i,j+2) * del3)
      V(4,i,j) = V(4,i,j+1)
      V(1,i,j) = V(4,i,j) * cgamma / ( &
                 (cgamma - 1.0_wp) * (H0(i,j+1) - 0.5_wp * ( &
                 V(2,i,j) ** 2 + V(3,i,j) ** 3)) )
    END DO

    ! Set top wall boundary condition
    j = jmax
    DO i = imin + 1, imax - 1
      !Set u, v velocities, density and pressure
      del2 = sqrt( PIPX(i,j) ** 2 + PIPY(i,j) ** 2 ) / &
             sqrt( PIPX(i,j-1) ** 2 + PIPY(i,j-1) ** 2)
      del3 = sqrt( PIPX(i,j) ** 2 + PIPY(i,j) ** 2 ) / &
             sqrt( PIPX(i,j-2) ** 2 + PIPY(i,j-2) ** 2)
      V(2,i,j) = PJPY(i,j) / JACOBIAN(i,j) * ( &
                 2.0_wp * UCON(1,i,j-1) * del2 - UCON(1,i,j-2) * del3)
      V(3,i,j) = -PJPX(i,j) / JACOBIAN(i,j) * ( &
                 2.0_wp * UCON(1,i,j-1) * del2 - UCON(1,i,j-2) * del3)
      V(4,i,j) = V(4,i,j-1)
      V(1,i,j) = V(4,i,j) * cgamma / ( &
                 (cgamma - 1.0_wp) * (H0(i,j-1) - 0.5_wp * ( & 
                 V(2,i,j) ** 2 + V(3,i,j) ** 2)) )
    END DO
    ! To set transformed flux vector and contravariant velocity
    CALL SetTransformedVariables()
  END SUBROUTINE SetBoundaryConditions
END MODULE SimulationSetup_m
