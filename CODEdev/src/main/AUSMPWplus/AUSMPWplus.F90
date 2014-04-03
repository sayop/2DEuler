!> \file: AUSMPWplus.F90
!> \author: Sayop Kim

MODULE AUSMPWplus_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE
   REAL(KIND=wp) :: alpha

CONTAINS
!-----------------------------------------------------------------------------!
  SUBROUTINE SetAUSMPWplus()
!-----------------------------------------------------------------------------!
    USE SimulationVars_m, ONLY: imin, jmin, imax, jmax, incell, jncell, &
                                U, V, DF, cgamma
    USE GridJacobian_m, ONLY: PIPX, PIPY, PJPX, PJPY, JACOBIAN

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
    REAL(KIND=wp) :: XIX, XIY, ETAX, ETAY, A1, &
                     rr, uu, vv, Pl, Pr, h0L, h0R, h0norm, &
                     utildL, vtildL, utildR, vtildR, Cavg, &
                     MtildL, MtildR, MtildLplus, MtildRminus, &
                     Pplus, Pminus, Pmin, fl, fr
                     
    REAL(KIND=wp), DIMENSION(4) :: UL, UR

    !-----------------------------------
    !  
    !  o-------o-------o-------o-------o
    !  |       |       |       |       |
    !  |       G       G       G       |
    !  |       |       |       |       |
    !  o---F---o---F---o---F---o---F---o
    !  |       |       |       |       |
    !  |       G       G       G       |
    !  |       |       |       |       |
    !  o---F---o---F---o---F---o---F---o
    !  |       |       |       |       |
    !  |       G       G       G       |
    !  |       |       |       |       |
    !  o-------o-------o-------o-------o
    !
    ! o: Grid points
    ! F: node for FP flux variables
    ! G: node for GP flux variables

    ! Set transformed flux in along j-const lines at every (i+1/2) points
    DO j = jmin + 1, jmax - 1
      DO i = imin, imax - 1
        ! Arithmatic average of grid metrics at i + 1/2
        XIX = 0.5_wp * (PIPX(i,j) + PIPX(i+1,j))
        XIY = 0.5_wp * (PIPY(i,j) + PIPY(i+1,j))
        ETAX = 0.5_wp * (PJPX(i,j) + PJPX(i+1,j))
        ETAY = 0.5_wp * (PJPY(i,j) + PJPY(i+1,j))
        A1 = sqrt(XIX ** 2 + XIY ** 2)
        ! Set u- and v-velcotiy at i + 1/2
        ! First calculate left-extrapolated state vector and convert
        UL = LeftExtrapolateU('i',i,j)
        rr = UL(1)
        uu = UL(2) / rr
        vv = UL(3) / rr
        Pl = (cgamma - 1.0_wp) * ( UL(4) - &
             0.5_wp * rr * (uu ** 2 + vv ** 2) )
        h0L = Pl * cgamma / (rr * (cgamma - 1.0_wp)) + &
              0.5_wp * (uu ** 2 + vv ** 2)
        utildL = (XIX * uu + XIY * vv) / A1
        vtildL = (ETAX * uu + ETAY * vv) / A1
        ! Then calculate right-extrapolated state vector from i+1 point
        UR = RightExtrapolateU('i',i+1,j)
        rr = UR(1)
        uu = UR(2) / rr
        vv = UR(3) / rr
        Pr = (cgamma - 1.0_wp) * ( UR(4) - &
             0.5_wp * rr * (uu ** 2 + vv ** 2) )
        h0R = Pr * cgamma / (rr * (cgamma - 1.0_wp)) + &
              0.5_wp * (uu ** 2 + vv ** 2)
        utildR = (XIX * uu + XIY * vv) / A1
        vtildR = (ETAX * uu + ETAY * vv) / A1
        ! Calculate stagnation enthalpy normal to the interface
        h0norm = 0.5_wp * (h0L + h0R - 0.5_wp * (vtildL ** 2 + vtildR ** 2))
        ! Calculate cell averaged speed of sound
        ! Use Cavg for calculating Cs and then update to Cavg
        ! Use rr temporarily to store half of sum of utildL and utildR
        Cavg = sqrt(2.0_wp * h0norm * (cgamma - 1.0_wp) / (cgamma + 1.0_wp))
        rr = 0.5_wp * (utildL + utildR)
        IF(rr .GE. 0.0) THEN
          Cavg = Cavg ** 2 / max(abs(utildL),Cavg)
        ELSE
          Cavg = Cavg ** 2 / max(abs(utildR),Cavg)
        END IF
        ! Calculate cell face Mach numbers
        MtildL = utildL / Cavg
        MtildR = utildR / Cavg
        ! Calculate split Mach numbers and pressures
        IF(abs(MtildL) .LE. 1.0) THEN
          MtildLplus = 0.25_wp * (MtildL + 1.0_wp) ** 2
          Pplus = MtildLplus * (2.0_wp - MtildL) + alpha * MtildL * &
                  (MtildL ** 2 - 1.0_wp) ** 2
        ELSE
          MtildLplus = 0.5_wp * (MtildL + abs(MtildL))
          Pplus = 0.5_wp * (1.0_wp + sign(1.0_wp,MtildL))
        END IF
        IF(abs(MtildR) .LE. 1.0) THEN
          MtildRminus = -0.25_wp * (MtildR - 1.0_wp) ** 2
          Pminus = -MtildRminus * (2.0_wp + MtildR) - alpha * MtildR * &
                   (MtildR ** 2 - 1.0_wp) ** 2
        ELSE
          MtildRminus = 0.5_wp * (MtildR - abs(MtildR))
          Pminus = 0.5_wp * (1.0_wp - sign(1.0_wp,MtildR))
        END IF
        ! Set pressure weighting terms
        ! Use rr temporarily to store Ps and w
        rr = Pplus * Pl + Pminus * Pr
        Pmin = min(V(4,i,j-1), V(4,i,j+1), V(4,i+1,j-1), V(4,i+1,j+1))
        IF(rr .LE. 0.0) THEN
          fl = 0.0_wp
          fr = 0.0_wp
        ELSE
          fl = (Pl / rr - 1.0_wp) * min(1.0_wp,Pmin/min(Pl,Pr)) ** 2
          fr = (Pr / rr - 1.0_wp) * min(1.0_wp,Pmin/min(Pl,Pr)) ** 2
        END IF
        rr = 1.0_wp - min(Pl/Pr, Pr/Pl)
        ! Apply the weightings to MtildLplus, MtildRminus
        ! Now MtildLplus and MtildRminus become averaged Mach number
        ! on the cell face
        IF(MtildLplus + MtildRminus .GT. 0.0) THEN
          MtildLplus = MtildLplus + MtildRminus * ((1.0_wp - rr) * &
                       (1.0_wp + fr) - fl)
          MtildRminus = MtildRminus * rr * (1.0_wp + fr)
        ELSE
          MtildLplus = MtildLplus * rr * (1.0_wp + fl)
          MtildRminus = MtildRminus + MtildLplus * ((1.0_wp - rr) * &
                        (1.0_wp + fl) - fr)
        END IF
        ! Update FPhalf vector
        ! Use rr as a temporary variable to store coefficients
        DO n = 1, 4
          rr = MtildLplus * Cavg * A1 * UL(n)
          rr = rr + MtildRminus * Cavg * UR(n)
          FPhalf(n,i,j) = rr / JACOBIAN(i,j)
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

  FUNCTION LeftExtrapolateXE(axis,i,j) RESULT(XE)
    USE SimulationVars_m, ONLY: PIPX, PIPY, PJPX, PJPY
    IMPLICIT NONE
    INTEGER :: i, j, n
    CHARACTER(LEN=1) :: axis
    REAL(KIND=wp) :: limiter, XE

    limiter = 1.0
    IF(axis .EQ. 'i') THEN
      
    ELSEIF(axis .EQ. 'j') THEN
      
      
    END IF

  END FUNCTION

  FUNCTION RightExtrapolateU(axis,i,j) RESULT(UR)
    USE SimulationVars_m, ONLY: U
    IMPLICIT NONE
    INTEGER :: i, j, n, ip, jp
    CHARACTER(LEN=1) :: axis
    REAL(KIND=wp) :: limiter
    REAL(KIND=wp), DIMENSION(4) :: UR

    limiter = 1.0
    IF(axis .EQ. 'i') THEN
      ip = 1
      jp = 0
    ELSEIF(axis .EQ. 'j') THEN
      ip = 0
      jp = 1
    END IF
    DO n = 1, 4
      UR(n) = U(n,i,j) + limiter * (U(n,i,j) - U(n,i+ip,j+jp)) * 0.5_wp
    END DO
  END FUNCTION
END MODULE AUSMPWplus_m
