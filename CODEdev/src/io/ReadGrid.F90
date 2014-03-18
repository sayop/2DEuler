!> \file: ReadGrid.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read grid file

MODULE ReadGrid_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE

   PUBLIC :: INGRID

CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE INGRID()
!-----------------------------------------------------------------------------!
!  Read grid file
!  X: X locations of grid points
!  Y: Y locations of grid points
!  IMAX: maximum number of points in the i-direction
!  JMAX: maximum number of points in the j-direction
!-----------------------------------------------------------------------------!
     USE SimulationVars_m, ONLY: XP, IMAX, JMAX

     IMPLICIT NONE
     INTEGER :: I, J 
     ALLOCATE(XP(3,IMAX,JMAX))

   END SUBROUTINE INGRID

END MODULE ReadGrid_m
