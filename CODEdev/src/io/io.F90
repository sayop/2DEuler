!> \file: io.F90
!> \author: Sayop Kim
!> \brief: Provides routines to read input and write output

MODULE io_m
   USE Parameters_m, ONLY: wp

   IMPLICIT NONE
   INTEGER, PARAMETER :: IOunit = 10, filenameLength = 64
   CHARACTER(LEN=50) :: prjTitle
CONTAINS

!-----------------------------------------------------------------------------!
   SUBROUTINE ReadInput()
!-----------------------------------------------------------------------------!
!  Read input files for transformation 1:
!-----------------------------------------------------------------------------!
     USE SimulationVars_m, ONLY: imax, jmax
     IMPLICIT NONE
     INTEGER :: ios
     CHARACTER(LEN=8) :: inputVar

     OPEN(IOunit, FILE = 'input.dat', FORM = 'FORMATTED', ACTION = 'READ', &
           STATUS = 'OLD', IOSTAT = ios)
     IF(ios /= 0) THEN
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Fatal error: Could not open the input data file."
        RETURN
     ELSE
        WRITE(*,'(a)') ""
        WRITE(*,'(a)') "Reading input file for transformation 1"
     ENDIF

     READ(IOunit,*)
     READ(IOunit,'(a)') prjTitle
     WRITE(*,'(4a)') 'Project Title:', '"',TRIM(prjTitle),'"'
     READ(IOunit,*) inputVar, imax
     WRITE(*,'(a,i6)') inputVar,imax
     READ(IOunit,*) inputVar, jmax
     WRITE(*,'(a,i6)') inputVar,jmax

  END SUBROUTINE ReadInput
   
END MODULE io_m
