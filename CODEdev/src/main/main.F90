!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE ReadGrid_m, ONLY: INGRID
   USE SimulationSetup_m, ONLY: InitializeGridArrays
   USE GridJacobian_m, ONLY: InverseGridMetricsArrays, GridMetricsArrays
   USE io_m

   IMPLICIT NONE
   CHARACTER(LEN=filenameLength) :: outputfile = 'output.tec'

   CALL ReadInput()
   CALL InitializeGridArrays()
   CALL INGRID()
   CALL InverseGridMetricsArrays()
   CALL GridMetricsArrays()
   CALL WriteTecPlot(outputfile,'"I","J","JACOBIAN"')

END PROGRAM main
