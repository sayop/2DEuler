!> \file: main.F90
!> \author: Sayop Kim

PROGRAM main
   USE Parameters_m, ONLY: wp
   USE ReadGrid_m, ONLY: INGRID
   USE MainLoop_m
   USE SimulationSetup_m, ONLY: InitializeGridArrays, SetInitialConditions
   USE GridJacobian_m, ONLY: InverseGridMetricsArrays, GridMetricsArrays
   USE io_m

   IMPLICIT NONE
   CHARACTER(LEN=filenameLength) :: outputfile = 'output.tec'

   CALL ReadInput()
   CALL InitializeGridArrays()
   CALL INGRID()
   CALL InverseGridMetricsArrays()
   CALL GridMetricsArrays()
   CALL SetInitialConditions()
   CALL MainLoop()
   CALL WriteTecPlot(outputfile,'"I","J","JACOBIAN","rho","u","v","p"')

END PROGRAM main
