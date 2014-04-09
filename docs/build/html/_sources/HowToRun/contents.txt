How to run the code
===================


Machine platform for development
--------------------------------

This 2DEuler code has been developed on personal computer operating on linux system (Ubuntu Linux 3.2.0-38-generic x86_64). Machine specification is summarized as shown below:

vendor_id       : GenuineIntel

cpu family      : 6

model name      : Intel(R) Core(TM) i7-2600 CPU @ 3.40GHz

cpu cores       : 4

Memory          : 16418112 kB



Code setup
----------

The 2DEuler source code has been developed with version management tool, GIT. The git repository was built on 'github.com'. Thus, the source code as well as related document files can be cloned into user's local machine by following command::

   $ git clone http://github.com/sayop/2DEuler.git

If you open the git-cloned folder **CouetteFlow**, you will see two different folders and README file. The **CODEdev** folder contains again **bin** folder, **Python** folder, and **src** folder. In order to run the code, use should run **setup.sh** script in the **bin** folder. **Python** folder contains python scripts that are used to postprocess data. It may contain **build** folder, which might have been created in the different platform. Thus it is recommended that user should remove **build** folder before setting up the code. Note that the **setup.sh** script will run **cmake** command. Thus, make sure to have cmake installed on your system::

  $ rm -rf build
  $ ./setup.sh
  -- The C compiler identification is GNU 4.8.1
  -- The CXX compiler identification is GNU 4.8.1
  -- Check for working C compiler: /usr/bin/cc
  -- Check for working C compiler: /usr/bin/cc -- works
  -- Detecting C compiler ABI info
  -- Detecting C compiler ABI info - done
  -- Check for working CXX compiler: /usr/bin/c++
  -- Check for working CXX compiler: /usr/bin/c++ -- works
  -- Detecting CXX compiler ABI info
  -- Detecting CXX compiler ABI info - done
  -- The Fortran compiler identification is GNU
  -- Check for working Fortran compiler: /usr/bin/gfortran
  -- Check for working Fortran compiler: /usr/bin/gfortran  -- works
  -- Detecting Fortran compiler ABI info
  -- Detecting Fortran compiler ABI info - done
  -- Checking whether /usr/bin/gfortran supports Fortran 90
  -- Checking whether /usr/bin/gfortran supports Fortran 90 -- yes
  -- Configuring done
  -- Generating done
  -- Build files have been written to: /home/sayop/data/Devel/GitHub.Clones/2DEuler/CODEdev/bin/build
  Scanning dependencies of target cfd.x
  [ 10%] Building Fortran object CMakeFiles/cfd.x.dir/main/Parameters.F90.o
  [ 20%] Building Fortran object CMakeFiles/cfd.x.dir/main/SimulationVars.F90.o
  [ 30%] Building Fortran object CMakeFiles/cfd.x.dir/main/GridJacobian.F90.o
  [ 40%] Building Fortran object CMakeFiles/cfd.x.dir/main/AUSMPWplus/AUSMPWplus.F90.o
  [ 50%] Building Fortran object CMakeFiles/cfd.x.dir/main/TimeIntegration.F90.o
  [  60%] Building Fortran object CMakeFiles/cfd.x.dir/io/io.F90.o
  [ 70%] Building Fortran object CMakeFiles/cfd.x.dir/main/SimulationSetup.F90.o
  [ 80%] Building Fortran object CMakeFiles/cfd.x.dir/main/MainLoop.F90.o
  [ 90%] Building Fortran object CMakeFiles/cfd.x.dir/io/ReadGrid.F90.o
  [100%] Building Fortran object CMakeFiles/cfd.x.dir/main/main.F90.o
  Linking Fortran executable cfd.x
  [100%] Built target cfd.x

If you run this, you will get executable named **cfd.x** and **input.dat** files. The input file is made by default. You can quickly change the required input options.


Input file setup
----------------

The 2DEuler code allows user to set multiple options to solve the unsteady 2-dimensional Euler problem by reading **input.dat** file at the beginning of the computation. Followings are default setup values you can find in the input file when you run **setup.sh** script::

  #Input file for tecplot print
  2-D Euler solver
  imax            71
  jmax            48
  ngl             3
  gridFile        grid.fine
  #Initial conditions
  density         4.0
  u               1.0
  v               0.0
  pressure        0.7142857
  gamma           1.4
  #Simulation parameters
  nmax            20000
  CFL             0.8
  errorLimit      1e-03
  #AUSMPW+ parameters
  alpha           0.0
  epsil           0
  limiter         0
  kappa           0.0

* **First line** ('2-D Euler solver' by default): Project Name

* **imax**: number of grid points in i-direction

* **jmax**: number of grid points in j-direction

* **ngl**: number of ghost layers (not available in this project)

* **gridFile**: grid file name to be read

* **density**: incoming flow density

* **u**: incoming flow velocity in x-direction

* **v**: incoming flow velocity in y-direction

* **pressure**: incoming flow pressure

* **gamma**: incoming flow heat specific ratio

* **nmax**: maximum number of iteration to be allowed and terminate case running

* **CFL**: CFL number

* **errorLimit**: normalized RMS error limit for convergence

* **alpha**: coefficient in AUSMPW+ (not used in this project)

* **epsil**: switch of second order accurate MUSCL differencing

* **limiter**: switch of MUSCL minmod limiter

* **kappa**: control parameter for 1st/2nd order accurate upwind differencing of MUSCL
