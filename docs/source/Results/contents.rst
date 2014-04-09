Results and discussions
=======================

Computational Grid
------------------

The grid used in this project has a resolution of 71 X 48 in i- and j-directions as shown below. In this project, unsteady 2-dimensional Euler solution is being resolved by performing the explicit time-integration with AUSMPW+ flux-splitting scheme.

.. image:: ./images/grid.png
   :width: 60%


CASE 1
------

- 1st order accuracy (CFL = 0.8)



CASE 2
------

- 2nd order accuracy MUSCL without flux limiter (CFL = 0.7)




CASE 3
------

- 2nd order accuracy MUSCL with minmod limiter (CFL = 0.7)




Comparative Analysis
--------------------

In this section, three different cases introduced above are compared in terms of convergency and calculated pressure along the bottom wall. The figure shown below illustrates the time history of RMS errors for different cases. The quantitative comparison is made in the table in terms of computational time and required iteration number for convergence. For those cases, the CASE #3 results in the heaviest computational cost. This is because 2nd order accurate needs one more neighboring points to extrapolate every interior points and moreover 'minmod' calculation should be added. The CASE #2 shows the irregular pattern of RMS error around :math:`10^{2}` iterations level. However, this is confirmed to disappear if the lower CFL number is employed for this case.

The table shown below also tells about applicable maximum CFL number limit for each cases. These numbers were achieved by experimenting the various number of CFL cases.

- Convergence check with RMS limit



Effect of CFL (for CASE 3)
--------------------------

Following figures show the effect of employed CFL number in CASE #3. Since the time-step for every iteration is determined on the basis of CFL number, grid size, local contravariant velocities and speed of sound, the effect of CFL number on computational time and convergence history is quite noticible. All these test cases were converged within a same level of RMS limit, which is earlier defined.

As noticed from below, the smaller CFL number is, the more far solution is obtained away from the exact solution. The required iteration number gets bigger as CFL number decreases. This is simply because less CFL number reduces the time step and it then results in less change in state vector in every time step. Thus, smaller CFL number case may not be able to show the fully developed steady flow. This is the main reason why the case of CFL = 0.01 shows the far pressure away especially beyond the half of air foil. This can be clearly observed by looking at two snapshots obtained at the same RMS limit (but at different iteration number).

