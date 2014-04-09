Numerical Method
================

During each time-integration step, the code calculate the fluxes :math:`\vec{F'}_{i+1/2,j}` at every ":math:`i`" half-point locations, and :math:`\vec{G'}_{i,j+1/2}` at every ":math:`j`" half-point locations. In order to obtain the flux terms properly treated with consideration of characteristics of wave propagation, MUSCL differencing should first be used to extrapolate the state vectors to every half point locations. After then AUSMPW+ scheme applies to those points for solving the flux terms.

MUSCL with limiter
------------------

The extrapolated state vector can be written:

.. math::
   \vec{U}^{L}_{i+\frac{1}{2}} = \vec{U}_{i} + \frac{\epsilon }{4} \Delta \vec{U}_{i-\frac{1}{2}} \left \{ (1+\kappa)\left[\varphi \left(\text{r}^{L}\right)\right] + (1+\kappa)\text{r}^{L}\left[\varphi\left ( \frac{1}{\text{r}^{L}} \right ) \right ] \right \}

where

.. math::
   \text{r}^{L} = \frac{\Delta \vec{U}_{i+\frac{1}{2}}}{\Delta \vec{U}_{i-\frac{1}{2}}} = \frac{\vec{U}_{i+1} - \vec{U}_{i}}{\vec{U}_{i} - \vec{U}_{i-1}}


.. math::
   \vec{U}^{R}_{i+\frac{1}{2}} = \vec{U}_{i+1} - \frac{\epsilon}{4} \Delta \vec{U}_{i+\frac{3}{2}} \left \{ \left ( 1+\kappa \right ) \left [ \varphi\left ( \frac{1}{\text{r}^{R}} \right ) \right ] + \left ( 1+\kappa \right ) \text{r}^{L} \left [ \varphi \left ( \text{r}^{R} \right ) \right ] \right \}

where

.. math::
   \text{r}^{R} = \frac{\Delta \vec{U}_{i+\frac{1}{2}}}{\Delta \vec{U}_{i+\frac{2}{3}}} = \frac{\vec{U}_{i+1} - \vec{U}_{i}}{\vec{U}_{i+2} - \vec{U}_{i+1}}

AUSMPW+ scheme: Flux Splitting with pressure weight
---------------------------------------------------

General time-integration in CFD solver should follow the algebraic equation of state- and flux-vectors as shown below:

.. math::
   \vec{U}^{n+1}_{i,j} = \vec{U}^{n}_{i,j} - \Delta t^{n}_{i,j} J_{i,j} \left [ \left ( \vec{F'}_{i+\frac{1}{2},j} - \vec{F'}_{i-\frac{1}{2},j} \right ) + \left ( \vec{G'}_{i,j+\frac{1}{2}} - \vec{G'}_{i,j-\frac{1}{2}} \right ) \right ]^{n}

where the time-step restriction at every grid cell is

.. math::
   \Delta t^{n}_{i,j} = \left \{ \frac{CFL}{|\tilde{U}| + |\tilde{V}| + c \sqrt{ \xi^{2}_{x} + \xi^{2}_{y} + \eta^{2}_{x} + \eta^{2}_{y} + 2 |\xi_{x} \eta_{x} + \xi_{y} \eta_{y} } | } \right \}^{n}_{i,j}

In the above equations, AUSMPW+ scheme applies to calculate the transformed flux vectors at every half points. In this scheme, the values of Mach numbers and pressures are calculated at the cell interfaces and split into the left- and right-extrapolated components. The scheme employed flux vectors can be determined as:

.. math::
   \vec{F'} = \frac{\widetilde{\overline{M}}^{+}_{L} C_{avg} A_{1}}{J} \begin{bmatrix} \rho \\ \rho u \\ \rho v \\ \left ( E_{t} + p \right ) \end{bmatrix}_{L} + \frac{\widetilde{\overline{M}}^{-}_{R} C_{avg} A_{1}}{J} \begin{bmatrix} \rho \\ \rho u \\ \rho v \\ \left ( E_{t} + p \right ) \end{bmatrix}_{R} + \frac{P^{+}}{J} \begin{bmatrix} 0 \\ \xi_{x}p\\ \xi_{y}p\\ 0 \end{bmatrix}_{L} + \frac{P^{-}}{J} \begin{bmatrix} 0 \\ \xi_{x}p\\ \xi_{y}p\\ 0 \end{bmatrix}_{R}

where

.. math::
   A_{1} = \sqrt{\xi_{x}^{2} + \xi_{y}^{2}}

.. math::
   \vec{G'} = \frac{\widetilde{\overline{M}}^{+}_{L} C_{avg} A_{1}}{J} \begin{bmatrix} \rho \\ \rho u \\ \rho v \\ \left ( E_{t} + p \right ) \end{bmatrix}_{L} + \frac{\widetilde{\overline{M}}^{-}_{R} C_{avg} A_{1}}{J} \begin{bmatrix} \rho \\ \rho u \\ \rho v \\ \left ( E_{t} + p \right ) \end{bmatrix}_{R} + \frac{P^{+}}{J} \begin{bmatrix} 0 \\ \eta_{x}p\\ \eta_{y}p\\ 0 \end{bmatrix}_{L} + \frac{P^{-}}{J} \begin{bmatrix} 0 \\ \eta_{x}p\\ \eta_{y}p\\ 0 \end{bmatrix}_{R}

.. math::
   A_{1} = \sqrt{\eta_{x}^{2} + \eta_{y}^{2}}

One thing to keep carefully in mind is that grid metrics quantities should not be left- and right- extrapolated because these quantities are transportable flow quantities but static grid-point related quantities.


Initial and Boundary Conditions
-------------------------------

At the beginning of simulation, the 2DEuler code sets the initial condition. After then the code set the boundary conditions at every time step. The initial conditions at all grid points is set on the basis of following pre-specified flow quantities:

.. math::
   \rho = 4,\; \;\; \; u = 1.0,\; \;\; \;v = 1.0, \; \;\; \; \gamma = 1.4,\; \;\; \; p = \frac{1}{\gamma}


The incoming flow and outflow are supersonic. Thus following pre-specified boundary conditions should be employed by


Inflow: :math:`\vec{U}_{1,j}^{n}` (determined from the initial condition parameters)

Outflow: :math:`\vec{U}_{imax,j}^{n} = \vec{U}_{imax-1,j}^{n}` (1st order extrapolation)

Solid boundary: No velocity in the :math:`\eta` direction. The 2DEuler code uses a 2nd order extrapolation that is described in the project assignment.


Convergence Limit
-----------------

In order to terminate the code running when the proper steady-state assumption can be made, the 2DEuler code calculates the RMS error at every time step as defined below:

.. math::
   \text{RMS}^{n} = \sqrt{\frac{1}{N}\sum_{m=1}^{4} \sum_{i=1}^{imax} \sum_{j=1}^{jmax} \left [ \left ( \vec{U}_{i,j}^{n+1} - \vec{U}_{i,j}^{n} \right )^{2} \right ]}

The computation will stop when RMS error normalized by the RMS error at first iteration meets the following criteria:

.. math::
   \frac{\text{RMS}^{n}}{\text{RMS}^{n=1}} \leq 1\text{x}10^{-3}
