Project description
===================

Given task
----------

In this project, 2-D Euler solver which utilizes the AUSMPW+ scheme (Advection Upstream Splitting Method - Pressure Weighted) has been developed. The developed 2-D Euler code was verified for a test case of a flowfield over the top half of a 10 deg. diamond airfoil.

Governing Equations
-------------------

The 2-D, unsteady Euler Equations will be solved. The equations will be marched forward in time until a steady state solution is achieved. The transformed 2-D Euler equations can be written:

.. math::
   \frac{\partial \left ( \bar{U} / J \right )}{\partial t} + \frac{\partial \vec{F'}}{\partial \xi} + \frac{\partial \vec{G'}}{\partial \eta} = 0

where the transformed state and inviscid flux vectors are

.. math::
   \vec{U} = \begin{bmatrix} \rho\\ \rho u\\ \rho v\\ E_{t} \end{bmatrix}

and

.. math::
   \vec{F'} = \frac{1}{J} \left ( \xi_{x} \vec{F}_{I} + \xi_{y} \vec{G}_{I} \right ),\;\;\;\; \vec{G'} = \frac{1}{J} \left ( \eta_{x} \vec{F}_{I} + \eta_{y} \vec{G}_{I} \right )

Here the total energy per unit volume is defined by:

.. math::
   E_{t} = \frac{p}{\gamma - 1} + \frac{\rho}{2} \left ( u^{2} + v^{2} \right )

and the stagnation enthalpy per unit mass is:

.. math::
   h_{0} = \frac{p \gamma}{\rho (\gamma - 1)} + \frac{1}{2} \left ( u^{2} + v^{2} \right )

The transformed velocity component (Contravariant velocities) in the generalized coordinates is determined as a function of :math:`u` and :math:`v` with the grid metrics as follows:

.. math::
   \tilde{U} = \xi_{x}u + \xi_{y}v

   \tilde{V} = \eta_{x}u + \eta_{y}v


Computational Domain
--------------------

This project analyze the top half of a 10 deg. diamond airfoil so the location of point E is (x,y) = (0.5, 0.0882). Each grid point can be describd by (x,y) location or (:math:`i`, :math:`j`) location where the :math:`i` index is in the :math:`\xi` direction and the :math:`j` index is in the :math:`\eta` direction. The grid will consist of 71 points in the ":math:`i`" direction and 48 points in the ":math:`j`" direction. The inverse grid metrics must be evaluated at every grid point in the computational domain (including the boundaries). Use 2nd order accurate, central differences for interior points and 2nd order accurate, one-sided differences for boundary points. After the inverse metrics are computed, the grid Jacobian and grid metrics must be computed and stored at every location (including the boundaries)

.. image:: ./images/domain.png
   :width: 70% 
