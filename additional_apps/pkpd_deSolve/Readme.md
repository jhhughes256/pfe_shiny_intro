This Shiny application was developed for the hands-on workshop at PAGANZ 2017.
It is an example of how to set up an application that simulates from a PopPK 
model. The model is located in `model.R` and is written using the `deSolve` 
package. 

This application demonstrates the importance of using `reactive` expressions to
avoid recomputing expensive code. Note that when input functions that affect
the plot are changed, that the simulation code does not run (model is not 
resimulated).

The simulation is run using the covariate and simulation information from the
panel to the left and the dosing information below the figure.

**THIS IS AN EXAMPLE APPLICATION AND IS FOR DEMONSTRATION PURPOSES ONLY.**