# PopulationSimulation


Population Simulation Platform
The functions are still under development. These functions serve a purpose of simulating an age and time structured pupolation. The major function requires user specified functions and other variables which are; 
-birth rate , 
-incidence, 
-base mortality, 
-excess mortality, 
-time step (delta), and 
-aging step (the age to which each birth cohort is aged to). 

The output is an age and time structured population for the specified times and ages. subdivided into susceptible and infected groups and from it the prevalence can be calculated. 

The inherent assumption is the incidence and mortality are functions of age and times, birth rate is a function is a function of time. do_one_simulation calls all the 
