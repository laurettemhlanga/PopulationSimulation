# PopulationSimulation

The functions are still under development. These functions work together to simulate an age and time structured pupolation. The major function requires user specified functions which are; 
 * birth rate , 
 * incidence, 
 * base mortality, 
 * excess mortality, 
 * time step (delta), and 
 * aging step (the age to which each birth cohort is aged to). 

The output is an age and time structured population for specified times and ages. Subdivided into susceptible and infected (specified by time since infection) groups and from it the prevalence can be calculated. 

The inherent assumption is that incidence and mortality are functions of age and times, birth rate is a function of time. The function do_one_simulation calls all the other functions to yield the output of interest - infected and susceptible populations. The aggregate population at every time step is approximated using the method of lines.

The time step determines how finely granular the aggregate population at any given time and age is calculate. 


