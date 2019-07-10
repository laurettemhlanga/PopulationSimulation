# PopulationSimulation

We create a simulation platform the simulates an age and time structured pupolation for an HIV epidemic, based on the SI model, best described by Partial differntial equations. The motivation of the platform is to serve as a testing and validation tool for HIV incidence estimators.  

Of interest various HIV incidence estimators are in existance,and there is need to test and validated the methods on a uniform platform for comparison and to ascertain each of their perfomances. The Pdes are solved using the method of lines, by discretising the age variable and  keeping the time variable continuous, the aggregate population at a specified is calculated. Considering the nature of national cross sectional surveys conducted by various large organisations, we mimic the status quo. The platform requires the user to specify the following functions; 

 * birth rate , 
 * pmtct rate,
 * incidence rate, 
 * base mortality rate, 
 * excess mortality rate, 
 * time step (delta), and 
 * aging step. 
 
The rates are either functions of time, age, time since infection or all. For example, the birth and pmtct rate are functions of time, the excess mortality is a function of all. The time step variable is defined as the difference between two consecutive times and age and determines how finely granular the population aggregates are. . 

The idea is to use  a wrapper function, do_one_simulation, to call the sub-functions within the platform. In addition, the platform allow room for the user to start the simulation at any point. Therefore, by parsing the input parameters and initial conditions we; approximate the cumulative survival probabilities of  each state, calculate the population status (susceptible and infected) at future ages and times. The output of do_one_simulation is an age and time structured population for specified times, ages, and time since infection. 

