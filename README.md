# Population Simulation

The code create a simulation platform the simulates an age and time structured pupolation for an HIV epidemic, based on the SI compartmental model, best described by Partial differential equations. The motivation of the platform is to serve as a testing and validation tool for HIV incidence estimators.  

Of interest, various HIV incidence estimators are in existance, there is need to test and validated the methods on a uniform platform for comparison. The Pdes are solved using the method of lines, by discretising the age variable and  keeping the time variable continuous, the aggregate population at a specified  age and time can be calculated. Considering the nature of national cross sectional surveys conducted by various large organisations, we mimic the status quo, and proceed to conduct cross sectional surveys. The platform requires the user to specify the following functions; 

 * birth rate , 
 * pmtct rate,
 * incidence rate, 
 * base mortality rate, 
 * excess mortality rate, 
 * time step (delta), and 
 * aging step. 
 
The rates are either functions of time, age, time since infection or all. For example, the birth and pmtct rate are functions of time, whereras excess mortality is a function of all. The time step variable is defined as the difference between two consecutive times and age and determines how  granular the population aggregates are i.e. weekly, fortnights, monthly, etcetera.

The idea is to use a wrapper function, do_one_simulation, to call the sub-functions within the platform. In addition, the platform allow room for the users to start the simulation at any point. Therefore, by parsing the input parameters and initial conditions the platform approximate the cumulative survival probabilities, and consequently population status (susceptible and infected) of each state at future ages and times. The output of do_one_simulation is an age and time structured population for specified times, ages, and time since infection. 

Tho output from the do_one_simulation serves as input to the function extract_population_status, which parses the input to the calculated_normilised_prevalence by age. This approach is the first step to conducting cross sectional surveys at a specified date. Modifications maybe implemented by introducing the partion_prevalence function, which sub divides the prevalence into regional/cluster level prevalence, assuming that the former (from do_one_simulation) is the weighted average of the regions/clusters. Partitioning of the prevalence is such that the weighted average of the cluster/regional prevalences is equal to the input prevalence. 
