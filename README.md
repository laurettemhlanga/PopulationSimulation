# Population Simulation

Estimating incidence is of critical importance in epidemiology. In the application to chronic conditions, such as HIV, where there is no simple relationship between incidence and prevalence, the methods are not mature. Of interest, various HIV incidence estimators are in existence, and there is not much consistent evaluation of limitations and optimization of ‘population representative’ surveys. Considering the nature of national cross sectional surveys conducted by various large organisations, we create a simulation platform that mimics the sampling and analysis processes. The motivation of the platform is to serve as a testing, validation, and benchmarking tool for HIV incidence estimators (and other chronic conditions). We present the platform as a budding R package, which simulates an age and time structured population for an HIV epidemic.It is best described by Partial differential equations (PDEs) derived from a susceptible - infected (SI) model. The PDEs are solved using a discretised version of the method of lines, in which both the age variable time share a discretisation step size. Given sufficiently specific (birth, infection and mortality) rates, which are allowed to be functions of time, age, and time since infection, the platform constructs a fully specified age and time structured population.

The platform requires the user to specify the following rate functions to calculate; 

 * birth rate , 
 * pmtct rate,
 * incidence rate, 
 * base mortality rate, 
 * time step (delta), and 
 * maximum age. 
 

The idea is to use a wrapper function, birth_corhot simulation (calculates the state of the cohort one at a time) or do_one_simulation (calculates the state of a vector of cohorts but cannot exceed certain creation of vectors greater that 2^31 - 1 due to R storage) , to call the sub-functions within the platform. Additionally, the platform allow room for the users to start the simulation at any point. Therefore, by parsing the input parameters and initial conditions the platform approximates the survival probabilities of surviving a given time step, and then the cumulative survival probabilities, and finally the population status (susceptible and infected) of each state at future ages and times. The output of do_one_simulation is an age and time structured population for specified times, ages, and time since infection. Whereas birth_cohort_simulation approximates the 

Tho output from the do_one_simulation serves as input to the function extract_population_status, which parses the input to the calculated_normilised_prevalence by age. This approach is the first step to conducting cross sectional surveys at a specified date. Modifications maybe implemented by introducing the partion_prevalence function, which sub divides the prevalence into regional/cluster level prevalence, assuming that the former (from do_one_simulation) is the weighted average of the regions/clusters. Partitioning of the prevalence is such that the weighted average of the cluster/regional prevalences is equal to the input prevalence. 
