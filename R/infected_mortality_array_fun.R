#' infected_mortality_array_fun
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param max_age denotes the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param birth_dates a numeric vector of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param delta the time or age difference between to consecutive times or ages i.e it is uniform in all values supplied
#' @param base_mortality a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @param excess_mortality a function which takes as arguments age, time and tau - i.e. the time since infection among the infected population - and returns a numberic rate of mortality for each age and time included in the simulation
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @return returns an array of dimensions time, age and time since infection - tau
#'
#' @export
#'
#'
#'



infected_mortality_array_fun <- function(max_age = 3,
                                     birth_dates = 1:5,
                                     delta = 1,
                                     excess_mortality = excess_mortality_fun,
                                     base_mortality = time_indep_age_linear_base_mortality)
{

  # calculates the probability of surviving  for time since infection, having been infected at age - time since infection,
  # time  - time since infection.  This is based on the excess and base mortality supplied.


  times  <- seq(min(birth_dates), max(birth_dates), delta)
  ages <- seq(0, max_age, delta)
  times_since_i <- seq(1, max_age, delta)

  infected_mortality_array <-  array(NA, dim = c((length(times) + (length(ages) - 1)), length(ages), length(ages)))
 
  

  age_index <- 0:length(ages)
  time_index <- 0:(length(times)-1)
  times_since_index <- 2:(length(ages))
  
  

  counter <- 1

  for (aa in age_index){
    for (ta in times_since_index){
#aa= 1; ta = 7
      infected_mortality_array[time_index + aa, aa, ta - 1] =
        exp(-(base_mortality(times + (ages[counter]+ 0.5 * delta), (ages[counter] + 0.5 * delta)) +
                excess_mortality(times + (ages[counter]+ 0.5 * delta), (ages[counter] + 0.5 * delta),
                                 (times_since_i[counter] + 0.5 * delta))) * delta)
     
      #infected_mortality_array[time_index + aa, aa, ta - 1] <- transform_data(infected_mortality_array)
    }
    counter <- counter + 1
   
  }

  # infected_mortality_array_2 <- array(NA, dim = c(length(times), length(ages), length(ages[-1])))
  #
  # for ( ts in (1:dim(infected_mortality_array)[3])){
  #
  #   infected_mortality_array
  #
  # }

  return(infected_mortality_array)

}

















































