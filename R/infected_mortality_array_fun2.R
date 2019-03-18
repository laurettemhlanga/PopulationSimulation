
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


infected_mortality_array_fun <- function(max_age,
                                         birth_dates,
                                         delta,
                                         excess_mortality,
                                         base_mortality)
{
  #calculates the probabolity of surviving in the infected state after 

times  <- seq(min(birth_dates), max(birth_dates), delta)
ages <- seq(0, max_age, delta)
times_since_i <- seq(0, max_age, delta)

infected_mortality_array <-  array(NA, dim = c(length(times) + max_age, length(ages), length(ages)))


  for (aa in seq_along(ages)){
    for (ta in seq_along(times_since_i)){
      
      if (ages[aa] < times_since_i[ta]){
  
        infected_mortality_array[times + (aa - 1), aa, ta] <- 0      
        
      }else if (times_since_i[ta] == 0){
        
        infected_mortality_array[times + (aa - 1), aa, ta] <-  1
        
      }else {
    
      
       infected_mortality_array[times + (aa - 1), aa, ta] <-  exp(-(base_mortality((times + (ages[aa] + (0.5 * delta))), ages[aa] + ((0.5 * delta))) + 
                                                                                    excess_mortality((times + (ages[aa] + (0.5 * delta)))- times_since_i[ta], 
                                                                                                   (ages[aa] + (0.5 * delta)) - times_since_i[ta], times_since_i[ta])))
      
         }
      
       }
  }

return(infected_mortality_array)

}
  
  
infected_mortality_array_fun(max_age = 3,
                             birth_dates = 1:5,
                             delta = 1,
                             excess_mortality = excess_mortality_fun,
                             base_mortality = time_indep_age_linear_base_mortality)
