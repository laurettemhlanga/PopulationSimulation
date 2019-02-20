#' infected_mortality_array_fun
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param age_steps denotes the number of steps forward each age group will be aged in the simulation by the do_sim function
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
#'
#'
#'

infected_mortality_array_fun <- function(age_steps,
                                     birth_dates,
                                     delta = 1,
                                     excess_mortality,
                                     base_mortality)
{


  times  <- 0:(max(birth_dates) - min(birth_dates))
  ages <- 0:age_steps
  infected_mortality_array <-  array(NA, dim = c((length(times) + length(ages)-1), length(ages), length(ages[-1])))
  #infected_mortality_array[1, , ] <- rep(1, ((nrow(infected_mortality_array) - ncol(infected_mortality_array)) + 1))

  for (aa in ages){
    for (ta in ages[-1]){

      infected_mortality_array[times + aa + 1, aa + 1, ta ] =
        exp(-(base_mortality(times + (aa + (0.5 * delta)), (aa + (0.5 * delta))) *
                excess_mortality(times + (aa + (0.5 * delta)), (aa + (0.5 * delta)), ta + (0.5 * delta))) * delta)
        # R counts from 1 and hence 1 is added to account for the age 0
    }
  }

  return(infected_mortality_array)

}

















































