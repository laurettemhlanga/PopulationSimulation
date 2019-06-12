#' probability_of_surviving_infected
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param wedge_of_excess_mortality_array array containing excess mortality rates for a given time space.
#' @param matrix_of_base_mortality matrix containing background rates for a given time space.
#' @param time_step the time or age difference between to consecutive times or ages i.e it is uniform in all values supplied
#'
#' @return returns an array of dimensions time, age and time since infection - tau which is the probability of surviving the given time_step given that you are infected.
#'
#'
#' @export


probability_of_surviving_infected <- function(wedge_of_excess_mortality_array,
                                              matrix_of_base_mortality,
                                              time_step)
{
  # calculates the probability of surviving in the infected state one more time slice/step at time
  # 'time' , age  'age' for a duration of tau years. Based on the excess and base mortality
  # functions an array of the respective survival probabilities
  # for a given age, time and  time since infection is created.

  probability_surviving_infected_1 <- array(NA, dim = dim(wedge_of_excess_mortality_array))

  for (time_si in 1:dim(wedge_of_excess_mortality_array)[3]){

    probability_surviving_infected_1[, , time_si] <- exp(-(matrix_of_base_mortality + wedge_of_excess_mortality_array[, , time_si]) * time_step)

  }

  return( probability_surviving_infected_1)
}



# probability_of_surviving_infected(wedge_of_excess_mortality_array = y,
#                                matrix_of_base_mortality = m,
#                                time_step = 1)




