
#' probability_of_surviving_infected
#'
#' a function that returns an matrix of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param excess_mortality_matrix matrix containing excess mortality rates for a given time space.
#' @param base_mortality_vector containing background rates for a given time space.
#' @param time_step the time or age difference between to consecutive times or ages i.e it is uniform in all values supplied
#'
#' @return returns an matrix of dimensions time, age and time since infection - tau which is the probability of surviving the given time_step given that you are infected.
#'
#'
#' @export



probability_surviving_infected_matrix <- function(excess_mortality_matrix,
                                                  time_step, base_mortality_vector
){

  #taus  <- seq(time_step, max_age, time_step)

  prob_survival <- matrix(NA, ncol = ncol(excess_mortality_matrix), nrow = nrow(excess_mortality_matrix))

  taus_indices <- 1:nrow(prob_survival)

  for (tau_index in seq_along(taus_indices)){

    prob_survival[tau_index, ] <- exp(-(excess_mortality_matrix[tau_index, ] + base_mortality_vector[-1]) * time_step)

  }

  return(prob_survival)
}





#' probability_of_surviving_infected_array
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


probability_surviving_infected_array <- function(wedge_of_excess_mortality_array,
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






# prob_surviv <- probability_surviving_infected(excess_mortality_matrix = excess_rate,
#                                time_step = 1, base_mortality_vector = base_mortality_vector)






# dim(probability_of_surviving_infected(wedge_of_excess_mortality_array = y,
#                                matrix_of_base_mortality = matrix(rep(0.1,20), 5, 4),
#                                time_step = 1))




