


#' cumulative_probability_surviving_infected
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param probability_surviving_infected_array  3 dimensional probability matrix
#' @return returns an array of dimensions time, age and time since infection - tau
#'
#' @export
#'
#'
#


cumulative_probability_surviving_infected <- function(probability_surviving_infected_array)
{
  # calculates the cumulative probability of surviving in the infected state after infection at time
  # t - tau, age - tau  for a duration of tau years. Time is constant as the matrix is being populated horizontally
  #

  dimcpsi <- c(dim(probability_surviving_infected_array)[1],
               dim(probability_surviving_infected_array)[2],
               dim(probability_surviving_infected_array)[3] + 1)

  cumulative_probability_surviving_array <-  array(NA, dim = dimcpsi)

  length_of_1vector <- dim(probability_surviving_infected_array)[1] * dim(probability_surviving_infected_array)[2]

  #first_column <- rep(NA, length(dim(probability_surviving_infected_array)[1]))

  cumulative_probability_surviving_array[, , 1] = matrix(c(rep(NA, (dim(probability_surviving_infected_array)[1])),
                                                           rep(1, (length_of_1vector - (dim(probability_surviving_infected_array)[1])))))


  #for(time in 1:dim(cumulative_probability_surviving_array)[1] ){
    for (age in 2:dim(cumulative_probability_surviving_array)[2] ){
      for (time_since_infection in 2:dim(cumulative_probability_surviving_array)[3]){


        cumulative_probability_surviving_array[ , age, time_since_infection] <- cumulative_probability_surviving_array[ , age -1, time_since_infection - 1] *
          probability_surviving_infected_array[ , age, time_since_infection - 1]


      }
    }
 # }
  return(cumulative_probability_surviving_array)
}




#cum_prob_survival_i <- cumulative_probability_surviving_infected(probability_surviving_infected_array = y)





