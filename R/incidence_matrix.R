#' incidence_matrix
#'
#' a function that returns a matrix of probabilities for each age and time step of the simulation
#'
#' @param max_age the number of steps to age the population
#' @param list_of_times a numeric vectors of length min:max; indicates the range of age to be included in simulation. Note that date format is not used.
#' @param incidence a function which takes as arguments age and time and returns a numberic rate of incidence for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_incidence_matrix function.
#' @param time_step the time step between consecurtivelist_of_times
#' @return a matrix of column length max_age and row lengthlist_of_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export

incidence_matrix <- function(max_age,
                             list_of_times,
                             incidence,
                             time_step)
{
  # populates an incidence matrix function based on the incidence function supplied
  # the maximum age, list of times and the required time-step. Note the approximation
  # of being infected in the interval in question is calculated at mid point.

  times  <-  list_of_times
  age  <-    seq(0, max_age, time_step)
  incidence_matrix <-  matrix(NA, nrow = length(times) + (length(age)-1), ncol =  length(age))

  counter <- 1

  age_index <- 1:length(age)
  time_index <- 0:(length(times)-1)

  for (aa in age_index){


    incidence_matrix[time_index + aa , aa ] =  incidence((age[counter] + 0.5 * time_step),
                                                         times + (age[counter]+ 0.5 * time_step))

    counter <- counter + 1

  }

  return(transform_data(incidence_matrix))
}




