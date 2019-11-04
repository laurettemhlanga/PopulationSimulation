#' incidence_vector
#'
#' a function that returns a matrix of probabilities for each age and time step of the simulation
#'
#' @param n_age_steps The maximuma age attained by each birth cohort (aging steps).
#' @param date_of_birth a numeric vectors indicating the dates in simulation. Note that date format is not used.
#' @param incidence a function which takes as arguments age and time and returns a numberic rate of incidence for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_incidence_matrix function.
#' @param time_step the time step between consecurtive birth dates in the vector list_of_birth_times
#' @return a matrix of column length of date birthas,
#'  Values stored in the matrix are numeric double, from 0-1, which represent the incidence rate at given age and time
#'
#'
#' @export




incidence_vector <- function(n_age_steps, date_of_birth,
                             incidence, time_step)
{
  # populates an incidence matrix function based on the incidence function supplied
  # the maximum age, list of times and the required time-step. Note the approximation
  # of being infected in the interval in question is calculated at mid point.

  ages  <- seq(from = time_step/2, by = time_step, length.out = n_age_steps)
  times <- seq(from = (date_of_birth + time_step/2), by = time_step, length.out = n_age_steps)


  incidence_vector =  incidence(ages = ages, times = times)


  return(incidence_vector)

}


