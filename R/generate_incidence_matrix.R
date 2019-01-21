#' generate_incidence_matrix
#'
#' a function that returns a matrix of probabilities for each age and time step of the simulation
#'
#' @param age_steps numeric vector of length 1. Indicates the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param birth_dates a numeric vectors of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param generate_incidence_fun a function which takes as arguments age and time and returns a numberic rate of incidence for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_incidence_matrix function.
#' @return a matrix of column length age_steps and row length birth_dates,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#' @examples y <- generate_incidence_matrix (age_steps = 2, birth_dates = 10:15, generate_incidence)


generate_incidence_matrix <- function(age_steps,
                                      birth_dates,
                                      generate_incidence_fun)
{

  incidence_matrix  = matrix(NA, nrow = length(birth_dates) + age_steps, ncol =  length(1:age_steps))
  times  = 0:length(birth_dates)

  for (aa in 1:age_steps){

    incidence_matrix[times + aa, aa] =  generate_incidence_fun(times + aa, aa)

  }

  return(incidence_matrix)
}
