
#'base_mortality_matrix_fun
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param age_steps a number. Indicates the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param birth_dates a numeric vector of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param generate_base_mortality_fun a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function
#' @param delta the time step between consecurtive birth_dates
#' @return a matrix of column length age_steps and row length birth_dates.
#' Values stored in the matrix are numeric-double, from 0-1, which represent the probability of dying at each age and time
#'
#'
#' @export



base_mortality_matrix_fun <- function(age_steps,
                                           birth_dates,
                                           generate_base_mortality_fun, delta)
{

  mortality_matrix  = matrix(NA, nrow = length(birth_dates) + age_steps, ncol =  length(0:age_steps))
  times  = 0:(max(birth_dates) - min(birth_dates))
  for (aa in (0:age_steps)){

    mortality_matrix[times + (aa + 1), (aa + 1)] =  generate_base_mortality_fun(times + (aa + 0.5 * delta), (aa + 0.5 * delta))
  }
  return(mortality_matrix)
}
