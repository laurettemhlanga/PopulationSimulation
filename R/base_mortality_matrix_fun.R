
#'base_mortality_matrix_fun
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param max_age a number. Indicates the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param list_of_times a numeric vector of length min:max; indicates the range ofage to be included in simulation. Note that date format is not used.
#' @param generate_base_mortality_fun a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function
#' @param time_step the time step between consecurtive list_of_times
#' @return a matrix of column length max_age and row length list_of_times.
#' Values stored in the matrix are numeric-double, from 0-1, which represent the probability of dying at each age and time
#'
#'
#' @export



base_mortality_matrix <- function(max_age,
                                  list_of_times,
                                  base_mortality,
                                  time_step)
{

  times <- list_of_times
  age <- seq(0, max_age, time_step)
  mortality_matrix <-  matrix(NA, nrow = length(times) + (length(age) -1), ncol =  length(age))

  counter <- 1

  #indexing the times andage vector

  age_index <- 1:length(age)
  time_index <- 0:(length(times)-1)

  for (aa in age_index){

    mortality_matrix[time_index + aa, aa ] = base_mortality((age[counter] + 0.5 * time_step),
                                                                         times + (age[counter]+ 0.5 * time_step))

    counter <- counter + 1

  }

  return(transform_data(mortality_matrix))
}




# base_mortality_matrix_fun <- function(max_age,
#                                            list_of_times,
#                                            generate_base_mortality_fun, time_step)
# {
#
#   mortality_matrix  = matrix(NA, nrow = length(list_of_times) + max_age, ncol =  length(0:max_age))
#   times  = 0:(max(list_of_times) - min(list_of_times))
#   for (aa in (0:max_age)){
#
#     mortality_matrix[times + (aa + 1), (aa + 1)] =  generate_base_mortality_fun(times + (aa + 0.5 * time_step), (aa + 0.5 * time_step))
#   }
#   return(mortality_matrix)
# }






