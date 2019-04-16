
#'base_mortality_matrix_fun
#'
#' a function that returns a matrix of probabilities of mortality for each ages and time step of the simulation
#'
#' @param max_age the number of steps to ages the population
#' @param list_of_birth_times a numeric vectors of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param base_mortality a function which takes as arguments ages and time and returns a numberic rate of mortality for each ages and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function
#' @param time_step the time step between consecurtivelist_of_birth_times
#' @return a matrix of column length max_age and row lengthlist_of_birth_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at ages and time
#'
#'
#' @export
#'



base_mortality_matrix <- function(max_age,
                                  list_of_birth_times,
                                  base_mortality,
                                  time_step)
{

  # populates an base mortality matrix function based on the base mortality function supplied
  # the maximum ages, list of times and the required time-step. Note the approximation
  # of being infected in the interval in question is calculated at mid point.

  birth_times <- list_of_birth_times
  ages <- seq(0, max_age, time_step)
  mortality_matrix <-  matrix(NA, nrow = length(birth_times) + (length(ages) -1), ncol =  length(ages))


  #indexing the birth_times and ages vector

  age_index <- 1:length(ages)
  birth_time_index <- 1:(length(birth_times))

  for (aa in age_index){


    mortality_matrix[birth_time_index + (aa -1), aa ] =   base_mortality( matrix_of_ages = ages[aa] + 0.5 * time_step ,
                                                                    matrix_of_times = birth_times + (ages[aa]+ 0.5 * time_step))


  }

  return(compress_age_time_matrix(mortality_matrix))
}




# base_mortality_matrix_fun <- function(max_age,
#                                            list_of_birth_times,
#                                            generate_base_mortality_fun, time_step)
# {
#
#   mortality_matrix  = matrix(NA, nrow = length(list_of_birth_times) + max_age, ncol =  length(0:max_age))
#   times  = 0:(max(list_of_birth_times) - min(list_of_birth_times))
#   for (aa in (0:max_age)){
#
#     mortality_matrix[times + (aa + 1), (aa + 1)] =  generate_base_mortality_fun(times + (aa + 0.5 * time_step), (aa + 0.5 * time_step))
#   }
#   return(mortality_matrix)
# }






