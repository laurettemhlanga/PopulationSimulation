
#'base_mortality_matrix_fun
#'
#' a function that returns a matrix of probabilities of mortality for each ages and time step of the simulation
#'
#' @param max_age The maximum age attained by each birth cohort (aging steps).
#' @param list_of_birth_times a numeric vectors indicating the birth dates in simulation. Note that date format is not used.
#' @param base_mortality a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the base_mortality_matrix function
#' @param time_step the time step between consecurtive birth dates in the vector list_of_birth_times
#' @return a matrix of column length max_age and row length list_of_birth_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the natural mortality rate at given age and time
#'
#'
#' @export
#'



base_mortality_matrix <- function(max_age,
                                  list_of_birth_times,
                                  base_mortality,
                                  time_step)
{

  # populates a base mortality matrix  based on the base mortality function supplied
  # the maximum ages, list of times and the required time-step. Note the approximation
  # of being infected in the interval in question is calculated at mid point.


  ages <- seq(0, max_age, time_step)
  mortality_matrix <-  matrix(NA, nrow = length(list_of_birth_times) + (length(ages) -1), ncol =  length(ages))


  #indexing the list_of_birth_times and ages vector

  age_index <- 1:length(ages)
  birth_time_index <- 1:(length(list_of_birth_times))

  for (index in age_index){

    mortality_matrix[birth_time_index + (index-1), index] =   base_mortality( matrix_of_ages = ages[index] + 0.5 * time_step ,
                                                                    matrix_of_times = list_of_birth_times + (ages[index]+ 0.5 * time_step))


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
#   for (indexin (0:max_age)){
#
#     mortality_matrix[times + (index+ 1), (index+ 1)] =  generate_base_mortality_fun(times + (index+ 0.5 * time_step), (index+ 0.5 * time_step))
#   }
#   return(mortality_matrix)
# }






