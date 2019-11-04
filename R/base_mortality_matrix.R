
#' base_mortality_vector
#'
#' a function that returns a matrix of probabilities of mortality for each ages and time step of the simulation
#'
#' @param n_age_steps The maximum age attained by each birth cohort (aging steps).
#' @param date_of_birth a numeric vectors indicating the dates in simulation. Note that date format is not used.
#' @param base_mortality_function a function which takes as arguments age and time and returns a numberic rate of mortality for each age and time included in the simulation.
#' This function can be defined by user or can be selected from several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the base_mortality_matrix function
#' @param time_step the time step between consecurtive birth dates in the vector list_of_birth_times
#' @return a matrix of column length max_age and row length list_of_birth_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the natural mortality rate at given age and time
#'
#'
#' @export
#'



base_mortality_vector <- function(n_age_steps, date_of_birth,
                                  base_mortality_function, time_step)
{
  # populates an incidence matrix function based on the incidence function supplied
  # the maximum age, list of times and the required time-step. Note the approximation
  # of being infected in the interval in question is calculated at mid point.

  ages  <- seq(from = time_step/2, by = time_step, length.out = n_age_steps)
  times <- seq(from = (date_of_birth + time_step/2), by = time_step, length.out = n_age_steps)


  mortality_vector =  base_mortality_function(ages = ages, times = times)

  return(mortality_vector)

}




#' base_mortality_matrix
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

  ages <- seq(time_step, max_age, time_step)

  mortality_matrix <-  matrix(NA, nrow = length(list_of_birth_times) , ncol =  length(ages))
  age_index <- 1:length(ages)
  birth_time_index <- 1:(length(list_of_birth_times))

  for (index in age_index){

    mortality_matrix[birth_time_index, index] =   base_mortality( matrix_of_ages = ages[index] + 0.5 * time_step ,
                                                                    matrix_of_times = list_of_birth_times + (ages[index]+ 0.5 * time_step))


  }

  return(mortality_matrix)
  #return((compress_age_time_matrix(mortality_matrix)))
}



