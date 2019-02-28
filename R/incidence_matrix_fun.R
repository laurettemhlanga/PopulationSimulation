#' incidence_matrix_fun
#'
#' a function that returns a matrix of probabilities for each age and time step of the simulation
#'
#' @param age_steps numeric vector of length 1. Indicates the number of steps forward each age group will be aged in the simulation by the do_sim function
#' @param birth_dates a numeric vectors of length min:max; indicates the range of ages to be included in simulation. Note that date format is not used.
#' @param generate_incidence_fun a function which takes as arguments age and time and returns a numberic rate of incidence for each age and time included in the simulation.
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_incidence_matrix function.
#' @param delta the time step between consecurtive birth_dates
#' @return a matrix of column length age_steps and row length birth_dates,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export

# incidence_matrix_fun <- function(age_steps,
#                                  birth_dates,
#                                  generate_incidence_fun,
#                                  delta)
# {
# 
#   times  <-  seq(min(birth_dates), max(birth_dates), delta)
#   ages <-    seq(0, age_steps, delta)
#   incidence_matrix <-  matrix(NA, nrow = length(times) + (length(ages)-1), ncol =  length(ages))
# 
#   counter <- 1
# 
#   age_index <- 1:length(ages)
#   time_index <- 0:(length(times)-1)
# 
#   for (aa in age_index){
#     #aa = 1
# 
#     incidence_matrix[time_index + aa , aa ] =  generate_incidence_fun(times + (ages[counter]+ 0.5 * delta),
#                                                                          (ages[counter] + 0.5 * delta))
# 
#     counter <- counter + 1
# 
#   }
# 
#   return(incidence_matrix)
# }






#OPTIONS B

incidence_matrix_function <- function(incidence_matrix,
                                      base_mortality_matrix,
                                      delta)
  {
  
 incidencce_adjusted <-  transform_data(incidence_matrix/(incidence_matrix + base_mortality_matrix)) *
   (1 - susceptible_cumulative_survival_fun(incidence_matrix,
                                            base_mortality_matrix,
                                            delta))
 return(incidencce_adjusted)
  
}



