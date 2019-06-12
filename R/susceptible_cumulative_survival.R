#' susceptible_cumulative_survival
#'
#' a function that returns a matrix of cumulative probabilities of survival -i.e. not dying and aquiring infection - for each age and time step of the simulation
#'
#' @param incidence_matrix probability matrix of getting infected in the susceptible
#' @param base_mortality_matrix probability matrix of dying among the susceptible
#' @param time_step the time or age difference between consecutive ages or time and it is uniform
#' @return a matrix calculated from the susceptible_survival_rate_matrix
#' Values stored in the matrix are numeric-double, from 0-1, which represent the  probability of surviving a specified age from birth in the susceptible population.
#
#'
#'
#'
#'
#'
#' @export


susceptible_cumulative_survival <- function(incidence_matrix,
                                            base_mortality_matrix,
                                            time_step)

{
  # calculates the probability of survival in the susceptibles pop, which is eventually used to estimate the cumulative probabilty
  # of surviving in the susceptibles from birth till current time.

  susceptible_survival_rate_matrix = exp( - ((incidence_matrix +  base_mortality_matrix) * time_step))


  susceptible_cumulative_survival_matrix <- matrix(NA, ncol = ncol(susceptible_survival_rate_matrix) + 1,
                                                   nrow = nrow(susceptible_survival_rate_matrix))



  susceptible_cumulative_survival_matrix[, 1] = rep(1,  nrow(susceptible_cumulative_survival_matrix))

  for  (age in 2:ncol(susceptible_cumulative_survival_matrix)){


        susceptible_cumulative_survival_matrix[ , age ] = susceptible_cumulative_survival_matrix[ , age - 1] *
          susceptible_survival_rate_matrix[ , age - 1]

  }
  return(susceptible_cumulative_survival_matrix)
}






























# susceptible_cumulative_survival_fun <- function(incidence_matrix,
#                                                 base_mortality_matrix,
#                                                 time_step )
#
# {
#
#   susceptible_survival_rate_matrix = exp( - ((incidence_matrix +  base_mortality_matrix) * time_step))
#   #calculate the survival rate matrix
#
#   susceptible_cumulative_survival_matrix <- matrix(NA, ncol = ncol(susceptible_survival_rate_matrix) + 1,
#                                                    nrow = nrow(susceptible_survival_rate_matrix) + 1)
#
#   column_1 <- (nrow(susceptible_survival_rate_matrix) - ncol(susceptible_survival_rate_matrix))+1
#
#   susceptible_cumulative_survival_matrix[1: column_1, 1] = rep(1,  column_1)
#
#   for  (age in 2:ncol(susceptible_survival_rate_matrix)){
#     for (tt in 2 : nrow(susceptible_survival_rate_matrix)){
#
#       if (!is.na(susceptible_survival_rate_matrix[tt, age]) == T){
#
#         susceptible_cumulative_survival_matrix[tt , age ] = susceptible_cumulative_survival_matrix[tt-1, age - 1] * susceptible_survival_rate_matrix[tt - 1 , age -1]
#
#       }else{
#         susceptible_cumulative_survival_matrix[tt, age]  = NA
#
#       }
#
#     }
#   }
#   return(transform_data(susceptible_cumulative_survival_matrix))
# }
#







# generate_susceptible_cumulative_survival_matrix <- function(susceptible_survival_rate_matrix)
#
# {
#
#   susceptible_cumulative_survival_matrix <- matrix(NA, ncol = ncol(susceptible_survival_rate_matrix), nrow = nrow(susceptible_survival_rate_matrix))
#
#   column_1 <- (nrow(susceptible_survival_rate_matrix) - ncol(susceptible_survival_rate_matrix))+1
#
#   susceptible_cumulative_survival_matrix[1: column_1, 1] = rep(1,  column_1)
#
#   for  (age in 2:ncol(susceptible_survival_rate_matrix)){
#     for (tt in 2 : nrow(susceptible_survival_rate_matrix)){
#
#       if (!is.na(susceptible_survival_rate_matrix[tt, age]) == T){
#
#         susceptible_cumulative_survival_matrix[tt , age ] = susceptible_cumulative_survival_matrix[tt-1, age - 1] * susceptible_survival_rate_matrix[tt - 1 , age -1]
#
#       }else{
#         susceptible_cumulative_survival_matrix[tt, age]  = NA
#
#       }
#
#     }
#   }
#   return(susceptible_cumulative_survival_matrix)
# }
#

