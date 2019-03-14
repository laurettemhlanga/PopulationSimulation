#' generate_susceptible_survival
#'
#' a function that returns a matrix of cumulative probabilities of survival -i.e. not dying and aquiring infection - for each age and time step of the simulation
#'
#' @param incidence_matrix probability matrix of getting infected in the susceptible
#' @param base_mortality_matrix probability matrix of dying among the susceptible
#' @param delta the time or age difference between consecutive ages or time and it is uniform
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

  susceptible_survival_rate_matrix = exp( - ((incidence_matrix +  base_mortality_matrix) * time_step))


  susceptible_cumulative_survival_matrix <- matrix(NA, ncol = ncol(susceptible_survival_rate_matrix) + 1,
                                                   nrow = nrow(susceptible_survival_rate_matrix))



  susceptible_cumulative_survival_matrix[, 1] = rep(1,  nrow(susceptible_cumulative_survival_matrix))

  for  (aa in 2:ncol(susceptible_cumulative_survival_matrix)){


        susceptible_cumulative_survival_matrix[ , aa ] = susceptible_cumulative_survival_matrix[ , aa - 1] * susceptible_survival_rate_matrix[ , aa - 1]

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
#   for  (aa in 2:ncol(susceptible_survival_rate_matrix)){
#     for (tt in 2 : nrow(susceptible_survival_rate_matrix)){
#
#       if (!is.na(susceptible_survival_rate_matrix[tt, aa]) == T){
#
#         susceptible_cumulative_survival_matrix[tt , aa ] = susceptible_cumulative_survival_matrix[tt-1, aa - 1] * susceptible_survival_rate_matrix[tt - 1 , aa -1]
#
#       }else{
#         susceptible_cumulative_survival_matrix[tt, aa]  = NA
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
#   for  (aa in 2:ncol(susceptible_survival_rate_matrix)){
#     for (tt in 2 : nrow(susceptible_survival_rate_matrix)){
#
#       if (!is.na(susceptible_survival_rate_matrix[tt, aa]) == T){
#
#         susceptible_cumulative_survival_matrix[tt , aa ] = susceptible_cumulative_survival_matrix[tt-1, aa - 1] * susceptible_survival_rate_matrix[tt - 1 , aa -1]
#
#       }else{
#         susceptible_cumulative_survival_matrix[tt, aa]  = NA
#
#       }
#
#     }
#   }
#   return(susceptible_cumulative_survival_matrix)
# }
#

