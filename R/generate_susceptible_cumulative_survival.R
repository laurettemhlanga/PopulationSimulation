#' generate_susceptible_survival
#'
#' a function that returns a matrix of cumulative probabilities of survival -i.e. not dying and aquiring infection - for each age and time step of the simulation
#'
#' @param susceptible_survival_rate_matrix a survival probability matrix
#' @return a matrix calculated from the susceptible_survival_rate_matrix
#' Values stored in the matrix are numeric-double, from 0-1, which represent the  probability of surviving a specified age from birth in the susceptible population.
#' @examples x <- generate_susceptible_cumulative_survival_matrix
#' (matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8))


generate_susceptible_cumulative_survival_matrix <- function(susceptible_survival_rate_matrix)

{

  susceptible_cumulative_survival_matrix <- matrix(NA, ncol = ncol(susceptible_survival_rate_matrix), nrow = nrow(susceptible_survival_rate_matrix))

  column_1 <- (nrow(susceptible_survival_rate_matrix) - ncol(susceptible_survival_rate_matrix))+1

  susceptible_cumulative_survival_matrix[1: column_1, 1] = rep(1,  column_1)

  for  (aa in 2:ncol(susceptible_survival_rate_matrix)){
    for (tt in 2 : nrow(susceptible_survival_rate_matrix)){

      if (!is.na(susceptible_survival_rate_matrix[tt, aa]) == T){

        susceptible_cumulative_survival_matrix[tt , aa ] = susceptible_cumulative_survival_matrix[tt-1, aa - 1] * susceptible_survival_rate_matrix[tt - 1 , aa -1]

      }else{
        susceptible_cumulative_survival_matrix[tt, aa]  = NA

      }

    }
  }
  return(susceptible_cumulative_survival_matrix)
}








susceptible_survival_rate_matrix <- matrix(seq(0.01, 0.24, 0.01), ncol = 3, nrow = 8)

x <- generate_susceptible_cumulative_survival_matrix(matrix(seq(0.01, 0.24, 0.01), ncol = 3, nrow = 8))

