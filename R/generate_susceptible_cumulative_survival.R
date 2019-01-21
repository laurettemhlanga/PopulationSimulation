#' generate_susceptible_cumulative_survival
#'
#' a function that returns a matrix of cumulative probabilities of survival -i.e. not dying and aquiring infection - for each age and time step of the simulation
#'
#' @param susceptible_survival_rate_matrix a survival probability matrix, as defined by the generate_susceptible_surv_rate function
#' @return a matrix of column length age_steps and row length birth_dates, calculated from the susceptible_survival_rate_matrix
#' Values stored in the matrix are numeric-double, from 0-1, which represent the cumulative probability of not dying and not aquiring infection among the susceptible population
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
