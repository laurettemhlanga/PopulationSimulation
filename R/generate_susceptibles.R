#' generate_susceptibles
#'
#' a function that returns a matrix of numeric values for age and time, representing the number of individuals within the population who are alive and not infected
#'
#' @param survival_matrix matrix of survival probabilities for each age and time, defined by package's generate_susceptible_cumulative_survival_matrix function.
#' @param birth_counts vector of numberic values representing the number of  birth_counts at age and time
#' @return a matrix of row length age_steps and column length birth_dates. Numeric values in each cell of the matrix represent the number of individuals in the population who are alive and not infected

generate_susceptibles <- function(survival_matrix,
                                  birth_counts)
{

  delta_d <- row(survival_matrix) - col(survival_matrix)
  susceptible_pop_counts  = matrix(NA, nrow = nrow(survival_matrix), ncol =  ncol(survival_matrix))

  susceptible_pop_counts[(1:length(birth_counts)), ] =  birth_counts

  seQ = min(delta_d):max(delta_d)

  for (aa in seQ){

    if (aa >= 0){

      susceptible_pop_counts[delta_d == aa]  = survival_matrix[delta_d == aa] * birth_counts[aa + 1]

    }else{

      susceptible_pop_counts[delta_d == aa] = NA

    }
  }
  return(susceptible_pop_counts)

}
