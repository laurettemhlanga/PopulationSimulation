#' susceptible_fun
#'
#' a function that returns a matrix of numeric values for age and time, representing the number of individuals within the population who are alive and not infected
#'
#' @param cumulative_survival_matrix matrix of survival probabilities in the susceptible for each age and time.
#' @param birth_counts vector of numberic values representing the number of  births at each time between time_1 and time_2.
#' @return a matrix of the susceptible at specified ages and calculations. Numeric values in each cell of the matrix represent the number of individuals in the population who are alive and not infected
#'
#'
#'
#'
#'
#'
#'
#' @export
#'
#'


#option 2

susceptible_population <- function(cumulative_survival_matrix,
                                   birth_counts)
  {

  susceptible <- cumulative_survival_matrix * birth_counts

  return(susceptible)
}




# susceptible_population_fun <- function(cumulative_survival_matrix,
#                                   birth_counts)
# {
#
#   delta_d <- row(cumulative_survival_matrix) - col(cumulative_survival_matrix)
#   susceptible_pop_counts  = matrix(NA, nrow = nrow(cumulative_survival_matrix), ncol =  ncol(cumulative_survival_matrix))
#
#   susceptible_pop_counts[(1:length(birth_counts)), ] =  birth_counts
#
#   seQ = min(delta_d):max(delta_d)
#
#   for (aa in seQ){
#
#     if (aa >= 0){
#        # do we need the if statement seQ can be 0:max(delta_d)
#       susceptible_pop_counts[delta_d == aa]  = cumulative_survival_matrix[delta_d == aa] * birth_counts[aa + 1] #(R strats counting at 1 i.e indexing birth_counts[0] yield an error)
#
#     }else{
#
#       susceptible_pop_counts[delta_d == aa] = NA
#
#     }
#   }
#   return(susceptible_pop_counts)
#
# }


# susceptible_pop_counts <- susceptible_population_fun(cumulative_survival_matrix = survival_prob,
#                                                 birth_counts = birth_count)
#

