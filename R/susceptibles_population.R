#' susceptible_population
#'
#' a function that returns a matrix of numeric values for age and time, representing the number of individuals within the population who are alive and not infected
#'
#' @param cumulative_survival_matrix matrix of cumulative survival probabilities in the susceptible from birth till the specified age and time.
#' @param birth_counts vector of numberic values representing the total birth counts for each birth count.
#' @param pmtct_birthcount number of newborns infected
#' @return a matrix of the susceptible population at a specified age and time.
#' number in each cell of the matrix represent the number of individuals in the population who are alive and un-infected
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


susceptible_population <- function(cumulative_survival_matrix,
                                   birth_counts,
                                   pmtct_birthcount)
  {

  # multiplies the birth counts with the cumulative probability of surviving infection/death in the
  # susceptible population at a specified time to yield the susceptibles at a given age and time

  susceptible <- cumulative_survival_matrix * (birth_counts - pmtct_birthcount)

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

