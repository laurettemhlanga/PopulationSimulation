#' generate_susceptible_survival_rate
#'
#' a function that calculates the survival probabilities in the susceptibles
#'
#' @param incidence_matrix probability matrix of getting infected in the susceptible
#' @param base_mortality_matrix probability matrix of dying among the susceptible
#' @param time_1  lower time interval
#' @param time_2  upper time interval
#' @return returns the probability of not getting infected or dying within  susceptibles or the survival probability of the susceptibles
#' @examples x <- generate_susceptible_surv_rate (matrix(seq(0.01, 0.16, 0.01),
#' ncol = 2 , nrow = 8),
#' matrix(seq(0.01, 0.16, 0.01),
#' ncol = 2 , nrow = 8))


#Option A_ 2018

# generate_susceptible_surv_rate <- function(incidence_matrix,
#                                            base_mortality_matrix)
# {
#
#   susceptible_surv_rate = 1 - (incidence_matrix * base_mortality_matrix)
#
#
#   return(susceptible_surv_rate)
#
# }


#Option B_ 2019

susceptible_surv_rate <- function(incidence_matrix,
                                           base_mortality_matrix,
                                           delta = 1)
{

 susceptible_surv_rate = exp( - ((incidence_matrix * base_mortality_matrix) * delta) )

  return(susceptible_surv_rate)

}




# y <- susceptible_surv_rate (matrix(seq(0.01, 0.16, 0.01),
#                                             ncol = 2 , nrow = 8),
#                                      matrix(seq(0.01, 0.16, 0.01),
#                                             ncol = 2 , nrow = 8))
