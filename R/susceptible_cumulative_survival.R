
#' susceptible_cumulative_survival_vector
#'
#' a function that returns a matrix of cumulative probabilities of survival -i.e. not dying and aquiring infection - for each age and time step of the simulation
#'
#' @param incidence_vector  vector of  incidence rates for specified ages and times
#' @param base_mortality_vector vector of  natural mortality rates for specified ages and times
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @return a vector of cumulative probabilities (from birth till age a at time t) of surviving in the susceptible population without being infected or dying from natural death  .
#' Values stored in the matrix are numeric-double, from 0-1, which represent
#' the  probability of surviving a specified age from birth in the susceptible population.
#
#'
#'
#'
#'
#'
#' @export

susceptible_cumulative_survival_vector <- function(incidence_vector,
                                                   base_mortality_vector,
                                                   time_step)

{
  # calculates the cumulative probabilty of surviving (CP_ss(t,a)) in the susceptibles population,
  # from the incidence matrix and base mottality matrix for the specified age, and time.
  # First the function calculates the probabilty of surviving in the specified time space
  # P_ss(t,a) and then the CP_ss(t,a) = CP_ss(t -1,a - 1)*P_ss(t,a)

  susceptible_survival_rate_vector <-  exp( - ((incidence_vector +  base_mortality_vector) * time_step))


  susceptible_cumulative_survival_vector <- as.vector(rep(NA, length(incidence_vector) + 1))
  susceptible_cumulative_survival_vector[1] <-  1

  for  (age in 2: length(susceptible_cumulative_survival_vector)){


    susceptible_cumulative_survival_vector[age] <-  susceptible_cumulative_survival_vector[age - 1] *
      susceptible_survival_rate_vector[age - 1]

  }
  return(susceptible_cumulative_survival_vector)
}


