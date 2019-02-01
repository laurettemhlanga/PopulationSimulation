#' generate_infected_tau
#'
#' generate_infected_tau_0 returns a matrix of numeric values for age and time, representing the number of individuals within the population whose time since infection is 0
#' a matrix of susceptible population counts - as defined by the generate_susceptibles function - and...
#' a matrix of incidence - as defined by the generate_incidence_matrix function
#'
#' @param incidence_matrix a matrix of susceptible population counts, as defined by the generate_susceptibles function
#' @param susceptible_pop_counts a matrix of incidence, as defined by the generate_incidence_matrix function
#' @return a matrix of row length age_steps and column length birth_dates. Numeric values in each cell of the matrix
#' represent the number of infected individuals in the population whose time since infection is 0
#' @examples x <- generate_infected_tau_0 (incidence_matrix = matrix(rep(seq(0.1, 1, 1/10), 10), 10, 10),
#' susceptible_pop_counts = matrix(seq(100,1000, 10), 10, 10))


generate_infected_tau_0 <- function(incidence_matrix,
                                    susceptible_pop_counts)
{

  infected_tau_0 <- incidence_matrix * susceptible_pop_counts


  return(infected_tau_0)
}

# now that we allowing it to be called in the infecte[,,tau] function do we still need it
