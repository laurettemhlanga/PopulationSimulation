
#' infected_population_natrix
#'
#' a function that returns a matrix of the infected population for a given birthcohort
#'
#' @param susceptible the susceptible population counts
#' @param incidence_mat associated incidence rates
#' @param pmtct_birthcount number of newborns infected
#' @param time_step the time or age difference between to consecutive times or ages the probability of surving infection or death in the susceptible statemi.e it is uniform in all values supplied
#' @param cumulative_infected_survival the survival probability of  being aged a at time t having been infected for tau years.
#' @param base_mortality_mat associated excess mortality survivial rates
#' the probability of surving infection or death in the susceptible state
#'
#' @return returns an matrix of dimensions time t , age a and time since infection - tau
#'
#' @export


infected_population_matrix <- function(incidence_mat, base_mortality_mat, time_step,
                                       susceptible, pmtct_birthcount,
                                       cumulative_infected_survival){

  # infected <- (susceptible * incidence_mat * time_step * exp(-base_mortality_mat * time_step) * cumulative_infected_survival[1,])

  infected <- (susceptible[-length(susceptible)] * (1 - exp(-incidence_mat * time_step)) * exp(-base_mortality_mat * time_step) *
                 cumulative_infected_survival[1,-ncol(cumulative_infected_survival)])

  infectedpop <- matrix(NA, ncol = ncol(cumulative_infected_survival), nrow = nrow(cumulative_infected_survival))

  infectedpop[1,] <- c(pmtct_birthcount, infected)

  tau_indices = 2:ncol(infectedpop)
  age_indices = 1:ncol(infectedpop)


  for (tau_index in tau_indices){

    for (age_index in age_indices){


      if(age_index - tau_index < 0){

        infectedpop[tau_index, age_index] <- NA

      }else{

        infectedpop[tau_index, age_index] <- infectedpop[1, age_index - (tau_index - 1)] * cumulative_infected_survival[tau_index, age_index]
       }
    }

  }
  return(infectedpop)
}

