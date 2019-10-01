
#' infected_population_vector
#'
#' a function that returns a matrix that returns the infected population for a given birthcohort at specified age and time steps
#'
#' @param susceptible the susceptible population counts
#' @param incidence_mat associated incidence rates
#' @param pmtct_birthcount number of newborns infected
#' @param time_step the time or age difference between to consecutive times or ages the probability of surving infection or death in the susceptible statemi.e it is uniform in all values supplied
#' @param cumulative_infected_survival the survival probability of  being aged a at time t having been infected for tau years.
#' @param base_mortality_mat associated excess mortality survivial rates
#' the probability of surving infection or death in the susceptible state
#' @return returns an array of dimensions time t , age a and time since infection - tau
#'
#' @export


infected_population_vector <- function(incidence_mat, base_mortality_mat, time_step,
                                       susceptible, pmtct_birthcount,
                                       cumulative_infected_survival){


  infected <- susceptible * incidence_mat * time_step * exp(-base_mortality_mat * time_step) * cumulative_infected_survival[1,]


  infectedpop <- matrix(NA, ncol = ncol(cumulative_infected_survival), nrow = nrow(cumulative_infected_survival))

  infectedpop[1,] <- c(pmtct_birthcount, infected)

  tau_indices = 2:ncol(infectedpop)
  age_indices = 2:ncol(infectedpop)


  for (age_index in age_indices){

    for (tau_index in tau_indices){

      if(age_index - tau_index < 0){

        infectedpop[tau_index, age_index] <- NA

      }else{

        infectedpop[tau_index, age_index] <- infectedpop[1, age_index - (tau_index - 1)] * cumulative_infected_survival[tau_index, age_index]
      }
    }
    return(infectedpop)
  }
}





#' infected_population
#'
#' a function that returns an array of the infected population for a given birthcohort at specified age, time step and taus
#'
#' @param susceptible the susceptible population counts
#' @param incidence_mat associated incidence rates
#' @param pmtct_birthcount number of newborns infected
#' @param time_step the time or age difference between to consecutive times or ages the probability of surving infection or death in the susceptible statemi.e it is uniform in all values supplied
#' @param cumulative_infected_survival the survival probability of  being aged a at time t having been infected for tau years.
#' @param base_mortality_mat associated excess mortality survivial rates
#' the probability of surving infection or death in the susceptible state
#' @return returns an array of dimensions time t , age a and time since infection - tau
#'
#' @export


infected_population <- function(susceptible,
                                incidence_mat,
                                pmtct_birthcount,
                                base_mortality_mat,
                                cumulative_infected_survival,
                                time_step)

{

# Calculates the number of people who are HIV positive  at a given age and time for specific time
# since infections, this results in a 3 dimensional data structure of equal dimension to the cumulative_infected_survival
# The first infected[, , 1] is created from S(a, t) * Inc(a,t).For cases where age - tau < 0  the calculation defaults to
# infected[, age , ts] <- NA and the rest
# is derived from multiplying the number of people infected at  age - tau and time - tau and the cumulative probability
# of surviving in the infected population aged a at time t having been infected for tau years

  infected <- array(NA, dim = dim(cumulative_infected_survival))

  infected[, -1, 1] <- (susceptible *  incidence_mat * time_step * exp(-base_mortality_mat *time_step) * cumulative_infected_survival[, , 1])[, -dim(cumulative_infected_survival)[2]]

  infected[, 1, 1] <-  pmtct_birthcount

  for (age in 1:dim(cumulative_infected_survival)[2]){

    for (time_since_infection in 2:dim(cumulative_infected_survival)[3]){


      if (age - time_since_infection < 0 ){

        infected[, age , time_since_infection] <- NA

      }else{

        infected[, age , time_since_infection] <-  infected[, age - (time_since_infection-1), 1] *
          cumulative_infected_survival[ , age, time_since_infection]
      }
     }
    }

  return(infected)
}











# infected_population <- function(susceptible = susceptible_pop_counts,
#                                 modified_incidence = mod_incidence,
#                                 cumulative_survival = Cum_prob_survival_i)
#
# {
#   #fix the bug on calcilating the last column
#   susceptible <- susceptible[,-ncol(susceptible)]
#
#   infected_population_array <- array(NA, dim = dim(cumulative_survival))
#
#   infected_population_array[, , 1] <- susceptible * mod_incidence * cumulative_survival[, , 1]
#
#   for (ages in 1: dim(cumulative_survival)[2]){
#     for (tau in 2:dim(cumulative_survival)[3]){
#
#       if (ages - tau <= 0){
#
#
#         infected_population_array[ , ages , tau] <- NA
#
#
#       }else{
#
#         infected_population_array[ , ages , tau ] <- susceptible[ , ages - tau ] * modified_incidence[ , ages - tau] * cumulative_survival[ , ages, tau]
#     }
#
#   }
# }
#
#   return(infected_population_array)
# }
