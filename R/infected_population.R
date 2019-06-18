
#' infected_population
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param susceptible the susceptible population counts
#' @param  incidence_mat associated incidence rates
#' @param time_step the time or age difference between to consecutive times or ages the probability of surving infection or death in the susceptible statemi.e it is uniform in all values supplied
#' @param cumulative_infected_survival the survival probability of  being aged a at time t having been infected for tau years.
#' @param base_mortality_mat associated excess mortality survivial rates
#' the probability of surving infection or death in the susceptible state
#' @return returns an array of dimensions time t , age a and time since infection - tau
#'
#' @export





infected_population <- function(susceptible,
                                incidence_mat,
                                base_mortality_mat,
                                cumulative_infected_survival,
                                time_step)

{

# Calculates the number of people who are HIV positive  at a given age and time for specific time
# since infections, this results in a 3 dimensional data structure of equal dimension to the cumulative_infected_survival
# The first infected[, , 1] is created from S(a, t) * Inc(a,t).For cases where age - tau < 0  the calculation defaults to  infected[, age , ts] <- NA and the rest
# is derived from multiplying the number of people infected at  age - tau and time - tau and the cumulative probability
# of surviving in the infected population aged a at time t having been infected for tau years

  infected <- array(NA, dim = dim(cumulative_infected_survival))

  infected[, , 1] <- susceptible *  incidence_mat * time_step * exp(-base_mortality_mat *time_step) * cumulative_infected_survival[, , 1]


  for (age in 1:dim(cumulative_infected_survival)[2]){

    for (time_since_infection in 2:dim(cumulative_infected_survival)[3]){


      if (age - time_since_infection < 0 ){

        infected[, age , time_since_infection] <- NA

      }else{

        infected[, age , time_since_infection] <-  infected[, age - (time_since_infection-1), 1] *
          cumulative_infected_survival[ , age, time_since_infection] * time_step

      }
     }
    }

  return(infected)
}





# I <- infected_population(susceptible = susceptible_pop_counts,
#                          incidence_mat = incidence_m,
#                          cumulative_infected_survival = cum_prob_survival_i)













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
