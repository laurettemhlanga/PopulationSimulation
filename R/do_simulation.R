#' do_one_simulation
#'
#' A wrapper function that returns a a list of the the susceptible and infected population
#'
#' @param first_birth_time the number of steps to age the population
#' @param last_birth_time a numeric vectors of length min:max; indicates the range of age to be included in simulation. Note that date format is not used.
#' @param time_step the time step between consecurtivelist_of_times
#' @param max_age maximum age each birth cohort is to be aged
#' @param birth_rate the birth rate in the hypothetival population
#' @param base_mortality mortality function due to natural causes as a function of age and time
#' @param incidence a function which takes as arguments age and time and returns a numberic rate of incidence for each age and time included in the simulation.
#' @param excess_mortality mortality function due to the disease causes as a function of age and time
#' This function can be defined by user or can be selected from among several default options included in the package.
#' The user-defined or package default function should be called by name when included as an argument in the generate_incidence_matrix function.
#'
#' @return a matrix of column length max_age and row lengthlist_of_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export





do_one_simulation <- function(first_birth_time, last_birth_time,
                              time_step, max_age,
                              birth_rate, base_mortality,
                              incidence ,
                              excess_mortality)
{

  #wrapper function to the functions in Population simulation project.

  list_of_times <- seq(first_birth_time, last_birth_time, time_step )

  birth_count <- birth_counts(dates_needing_birth_counts = list_of_times,
                              birth_rate = birth_rate, time_step = time_step)


  incidence_m <- incidence_matrix(max_age, list_of_times, incidence, time_step)


  base_mortality_m <- base_mortality_matrix(max_age, list_of_times, base_mortality, time_step)



  survival_prob <- susceptible_cumulative_survival(incidence_m, base_mortality_m,
                                                   time_step)


  susceptible_pop_counts <- susceptible_population(cumulative_survival_matrix = survival_prob,
                                                   birth_counts = birth_count)

  mod_incidence <-  incidence_matrix_exp(incidence_matrix = incidence_m,
                                         base_mortality_matrix = base_mortality_m,
                                         time_step = 1)

  #remember this is last column shot for the (age + 1)not recorded !!

  probability_surviving <-  probability_surviving_infected(max_age,
                                    list_of_times,
                                    time_step,
                                    excess_mortality ,
                                    base_mortality )


  cum_prob_survival_i <- cumulative_probability_surviving_infected(probability_surviving)


  infected <-  infected_population(susceptible = susceptible_pop_counts[,-ncol(susceptible_pop_counts)],
                                    incidence_matrix_mod = mod_incidence,
                                    cumulative_infected_survival = cum_prob_survival_i)


  return(list(susceptible_count = susceptible_pop_counts,  infected_count = infected))


}
