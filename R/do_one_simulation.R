#' birth_cohort_simulation
#'
#' A wrapper function that returns a list of the the susceptible and infected population
#'
#' @param date_of_birth the minimum date of birth for the birth cohorts
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param max_age maximum age attained by each birth cohort
#' @param birth_rate the birth rate in the hypothetical population at the specified times
#' @param pmtct_birth_rate the birth rate newborns who are infecteds.
#' @param base_mortality a function that specifies  the rate of occurence of natural deaths with arguments age and time.
#' @param incidence a function that specifies  the rate of occurence of the infections with arguments age and time.
#' @param excess_mortality a function that specifies  the rate of occurence ofdisease induced deaths with arguments age and time.
#'
#'
#'
#' @return a matrix of column length max_age and row length list_of_birth_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export




birth_cohort_simulation <- function(date_of_birth,
                              time_step, max_age,
                              birth_rate,
                              pmtct_birth_rate,
                              base_mortality,
                              incidence ,
                              excess_mortality)
{

  # The function do_one_simulation takes user defined functions of
  # the rates stated or the user can use the rates that come with the package.
  # The user-defined or package default function should be called by name when included as an argument.

  list_of_times <- seq(from  = date_of_birth, to = (date_of_birth + max_age), by = time_step)

  birth_count <- birth_counts(dates_needing_birth_counts = date_of_birth,
                              birth_rate = birth_rate, time_step = time_step)

  pmtct_birth_count <- pmtct_birthcounts(dates_needing_birth_counts = date_of_birth, birth_count = birth_count,
                                         pmtct_birth_rate = pmtct_birth_rate, time_step = time_step)

  incidence_m <- incidence_vector(max_age = max_age,
                                  list_of_times = list_of_times,
                                  incidence = incidence, time_step = time_step)

  base_mortality_m <- base_mortality_vector(max_age = max_age,
                                            list_of_times = list_of_times,
                                            base_mortality = base_mortality,
                                            time_step = time_step)

  susceptible_survival_prob <- susceptible_cumulative_survival_vector(incidence_vector = incidence_m,
                                                               base_mortality_vector = base_mortality_m,
                                                               time_step = time_step)

  excess_mortality_a <- wedge_excess_mortality_matrix(max_age = max_age,
                                                    list_of_times = list_of_times,
                                                    excess_mortality = excess_mortality,
                                                    time_step = time_step)

  infected_survival_prob <- probability_surviving_infected_matrix(excess_mortality_matrix = excess_mortality_a,
                                                                  base_mortality_vector = base_mortality_m[-1],
                                                                 time_step = time_step)

  cum_prob_survival_i <- infected_cumulative_survival_prob(infected_survival_prob)

  susceptible_pop_counts <- susceptible_population(cumulative_survival_matrix = susceptible_survival_prob,
                                                   birth_counts = birth_count,
                                                   pmtct_birthcount = pmtct_birth_count)

  infected_pop_counts <-  infected_population_matrix(susceptible = susceptible_pop_counts,
                                                      pmtct_birthcount = pmtct_birth_count,
                                                      incidence_mat = incidence_m,
                                                      base_mortality_mat =  base_mortality_m,
                                                      cumulative_infected_survival = cum_prob_survival_i,
                                                      time_step  = time_step)


  population <- compact_birthcohort(susceptible = susceptible_pop_counts, infected = infected_pop_counts)

  return(population)


}


#
# date_of_birth = 1990
# time_step = 1
# max_age = 3
# birth_rate = constant_birth_rate
# pmtct_birth_rate = constant_pmtct_rate
# base_mortality = time_indep_age_linear_base_mortality
# incidence  = incidence_mahiane
# excess_mortality = excess_mahiane



#' do_one_simulation
#'
#' A wrapper function that returns a list of the the susceptible and infected population
#'
#' @param first_birth_time the minimum date of birth for the birth cohorts
#' @param last_birth_time the maximum date of birth for the birth cohorts
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param max_age maximum age attained by each birth cohort
#' @param birth_rate the birth rate in the hypothetical population at the specified times
#' @param pmtct_birth_rate the birth rate newborns who are infecteds.
#' @param base_mortality a function that specifies  the rate of occurence of natural deaths with arguments age and time.
#' @param incidence a function that specifies  the rate of occurence of the infections with arguments age and time.
#' @param excess_mortality a function that specifies  the rate of occurence ofdisease induced deaths with arguments age and time.
#'
#'
#'
#' @return a matrix of column length max_age and row length list_of_birth_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export





do_one_simulation <- function(first_birth_time, last_birth_time,
                              time_step, max_age,
                              birth_rate,
                              pmtct_birth_rate,
                              base_mortality,
                              incidence ,
                              excess_mortality)
{

  # The function do_one_simulation takes user defined functions of
  # the rates stated or the user can use the rates that come with the package.
  # The user-defined or package default function should be called by name when included as an argument.

  list_of_birth_times <- seq(first_birth_time,
                             last_birth_time,
                             time_step)

  birth_count <- birth_counts(dates_needing_birth_counts = list_of_birth_times,
                              birth_rate = birth_rate, time_step = time_step)

  pmtct_birth_count <- pmtct_birthcounts(dates_needing_birth_counts = list_of_birth_times, birth_count = birth_count,
                                         pmtct_birth_rate = pmtct_birth_rate, time_step = time_step)

  incidence_m <- incidence_matrix(max_age = max_age,
                                  list_of_birth_times = list_of_birth_times,
                                  incidence = incidence, time_step = time_step)


  base_mortality_m <- base_mortality_matrix(max_age = max_age,
                                            list_of_birth_times = list_of_birth_times,
                                            base_mortality = base_mortality,
                                            time_step = time_step)

  susceptible_survival_prob <- susceptible_cumulative_survival(incidence_matrix = incidence_m,
                                                               base_mortality_matrix = base_mortality_m,
                                                               time_step = time_step)


  excess_mortality_a = wedge_excess_mortality_array(max_age = max_age,
                                                    list_of_birth_times = list_of_birth_times,
                                                    excess_mortality = excess_mortality,
                                                    time_step = time_step)

  infected_survival_prob <- probability_surviving_infected_array(wedge_of_excess_mortality_array = excess_mortality_a,
                                                                matrix_of_base_mortality = base_mortality_m ,
                                                                time_step = time_step)

  cum_prob_survival_i <- cumulative_probability_surviving_infected(infected_survival_prob)




  susceptible_pop_counts <- susceptible_population(cumulative_survival_matrix = susceptible_survival_prob,
                                                   birth_counts = birth_count,
                                                   pmtct_birthcount = pmtct_birth_count)



  infected_pop_counts <-  infected_population(susceptible = susceptible_pop_counts,
                                              pmtct_birthcount = pmtct_birth_count,
                                              incidence_mat = incidence_m,
                                              base_mortality_mat =  base_mortality_m,
                                              cumulative_infected_survival = cum_prob_survival_i,
                                              time_step  = time_step)


  population <- compact_population(susceptible = susceptible_pop_counts, infected = infected_pop_counts)


  return(population)


}
