#' birth_cohort_simulation
#'
#' A wrapper function that returns a list of the the susceptible and infected population
#'
#' @param date_of_birth the minimum date of birth for the birth cohorts
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param max_age maximum age attained by each birth cohort
#' @param birth_rate the birth rate in the hypothetical population at the specified times
#' @param pmtct_birth_rate the birth rate newborns who are infecteds.
#' @param base_mortality_function a function that specifies  the rate of occurence of natural deaths with arguments age and time.
#' @param incidence_function a function that specifies  the rate of occurence of the infections with arguments age and time.
#' @param excess_mortality_function a function that specifies  the rate of occurence ofdisease induced deaths with arguments age and time.
#' @param time_slice dates for conducting surveys
#' @param compact allows for a switch in the function
#'
#'
#' @return a matrix of column length max_age and row length list_of_birth_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export




birth_cohort_simulation <- function(date_of_birth,
                                    time_step, max_age, time_slice,
                                    birth_rate,
                                    pmtct_birth_rate,
                                    base_mortality_function,
                                    incidence_function,
                                    excess_mortality_function,
                                    compact = FALSE)
{

  # The function do_one_simulation takes user defined functions of
  # the rates stated or the user can use the rates that come with the package.
  # The user-defined or package default function should be called by name when included as an argument.

  #n_age_steps  <-  round(max_age/time_step)

  if (any(compact == FALSE & time_slice >= date_of_birth)) {

    n_age_steps  <- round((max(time_slice) - date_of_birth) / time_step)

  }else{

    n_age_steps  <- round(max_age/time_step)
  }

  n_age_steps <- ifelse(n_age_steps <= 1, 3, n_age_steps)

  ages  <- seq(from = time_step / 2, by = time_step, length.out = n_age_steps)
  times <- seq(from = (date_of_birth + time_step/2), by = time_step, length.out = n_age_steps)


  birth_count <-  birth_rate(date_of_birth) * time_step
  pmtct_birth_count <-  pmtct_birth_rate(date_of_birth) * birth_count


  incidence_vector =  incidence_function(vector_of_ages = ages, vector_of_times = times)
  base_mortality_vector =  base_mortality_function(vector_of_ages = ages, vector_of_times = times)


  susceptible_survival_prob <- susceptible_cumulative_survival_vector(incidence_vector = incidence_vector,
                                                               base_mortality_vector = base_mortality_vector,
                                                               time_step = time_step)

  excess_mortality_a <- wedge_excess_mortality_matrix(n_age_steps = n_age_steps,
                                                    list_of_times = times,
                                                    excess_mortality = excess_mortality_function,
                                                    time_step = time_step)

  infected_survival_prob <- probability_surviving_infected_matrix(excess_mortality_matrix = excess_mortality_a,
                                                                  base_mortality_vector = base_mortality_vector,
                                                                 time_step = time_step)

  cum_prob_survival_i <- infected_cumulative_survival_prob(infected_survival_prob)

  susceptible_pop_counts <- susceptible_population(cumulative_survival_matrix = susceptible_survival_prob,
                                                   birth_counts = birth_count,
                                                   pmtct_birthcount = pmtct_birth_count)

  infected_pop_counts <-  infected_population_matrix(susceptible = susceptible_pop_counts,
                                                      pmtct_birthcount = pmtct_birth_count,
                                                      incidence_mat = incidence_vector,
                                                      base_mortality_mat =  base_mortality_vector,
                                                      cumulative_infected_survival = cum_prob_survival_i,
                                                      time_step  = time_step)


  population <- compact_birthcohort(susceptible = susceptible_pop_counts, infected = infected_pop_counts)


  if (compact == FALSE){

    cohort_survey_dates <- extract_cohort_status_surveydate(date_of_birth = date_of_birth, max_age = n_age_steps,
                                                            survey_dates = time_slice, time_step = time_step,
                                                            population = population)
    return(cohort_survey_dates)

  }else{

    return(population)

  }

}

# birth_cohort_simulation(date_of_birth = 0, time_step = 1,
#                         max_age = 4, time_slice = 4,
#                         excess_mortality_function = step_excess_mortality,
#                         pmtct_birth_rate = constant_pmtct_rate,
#                         incidence_function = time_indept_age_tent_incidence,
#                         birth_rate = constant_birth_rate,
#                         base_mortality_function = time_indep_age_linear_base_mortality,
#                         compact = TRUE)
#
#
# birth_cohort_simulation(date_of_birth = 0, time_step = 1,
#                         max_age = 4, time_slice = 4,
#                         excess_mortality_function = step_excess_mortality,
#                         pmtct_birth_rate = constant_pmtct_rate,
#                         incidence_function = time_indept_age_tent_incidence,
#                         birth_rate = constant_birth_rate,
#                         base_mortality_function = time_indep_age_linear_base_mortality,
#                         compact = FALSE)





#
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
