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
#' @param detailed allows for a switch in the function
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
                                    detailed = FALSE)
{# The function do_one_simulation takes user defined functions of
  # the rates stated or the user can use the rates that come with the package.
  # The user-defined or package default function should be called by name when included as an argument.

  #n_age_steps  <-  round(max_age/time_step)

  if (any(detailed == FALSE & time_slice >= date_of_birth)) {

    n_age_steps  <- round((max(time_slice) - date_of_birth) / time_step)

  }else{

    n_age_steps  <- round(max_age/time_step)
  }

  n_age_steps <- ifelse(n_age_steps <= 1, 3, n_age_steps)

  ages  <- seq(from = time_step / 2, by = time_step, length.out = n_age_steps)
  times <- seq(from = (date_of_birth + time_step/2), by = time_step, length.out = n_age_steps)


  birth_count <-  birth_rate(date_of_birth) * time_step
  pmtct_birth_count <-  pmtct_birth_rate(date_of_birth) * birth_count


  incidence_vector =  incidence_function(ages = ages, times = times)
  base_mortality_vector =  base_mortality_function(ages = ages,times = times)


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


  if (detailed == FALSE){

    cohort_survey_dates <- extract_cohort_status_surveydate(date_of_birth = date_of_birth, max_age = n_age_steps,
                                                            time_slice = time_slice, time_step = time_step,
                                                            population = population)
    return(cohort_survey_dates)

  }else{

    return(population)

  }

}

