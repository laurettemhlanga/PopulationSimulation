





do_one_simulation <- function(first_birth_time = 1985, last_birth_time = 1990, time_step = 1, max_age = 3,
                              birth_rate = constant_birth_rate, base_mortality = time_indep_age_linear_base_mortality,
                              incidence = time_indept_age_tent_incidence,
                              excess_mortality = excess_mortality_fun)
{
  
  #wrapper function to the functions in Population simulation project.
  
  list_of_times <- seq(first_birth_time, last_birth_time, time_step )
  
  birth_count <- birth_counts(dates_needing_birth_counts = list_of_times,
                              birth_rate = birth_rate, time_step = time_step)
  
  
  incidence_m <- incidence_matrix(max_age, list_of_times, incidence, time_step)
  
  
  base_mortality_m <- base_mortality_matrix(max_age, list_of_times, base_mortality, time_step)
  
  
  
  survival_prob <- susceptible_cumulative_survival(incidence_matrix = incidence_m, base_mortality = base_mortality_m,
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
                                    excess_mortality = excess_mortality_fun,
                                    base_mortality = time_indep_age_linear_base_mortality)
  
  
  cum_prob_survival_i <- cumulative_infected_survival_probability(probability_surviving)
  
  
  infected <-  infected_population(susceptible = susceptible_pop_counts[,-ncol(susceptible_pop_counts)],
                                    incidence_matrix_mod = mod_incidence,
                                    cumulative_infected_survival = cum_prob_survival_i)
  
  
  return(list(susceptible_count = susceptible_pop_counts,  infected_count = infected))
  
  
}
