
#' do_simulation
#'
#' a function that simulates an age and time structured population from a non-mechanistic model.
#' The user specifies a range of ages, a number of steps forward in time that the population will be aged,
#' and functions specifying the birth rate, incidence mortality and excess mortality among infected members of the popultion.
#' The funciton returns a matrix - or matrices - of the total number of individual members of the simulated population who are
#' infected and susceptible at each age and time specified by the user.
#' The function also returns a matrix - or matrices -  of the prevalence of infection at each age and time.
#'
#' @param total_births a number, indicates the total number of individual members of the population
#' @param birth_dates a vector of numbers, indicating the minimum and the maximum ages to be included in the simulation
#' @param delta a number, indicates the rate of change between each age/time step, which is used to calculate the total number of births
#' @param age_steps a number, indicates the number of steps forward in time that the population will be aged
#' @param generate_birth_counts_fun a function, indicating the rate that birth occur for each age and time
#' May be user defined. Otherwise a default value requiring no additional input to the argument of the do dim function is provided by the package
#' @param generate_incidence_fun a function, indicating the incidence rate for each age and time
#' May be user defined. Otherwise a default value requiring no additional input to the argument of the do dim function is provided by the package
#' @param generate_base_mortality_fun a function, indicating the rate of mortality in the non-infected population
#' May be user defined. Otherwise a default value requiring no additional input to the argument of the do dim function is provided by the package
#' @param generate_base_mortality_tau_fun a function, indicating the rate of mortality in the infected population
#' May be user defined. Otherwise a default value requiring no additional input to the argument of the do dim function is provided by the package
#' @return a list, including a matrix of counts of the infected population, a matrix of counts of the susceptible population, and a matrix of the
#' prevalence of infection among the total population at each age and time indicated by the simulation
#' @export
#' @examples do_simiulation(total_births = 1000,
#'                         birth_dates = 1945:1950,
#'                         delta = 1,
#'                         age_steps = 3,
#'                         generate_birth_counts_fun = generate_birth_counts,
#'                         generate_incidence_fun = generate_incidence,
#'                         generate_base_mortality_fun = generate_base_mortality,
#'                         generate_base_mortality_tau_fun = generate_excess_mortality_tau)


do_simiulation <- function (total_births,
                            birth_dates,
                            delta,
                            age_steps,
                            generate_birth_counts_fun = generate_birth_counts,
                            generate_incidence_fun = generate_incidence,
                            generate_base_mortality_fun = generate_base_mortality,
                            generate_base_mortality_tau_fun = generate_base_mortality_fun)
{

  birth_counts <- generate_birth_counts_fun (total_births, birth_dates, delta)

  incidence_matrix <- generate_incidence_matrix (age_steps, birth_dates, generate_incidence_fun)

  base_mortality_matrix <- generate_base_mortality_matrix (age_steps,birth_dates, generate_base_mortality_fun)

  susceptible_survival_rate_matrix <- generate_susceptible_surv_rate(incidence_matrix, base_mortality_matrix)

  susceptible_cumulative_survival_matrix <- generate_susceptible_cumulative_survival_matrix(susceptible_survival_rate_matrix)

  susceptible_pop_counts <- generate_susceptibles(susceptible_cumulative_survival_matrix, birth_counts)

  # infected_mortality_matrix <- generate_infected_mortality_matrix (generate_base_mortality_fun,
  #                                                                  generate_excess_mortality_fun,
  #                                                                  birth_dates, age_steps) # excess + baseline mortality

  infected_tau_0_counts <- generate_infected_tau_0(incidence_matrix, susceptible_pop_counts)

  # infected_mortality_array <- generate_infected_mortality_array (generate_base_mortality_fun,
  #                                                                generate_excess_mortality_tau_funy,
  #                                                                birth_dates,
  #                                                                age_steps) # excess + baseline mortality
  #
  # infected_pop_counts <- generate_infected_population (infected_tau_0_counts, infected_mortality_array) # do we need a matrix version of this

  prevalence <- infected_tau_0_counts/ (susceptible_pop_counts + infected_tau_0_counts)
  output <- list (infected_tau_0_counts, susceptible_pop_counts, prevalence)

  return(output)

}
