#' generate_infected_mortality_array
#'
#' a function that simulates the infected population from a the susceptibles, incidence and the probalility of surviving to a given age and time having been infected for a time "time_since_infection"
#'
#' @param incidence_fun denotes a  incidence function that yields the probabilities of getting infected at a given age and time.
#' @param mortality_fun denotes a  moratlity function that yields the probabilities of not dying either from the condition or other natural causes of death.
#' @param susceptible_fun denotes a function that calculates the number of people who where susceptible at a given age and time
#' @return returns an array of dimensions time, age and "time_since_infection"
#' @examples
#' x <- generate_infected_mortality_array (age_steps = 2, birth_dates = 1992:1995,
#' generate_excess_mortality_tau_fun = generate_excess_mortality_tau,
#' generate_base_mortality_fun = generate_base_mortality)
#'
#'
#'




generate_incidence_matrix <- function(age_steps,
                                      birth_dates,
                                      generate_incidence_fun)
{

  incidence_matrix  = matrix(NA, nrow = length(birth_dates) + age_steps, ncol =  length(1:age_steps))
  times  = 0:length(birth_dates)

  for (aa in 1:age_steps){

    incidence_matrix[times + aa, aa] =  generate_incidence_fun(times + aa, aa)

  }

  return(incidence_matrix)
}




generate_susceptibles <- function(cumulative_survival_matrix,
                                  birth_counts)
{

  delta_d <- row(cumulative_survival_matrix) - col(cumulative_survival_matrix)
  susceptible_pop_counts  = matrix(NA, nrow = nrow(cumulative_survival_matrix), ncol =  ncol(cumulative_survival_matrix))

  susceptible_pop_counts[(1:length(birth_counts)), ] =  birth_counts

  seQ = min(delta_d):max(delta_d)

  for (aa in seQ){

    if (aa >= 0){
      # do we need the if statement seQ can be 0:max(delta_d)
      susceptible_pop_counts[delta_d == aa]  = cumulative_survival_matrix[delta_d == aa] * birth_counts[aa + 1] #(R strats counting at 1 i.e indexing birth_counts[0] yield an error)

    }else{

      susceptible_pop_counts[delta_d == aa] = NA

    }
  }
  return(susceptible_pop_counts)

}






generate_infected_population_array <- function(incidence_fun,
                                               mortality_fun,
                                               susceptible_fun
                                               )
{

  for (aa in 1:ncol())
   infected_population_array <-  array(NA, dim = dim(mortality_matrix))
   infected_population_array[ , , 1] <- incidence_fun()*mortality_fun()

   for(aa in bla){

  }

  return(infected_population_array)

}
