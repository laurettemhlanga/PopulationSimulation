#' generate_infectedarray
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
#'
#'
#'
#'
#'
infected_population_a <- function(susceptible,
                                incidence_fun,
                                mortality_fun)
{
  infected_population_a <- array(NA, dim = c(nrow(susceptible), ncol(susceptible), ((dim(mortality_fun))[3] + 1)))
  infected_population_a [ , ,1] <-  susceptible * incidence_fun

  for (time in 0:nrow(susceptible)){

    for (age in 0:ncol(susceptible)){

      for (tau in 1:age){

        if (time  - tau <= 0 || age - tau <= 0){

          infected_population_a[time, age, tau -1] <-  0

        }else{

          infected_population_a[time, age, tau + 1] <-  susceptible[time - tau +1, age - tau ] *incidence_fun [time - tau , age- tau ] * mortality_fun[time, age, tau]
          }
        }

    }

  }

  return(infected_population_a)
}


infected_population_a(susceptible = susceptible_pop_counts,
                    incidence_fun = incidence_matrix,
                    mortality_fun = infected_survival_probs)


time  = 1; age  = 1

tau  = 1

infected_population_a[ time , age , 2] <-  susceptible[time - tau +1, age - tau +1] * incidence_fun [time - tau +1, age- tau +1] * mortality_fun[time, age, tau]
infected_population_a[ time , age , 2] <-  susceptible[time - tau +1, age - tau +1] * incidence_fun [time - tau +1, age- tau +1] * mortality_fun[time, age, tau]






























