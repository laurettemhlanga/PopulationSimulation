#' generate_infected_population_array
#'
#' a function that simulates the infected population from a the susceptibles, incidence and the probalility of surviving to a given age and time having been infected for a time "time_since_infection"
#'
#' @param incidence_fun denotes a  incidence function that yields the probabilities of getting infected at a given age and time.
#' @param mortality_fun denotes a  moratlity function that yields the probabilities of not dying either from the condition or other natural causes of death.
#' @param susceptible_fun denotes a function that calculates the number of people who where susceptible at a given age and time
#' @return returns an array of dimensions time, age and "time_since_infection"
#' @examples
#' Infected <- generate_infected_population_array(susceptible_pop_counts = susceptible_pop_counts,
#'  incidence_prob = incidence_m,survival_probability = infected_survival_probs)
#'
#'


generate_infected_population_array <- function(susceptible_pop_counts,
                                               incidence_prob,
                                               survival_probability)
{

  #adopt pevious matrix dimensions
  infected_population_array <-  array(NA, dim = c(dim(survival_probability)[1],
                                                  dim(survival_probability)[2],
                                                  (dim(survival_probability)[3]) + 1))

  infected_population_array[ , , 1] <- incidence_prob * susceptible_pop_counts



  for (times in 1:nrow(susceptible_pop_counts)){

    for(aa in 1:ncol(susceptible_pop_counts)){

      for (ta in 1:(dim(infected_population_array)[3] - 1)){

        if (any(((times - ta ) <= 0 || (aa - ta ) <=  0)) ){


          infected_population_array[times, aa ,ta  + 1 ] <-  ifelse(is.na(susceptible_pop_counts[times , aa]) == T, NA , 0)
          #ensures the entries that have NA by design are maintained

        }else{


          infected_population_array[times, aa ,ta + 1 ] <- incidence_prob[(times - ta), (aa - ta)] *
            susceptible_pop_counts[(times - ta), (aa - ta )] *
            survival_probability[(times - ta ), (aa - ta ), ta]


        }

      }
    }
  }


  return(infected_population_array)

}

infected <- generate_infected_population_array(susceptible_pop_counts = susceptible_pop_counts,
                                               incidence_prob = incidence_m,
                                               survival_probability = infected_survival_probs)

















