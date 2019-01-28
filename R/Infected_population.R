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


generate_infected_population_array <- function(susceptible_pop_counts,
                                               incidence_prob,
                                               survival_probability)
{


   infected_population_array <-  array(NA, dim = c(dim(survival_probability)[1], dim(survival_probability)[2],(dim(survival_probability)[3]+1)))
   infected_population_array[ , , 1] <- incidence_prob * susceptible_pop_counts



   for (times in 1:nrow(susceptible_pop_counts)){

     for(aa in 1:ncol(susceptible_pop_counts)){

       for (ta in 2:aa){

         if (any(times - (ta - 1) <= 0 | aa - (ta - 1) <= 0)){

           infected_population_array[times, aa ,ta] <- 0

         }else{

            infected_population_array[times, aa ,ta] <- incidence_matrix[times - (ta - 1), aa - (ta - 1)] *  susceptible_pop_counts[times - (ta - 1), aa - (ta - 1)] * survival_probability[times - (ta - 1), aa - (ta - 1), (ta - 1)]

         }

        }
     }
  }


  return(infected_population_array)

}


infected_population_array[3,3 ,3] <- incidence_matrix[3 - (3 - 1), 3- (3- 1)] *  susceptible_pop_counts[3 - (3 - 1), 3- (3- 1)] * survival_probability[3 - (3 - 1), 3- (3- 1), (3 - 1)]

generate_infected_population_array(susceptible_pop_counts = susceptible_pop_counts,
                                   incidence_prob = incidence_matrix,
                                   survival_probability = infected_survival_probs)


