#' infected_population_fun
#'
#' a function that simulates the infected population from a the susceptibles, incidence and the probalility of surviving to a given age and time having been infected for a time "time_since_infection"
#'
#' @param incidence_prob denotes a  incidence matrix that yields the probabilities of getting infected at a given age and time.
#' @param survival_probability is the survival probability matrix associated with being infected at age a, time t, and for tau years.
#' @param susceptible_pop_counts denotes a function that calculates the number of people who where susceptible at a given age and time
#' @return returns an array of dimensions time, age and "time_since_infection - tau"
#'
#'
#'
#'
#' @export

infected_population_fun <- function(susceptible_pop_counts,
                                    incidence_prob = mod_incidence,
                                    survival_probability = Cum_prob_survival_i)
{

  #adopt pevious matrix dimensions
  infected_population_array <-  array(NA, dim = c(dim(survival_probability)[1],
                                                  dim(survival_probability)[2],
                                                  (dim(survival_probability)[3])))

  #for (times in 1:(nrow(susceptible_pop_counts) - 1)){

    for(aa in 1:ncol(susceptible_pop_counts)){

      for (ta in 1:(dim(infected_population_array)[3])){

        #if (any((times - ta ) <= 0 || (aa - ta ) <=  0)){


          #infected_population_array[times, aa ,ta  ] <-  ifelse(is.na(susceptible_pop_counts[times , aa]) == T, NA , 0)
          #ensures the entries that have NA by design are maintained as NAs and all other entries 0

       # }else{


          infected_population_array[ , aa ,ta  ] <- incidence_prob[, (aa - ta)] *
            susceptible_pop_counts[ , (aa - ta )] *
            survival_probability[ , aa , ta]
          #not sure if we should be subtracting the tau in survival probabilities


        #}

     # }
    }
  }


  return(infected_population_array)

}

# infected <- infected_population_fun(susceptible_pop_counts = susceptible_pop_counts,
#                                                incidence_prob = incidence_m,
#                                                survival_probability = infected_survival_probs)
#
















