#' probability_recent_among_positive
#'
#' calculates the probability of being recently infected given that you already HIV positive.
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param population_at_date Status of the popuation in the population.
#' @param probability_of_recent_infection a function that takes in possible  time since infection in years and calculates the probability
#' of being infected for that amount of years
#' @param type type of function to be utilised in the probability of infection function
#' @param keeping_infection_time if TRUE the function returns the probability of being recently infected among the positives for
#' given ages and times since infection, else it returns the sum of the columns. Which is the probability of being recently infected
#' among the positives at a specified age summing over time since infection.
#'
#'
#'
#' @return a matrix with the probability of testing recently infected among the positive.
#'
#'
#' @export



probability_recent_among_positive <- function(time_step,
                                              type,
                                              population_at_date,
                                              probability_of_recent_infection,
                                              keeping_infection_time = FALSE)
  {
  # calculates the probability of being recently infected among the positive,
  # it takes the population at the date of interest
  #

    population_infected <- population_at_date[-1, ]

    time_in_years <- seq(time_step, (nrow(population_infected) * time_step), time_step)

    recent_among_positive <- matrix(NA, ncol = ncol(population_infected), nrow = nrow(population_infected))

    for (age_index in (1:ncol(population_infected))){

        age_prevalence <- population_infected[ ,age_index] / sum(population_at_date[  ,age_index], na.rm = T)

        #recent_among_positive[ ,age_index] <- age_prevalence *  probability_of_recently_infected(time_in_years)

        recent_among_positive[ ,age_index] <- age_prevalence *  probability_of_recent_infection(time_in_years, type)
      }

     if (keeping_infection_time){

        return(recent_among_positive)

    }else{

      return(colSums(recent_among_positive, na.rm = TRUE))
    }

  }




