#' probability_recent_among_positive
#'
#' calculates the probability of being recently infected given that you already HIV positive.
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param population_at_date Status of the popuation in the population.
#'
#'
#'
#' @return a matrix with the probability of testing recently infected among the positive.
#'
#'
#' @export



probability_recent_among_positive <- function(time_step,
                                              population_at_date)
  {

    population_infected <- population_at_date[-1, ]

    time_in_years <- seq(time_step, (nrow(population_infected) * time_step), time_step)

    recent_among_positive <- matrix(NA, ncol = ncol(population_infected), nrow = nrow(population_infected))

    for (age_index in (1:ncol( population_infected))){

      age_prevalence <- population_infected[ ,age_index] / sum(population_infected[  ,age_index], na.rm = T)

      recent_among_positive[ ,age_index] <- age_prevalence *  probability_of_recently_infected(time_in_years)

    }

    return(recent_among_positive)
}




