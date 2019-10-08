#' prevalences_calculation
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
#'
#'
#'
#' @return a list of prevalences i.e. prevalence of HIV status, prevalence of recency,and prevelance of HIV age and tau .
#'
#' @export


prevalences_calculation <- function(time_step ,type ,
                                    population_at_date,
                                    probability_of_recent_infection)
{
  # calculates the prevalences of recency, hiv status and of tau and age
  # at a particular time
  #

  #population_infected <- population_at_date[-1, ]
  if( is.vector(population_at_date) == T){

    time_in_years <- seq(time_step, (length(population_at_date[-1 ]) * time_step), time_step)

    overall_age_prevalence <- sum(population_at_date[-1], na.rm = T) / sum(population_at_date,  na.rm = T)

    prevalence_T <- population_at_date / sum(population_at_date,  na.rm = T)

    recent_infections <- population_at_date[-1] * probability_of_recent_infection(time_in_years, type)

    prevalence_R <- sum(recent_infections, na.rm = TRUE) / (overall_age_prevalence * sum(population_at_date,  na.rm = T))

  }else{

    time_in_years <- seq(time_step, (nrow(population_at_date[-1, ]) * time_step), time_step)

    overall_age_prevalence <- colSums(population_at_date[-1,], na.rm = T) / colSums(population_at_date,  na.rm = T)

    prevalence_T <- matrix(NA, ncol = ncol(population_at_date), nrow = nrow(population_at_date))
    recent_infections <- matrix(NA, ncol = ncol(population_at_date[-1, ]), nrow = nrow(population_at_date[-1, ]))

    for (age_index in (1:ncol(population_at_date))){

      prevalence_T[ ,age_index] <- population_at_date[ ,age_index] / sum(population_at_date[, age_index],  na.rm = T)

      recent_infections[ ,age_index] <- population_at_date[-1, age_index ] * probability_of_recent_infection(time_in_years, type)

    }


     prevalence_R <- colSums(recent_infections, na.rm = TRUE) / (overall_age_prevalence * colSums(population_at_date,  na.rm = T))

     }

  return(list( prevalence_H = overall_age_prevalence, prevalence_R = prevalence_R, prevalence_T = prevalence_T))
  }


# x <-  matrix(c(c(100, 80, 60, 50),
#                c(20,10,12,10),
#                c(NA,2,8,6),
#                c(NA, NA,4,3)),
#              byrow = T, nrow = 4)
#
#
# y = prevalences_calculation(time_step = 1 ,type = "step",
#                         population_at_date = x,
#                         probability_of_recent_infection = probability_of_recently_infected)
#
#
#
#
# y$prevalence_H - c(20/120, 12/92,24/84, 19/69)
#
#
# y$prevalence_R - c(0.5, 0.5, 10/24, 8/19)
#
#
# y$prevalence_T - matrix(c(c(100/120, 80/92, 60/84, 50/69),
#                           c(20/120, 10/92, 12/84, 10/69),
#                           c(NA,2/92, 8/84, 6/69),
#                           c(NA, NA,4/84,3/69)),
#                         byrow = T, nrow = 4)
#
#
#
# x <-  matrix(c(c(100, 80, 60, 50),
#                c(20,10,12,10),
#                c(NA,2,8,6),
#                c(NA, NA,4,3)),
#               byrow = T, nrow = 4)
#
#
#
# prev =   colSums(x[-1,], na.rm = T)/colSums(x, na.rm = T)
#
#
#
# prevR = colSums((x[-1,] * c(0.5,0.5, 0, 0, 0)), na.rm = T)/(colSums(x, na.rm = T) * prev)
#
#
# prevT = x/colSums(x, na.rm = T)
#
#
# colSums(prevT, na.rm =T)
#
#
#
#
# sapply(seq_along(1:nrow(x)), )





#' prevalences_calculation_compact
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
#'
#'
#'
#' @return a list of prevalences i.e. prevalence of HIV status, prevalence of recency,and prevelance of HIV age and tau .
#'
#' @export





prevalences_calculation_compact <- function(time_step,type,
                                    population_at_date,
                                    probability_of_recent_infection)
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


  overall_age_prevalence <- colSums(population_at_date[-1,], na.rm = T) / colSums(population_at_date,  na.rm = T)

  prevalence_R <- colSums(recent_among_positive, na.rm = TRUE) / overall_age_prevalence


  return(colSums(recent_among_positive, na.rm = TRUE))
}

