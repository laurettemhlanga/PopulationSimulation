#' prevalences_calculation
#'
#' calculates the probability of being recently infected given that you already HIV positive.
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param cohort Status of the popuation in the cohort.
#' @param probability_of_recent_infection a function that takes in possible  time since infection in years and calculates the probability
#' of being infected for that amount of years
#' @param n_age_steps type of function to be utilised in the probability of infection function
#' @param type type of function to be utilised in the probability of infection function
#'
#'
#'
#' @return a list of prevalences i.e. prevalence of HIV status, prevalence of recency,and prevelance of HIV age and tau .
#'
#' @export


prevalences_calculation <- function(time_step ,type ,
                                    cohort, n_age_steps,
                                    probability_of_recent_infection)
{
  # calculates the prevalences of recency, and
  # hiv status at a particular time age

  cohort_at_date <- cohort$survey_status

  n_age_steps <- ((cohort$age_at_survey)/time_step)

  if(is.vector(cohort_at_date) == T){

    totalcohort <- sum(cohort_at_date,  na.rm = T)

    time_in_years <-  seq(from = time_step/2, by = time_step, length.out = n_age_steps)

    overall_age_prevalence <- (totalcohort - cohort_at_date[1]) / totalcohort

    recent_infections <- cohort_at_date[-1] * probability_of_recent_infection(time_in_years, type)

    prevalence_R <- sum(recent_infections, na.rm = TRUE) / (overall_age_prevalence * totalcohort)

  }else {

    #time_in_years <- seq(from = 0, to = (nrow(cohort_at_date[-1, ]) * time_step), by = time_step)

    totalcohort <- colSums(cohort_at_date,  na.rm = T)

    overall_age_prevalence <-  (totalcohort - cohort_at_date[1,]) / totalcohort

    recent_infections <- matrix(NA, ncol = ncol(cohort_at_date[-1, ]), nrow = nrow(cohort_at_date[-1, ]))

    for (age_index in (1:ncol(cohort_at_date))){

      time_in_years <-  ifelse(n_age_steps[age_index] == 0, 0,seq(from = time_step/2, by = time_step, length.out = n_age_steps[age_index]))

     # time_in_years <-  seq(from = time_step/2, by = time_step, length.out = n_age_steps[age_index])

      recent_infections[ ,age_index] <- cohort_at_date[-1, age_index ] * probability_of_recent_infection(time_in_years, type)

    }
    prevalence_R <- colSums(recent_infections, na.rm = TRUE) / (overall_age_prevalence * totalcohort)
  }

  return(data.frame(totalcohort = totalcohort,
                     prevalence_H = overall_age_prevalence,
                    prevalence_R = prevalence_R ))
  }



