#' prevalences_calculation
#'
#' calculates the probability of being recently infected given that you already HIV positive.
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param cohort Status of the popuation in the cohort.
#' @param recency_type type of function to be utilised in the probability of infection function
#'
#'
#'
#' @return a list of prevalences i.e. prevalence of HIV status, prevalence of recency,and prevelance of HIV age and tau .
#'
#' @export


prevalences_calculation <- function(time_step ,recency_type ,
                                    cohort)
{
  # calculates the prevalences of recency, and
#browser()

  cohort_at_date <- cohort$survey_status

  n_tau_steps <- ((cohort$age_at_survey)/time_step) + 1

  if(is.vector(cohort_at_date) == T){

    totalcohort <- sum(cohort_at_date,  na.rm = T)

    overall_age_prevalence <- (totalcohort - cohort_at_date[1]) / totalcohort

    tau_values <-  seq(from = time_step/2, by = time_step, length.out = n_tau_steps)


    recent_infections <- cohort_at_date[-1] * probability_of_recently_infected(tau_values, recency_type)

    prevalence_R <- sum(recent_infections, na.rm = TRUE) / (overall_age_prevalence * totalcohort)

  }else{

    totalcohort <- colSums(cohort_at_date,  na.rm = T)

    overall_age_prevalence <-  (totalcohort - cohort_at_date[1,]) / totalcohort

    recent_infections <- matrix(NA, ncol = ncol(cohort_at_date[-1, ]), nrow = nrow(cohort_at_date[-1, ]))

    for (timeslice_index in (1:ncol(cohort_at_date))){

      if( n_tau_steps == 0){

        tau_values = 0

        }else{

        tau_values <-  seq(from = time_step/2, by = time_step, length.out = n_tau_steps[timeslice_index])
      }

      recent_infections[ ,timeslice_index] <- cohort_at_date[-1, timeslice_index ] * probability_of_recently_infected(tau_values, recency_type)

    }
    prevalence_R <- colSums(recent_infections, na.rm = TRUE) / (overall_age_prevalence * totalcohort)

  }

  return(data.frame(totalcohort = totalcohort,
                    prevalence_H = overall_age_prevalence,
                    prevalence_R = prevalence_R ))
  }





# prevalences_calculation(cohort = populationstatus, time_step = 1/12, recency_type = "weibull")


