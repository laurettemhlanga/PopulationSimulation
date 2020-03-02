#' prevalences_calculation
#'
#' calculates the probability of being recently infected given that you already HIV positive.
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param cohort Status of the popuation in the cohort.
#' @param recency_function the recent function that describes the recency aspect
#' @paramrecency_function type of function to be utilised in the probability of infection function
#'
#'
#'
#' @return a list of prevalences i.e. prevalence of HIV status, prevalence of recency,and prevelance of HIV age and tau .
#'
#' @export


prevalences_calculation <- function(time_step, recency_function,
                                    cohort)
{
  # calculates the prevalences of recency, and
#browser()

  cohort_at_date <- cohort$survey_status

  n_tau_steps <- ((cohort$age_at_survey)/time_step)

  if(is.vector(cohort_at_date) == T){

    totalcohort <- sum(cohort_at_date,  na.rm = T)

    totalptve <- (totalcohort - cohort_at_date[1])

    overall_age_prevalence <-  totalptve / totalcohort

    tau_values <-  seq(from = time_step/2, by = time_step, length.out = n_tau_steps)

    recent_infections <- cohort_at_date[-1] * recency_function(tau_values)

    totalrec <- sum(recent_infections, na.rm = TRUE)

    prevalence_R <- totalrec / (overall_age_prevalence * totalcohort)

  }else{

    totalcohort <- colSums(cohort_at_date,  na.rm = T)

    totalptve <- (totalcohort - cohort_at_date[1,])

    overall_age_prevalence <-  totalptve / totalcohort

    recent_infections <- matrix(NA, ncol = ncol(cohort_at_date[-1, ]), nrow = nrow(cohort_at_date[-1, ]))

    for (timeslice_index in (1:ncol(cohort_at_date))){

      if(any(n_tau_steps == 0)){

        tau_values = 0

        }else{

        tau_values <-  seq(from = time_step/2, by = time_step, length.out = (n_tau_steps[timeslice_index]))
      }

      recent_infections[ ,timeslice_index] <- cohort_at_date[-1, timeslice_index ] * recency_function(tau_values)

    }
    totalrec <- colSums(recent_infections, na.rm = TRUE)
    prevalence_R <- totalrec / (overall_age_prevalence * totalcohort)

  }

  return(data.frame(totalcohort = totalcohort,  totalngtve  = (totalcohort - totalptve),
                    totalrec =  totalrec, totalnonrec = (totalptve - round(totalrec, digits = 8)),
                    prevalence_H = overall_age_prevalence,
                    prevalence_R = prevalence_R ))
  }





# prevalences_calculation(cohort = populationstatus, time_step = 1/12,recency_function = "weibull")

# remove the dob, totalptve, totalngtve, totalrec
