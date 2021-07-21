#' prevalences_calculation
#'
#' calculates the probability of being recently infected given that you already HIV positive.
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param cohort Status of the popuation in the cohort.
#' @param recency_function the recent function that describes the recency aspect
#' @param tau_cutoff switch parameter that enables cutting off/not the rececny prevelances calculated beyond big T
#' @param BigT the point to cut off for all points that are beyond recency classification mark in days
#' @paramrecency_function type of function to be utilised in the probability of infection function
#'
#'
#'
#' @return a list of prevalences i.e. prevalence of HIV status, prevalence of recency,and prevelance of HIV age and tau .
#'
#' @export


prevalences_calculation <- function(time_step, recency_function,
                                    tau_cutoff, BigT, cohort)
{
  # calculates the prevalences of recency, and
#browser()

  cohort_at_date <- cohort$survey_status

  n_tau_steps <- ((cohort$age_at_survey)/time_step)

  cutoff <- round(BigT/(time_step * 365.25))

  if(is.vector(cohort_at_date) == T){

    totalcohort <- sum(cohort_at_date,  na.rm = T)

    totalptve <- (totalcohort - cohort_at_date[1])

    overall_age_prevalence <-  totalptve / totalcohort

    tau_values <-  seq(from = time_step/2, by = time_step, length.out = n_tau_steps)
    #tau_values <-  seq(from = 0, by = time_step/2, length.out = n_tau_steps)


    recent_infections <- cohort_at_date[-1] * recency_function(tau_values)

    if (tau_cutoff == T){

      totalrec <- sum(recent_infections[1:cutoff], na.rm = TRUE)
      totalfrr <- 0

      }else{

      totalrec <- sum(recent_infections, na.rm = TRUE)
      totalfrr <- sum(recent_infections[cutoff:lenth(recent_infections)], na.rm = TRUE)
     }

    #global variables to ensure that it discards the values

    prevalence_R <- totalrec / (overall_age_prevalence * totalcohort)
    prevalence_frr <- ifelse(totalfrr == 0, 0, totalfrr / totalrec)

  }else{

    totalcohort <- colSums(cohort_at_date,  na.rm = T)

    totalptve <- (totalcohort - cohort_at_date[1,])

    overall_age_prevalence <-  totalptve / totalcohort

    recent_infections <- matrix(NA, ncol = ncol(cohort_at_date[-1, ]), nrow = nrow(cohort_at_date[-1, ]))

    for (timeslice_index in (1:ncol(cohort_at_date))){

      if(any(n_tau_steps == 0)){

        tau_values = 0

        }else{

          #tau_values <-  seq(from = 0, by = time_step/2, length.out = (n_tau_steps[timeslice_index]))
          tau_values <-  seq(from = time_step/2, by = time_step, length.out = (n_tau_steps[timeslice_index]))
      }

      recent_infections[ ,timeslice_index] <- cohort_at_date[-1, timeslice_index ] * recency_function(tau_values)


    }

    if (tau_cutoff == T & dim(recent_infections)[1] > cutoff){

    totalrec <- colSums(recent_infections[1:cutoff, ], na.rm = TRUE)
    totalfrr <- colSums(recent_infections[cutoff:dim(recent_infections)[1], ], na.rm = TRUE)
    }else{

      totalrec <- colSums(recent_infections, na.rm = TRUE)
      totalfrr <- 0
    }

  # }

    prevalence_R <- totalrec / (overall_age_prevalence * totalcohort)
    # prevalence_frr <- ifelse(totalfrr == 0, 0, totalfrr/totalrec)
  }

  return(data.frame(totalcohort = totalcohort,  totalngtve  = (totalcohort - totalptve),
                    totalrec =  totalrec, totalnonrec = (totalptve - round(totalrec, digits = 8)),
                    prevalence_H = overall_age_prevalence,
                    prevalence_R = prevalence_R,
                    prevalence_frr = prevalence_frr))
  }






# prevalences_calculation(cohort = populationstatus, time_step = 1/12,recency_function = "weibull")

# remove the dob, totalptve, totalngtve, totalrec
