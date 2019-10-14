
#' extract_cohort_status_surveydate
#'
#' a function that calculates the probability of testing recently infected at a time since infection tau.
#'
#' @param survey_dates dates with which the survey are to be conducted
#' @param population dates with which the survey are to be conducted
#' @param date_of_birth the date of birth of the cohort.
#' @param max_age  maximum age of the birth cohort.
#' @param time_step the time or age difference between to consecutive times or ages the probability of surving infection or
#' death in the susceptible statemi.e it is uniform in all values supplied
#'
#'
#' @return a vector which denotes being the probability of testing recently infected  being recently infected fo
#'
#'
#'
#' @export







# extract_cohort_status_surveydate <- function(date_of_birth, max_age,
#                                              survey_dates, time_step,
#                                              population)
# {
#
#   # takes a population history and extracts the status of the population at specified survey dates
#   # returns #date_of_birth = min(times)rvey_dat
#
#
#   index <- round((survey_dates - date_of_birth)/time_step) + 1
#   index <- ifelse(index <= 0 | index > (max_age /time_step), next, index)
#   index <- index[which(is.na(index)== F)]
#
#   survey_status <- population[ ,index]
#
#   ages <- seq(from = date_of_birth, by = time_step, length.out = max_age/time_step)
#
#   return(list(survey_status = survey_status, age_at_survey = ages[index]))
# }
#



extract_cohort_status_surveydate <- function(date_of_birth, max_age,
                                             survey_dates, time_step,
                                             population)
{

  # takes a population history and extracts the status of the population at specified survey dates
  # returns #date_of_birth = min(times)

  valid_index <- numeric()

  for(surveydate_index in  seq_along(survey_dates)){


    index <- round((survey_dates[surveydate_index] - date_of_birth)/time_step) +1

    if (index <= 0|index > round(max(survey_dates) / time_step)) next

    valid_index[surveydate_index] <- index

  }

  ages <- seq(from = 0 , to = max_age, by = time_step)

if ( (length(valid_index) == 0) == 1){

    cohort = NA
  }else{

    cohort <- list(survey_status = population[ ,valid_index], age_at_survey = ages[valid_index])
  }

  return(cohort)
}





# extract_cohort_status_surveydate <- function(date_of_birth, max_age,
#                                              date_survey, time_step,
#                                              age_cohort_status)
#   {
#
#   # takes a population history and extracts the status of the population at specified survey dates
#   # returns
#
#
#   index <- round((date_survey- date_of_birth)/time_step)
#
#   surveydate_index <- ifelse(index <= 0 | index > max_age /time_step, NA, index)
#
#   surveydate_index <- stats::na.omit(surveydate_index)
#
#   survey_status <- age_cohort_status[ ,surveydate_index]
#
#   #return(list(actual_timeage, survey_status))
#
#
# }





