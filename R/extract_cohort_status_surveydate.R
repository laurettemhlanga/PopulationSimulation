
#' extract_cohort_status_surveydate
#'
#' a function that calculates the probability of testing recently infected at a time since infection tau.
#'
#' @param age_cohort_status the cohort status for the length of the simulation
#' @param date_survey dates with which the survey are to be conducted
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




extract_cohort_status_surveydate <- function(date_of_birth, max_age,
                                             date_survey, time_step,
                                             age_cohort_status){

  # takes a population history and extracts the status of the population at specified survey dates
  # returns

  times <- seq(from = date_of_birth, to = (date_of_birth + max_age), by = time_step)
  ages <- seq(from = 0, to = max_age, by = time_step)
  lookup <- data.frame(times = times, ages = ages)

  index <- round((date_survey- min(times))/time_step) + 1
  surveydate_index <- index  #ifelse(index <= 0 | index > length(times), NA, index)
  surveydate_index <- stats::na.omit(surveydate_index)

  actual_timeage <- lookup[surveydate_index,]
  survey_status <- age_cohort_status[ ,surveydate_index]

  return(list(actual_timeage, survey_status))
}
