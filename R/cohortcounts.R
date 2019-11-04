#' cohortcounts
#'
#' calculates the probability of being recently infected given that you already HIV positive.
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param cohort cohort history at survey timeslices.
#' @param time_slice times the cross sectional surveys are conducted
#' @param date_of_birth date of birth of the cohort
#'
#'
#'
#' @return a long data frame with ages and population counts at a specified time.
#'
#' @export


cohortcounts <- function(cohort, date_of_birth,
                         time_slice
){

 cohortcounts <-  data.frame(dob = numeric(), times = numeric(),
                                  ages = numeric(), counts = numeric())
  dimpop = dim(cohort$survey_status)

  for (timesliceindex in 1:dimpop[2]){

   cohortcount <-  data.frame(dob = rep(date_of_birth ,dimpop[1]), times =  time_slice[timesliceindex],
                                   ages = cohort$age_at_survey[timesliceindex],
                                   counts = cohort$survey_status[,timesliceindex])

   cohortcounts <-  rbind(cohortcounts,cohortcount)
  }
  return(cohortcounts)
}


#cohortcounts(cohort = y, time_slice = c(2,3,4))
