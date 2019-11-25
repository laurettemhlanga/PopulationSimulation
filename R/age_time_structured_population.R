#' age_time_structured_population
#'
#' @param time_slice dates the surveys are to be conducted
#' @param max_age the maximum age of each cohort
#' @param min_birth_date minimum date of birth of the cohorts
#' @param max_birth_date minimum date of birth of the cohorts
#' @param time_step  the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param reporting_bin the size of the reporting bin if NA is specified the calculation defaults
#' to calculations based on the time step
#' @param recency_type the type of function to be used in estimating the probability of testing recently infected
#' @param birth_rate bith rate at the the give date
#' @param base_mortality_function base mortality rate ata a given age and time
#' @param excess_mortality_function excess  mortality rate ata a given age and time
#' @param pmtct_birth_rate rate of babies/proportion of the population born HIV positive
#' @param incidence_function  Incidence rate at a given time
#'
#'
#' @export




age_time_structured_population <- function(time_slice, max_birth_date, min_birth_date,
                                           time_step, max_age, recency_type,  birth_rate,
                                           base_mortality_function, pmtct_birth_rate,
                                           incidence_function, excess_mortality_function,
                                           reporting_bin = NA){
  if (is.na(reporting_bin) == T){

      birth_dates <- seq(from = min_birth_date, to = max_birth_date, by = time_step)

    } else{

      birth_dates <- seq(from = min_birth_date, to = max_birth_date, by = reporting_bin)
    }

  populationprevalences <- data.frame(date_birth = numeric(), dates = numeric(), age = numeric(), total = numeric(),
                                      hivngtve = numeric(), hivptve_rec = numeric(), hivptve_nonrec = numeric(),
                                      prevalence_H = numeric(), prevalence_R = numeric())

  for (dob in seq_along(birth_dates)){

    population = birth_cohort_simulation(date_of_birth = birth_dates[dob], time_slice = time_slice,
                                         time_step = time_step, max_age = max_age,  birth_rate = birth_rate,
                                         base_mortality_function =  base_mortality_function,
                                         pmtct_birth_rate = pmtct_birth_rate,
                                         incidence_function =  incidence_function,
                                         excess_mortality_function = excess_mortality_function,
                                         detailed = FALSE)

    if (any(is.na(population) == T)) next

    surveydates_prevalences <- prevalences_calculation(time_step = time_step, recency_type = recency_type,
                                                       cohort = population)


    populationprevalence <- data.frame(date_birth = birth_dates[dob], dates = birth_dates[dob] + population$age_at_survey,
                                       age = population$age_at_survey,  total = surveydates_prevalences$totalcohort,
                                       hivngtve = surveydates_prevalences$totalngtve, hivptve_rec = surveydates_prevalences$totalrec,
                                       hivptve_nonrec = round(surveydates_prevalences$totalnonrec, digits = 9),
                                       prevalence_H =  surveydates_prevalences$prevalence_H,
                                       prevalence_R = surveydates_prevalences$prevalence_R)

    populationprevalences <- rbind(populationprevalences, populationprevalence)


  }

  return(populationprevalences)
}



