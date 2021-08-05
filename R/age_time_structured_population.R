#' age_time_structured_population
#'
#' @param time_slice dates the surveys are to be conducted
#' @param max_age the maximum age of each cohort
#' @param min_birth_date minimum date of birth of the cohorts
#' @param max_birth_date minimum date of birth of the cohorts
#' @param time_step  the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param reporting_bin the size of the reporting bin if NA is specified the calculation defaults
#' to calculations based on the time step
#' @param tau_cutoff switch parameter that enables cutting off/not the  prevelances of rececny calculated beyond big T
#' @param BigT the point to cut off of all points that are beyond recency classification mark
#' @param recency_function the type of function to be used in estimating the probability of testing recently infected
#' @param birth_rate bith rate at the the give date
#' @param base_mortality_function base mortality rate ata a given age and time
#' @param excess_mortality_function excess  mortality rate ata a given age and time
#' @param pmtct_birth_rate rate of babies/proportion of the population born HIV positive
#' @param incidence_function  Incidence rate at a given time
#'
#'
#' @export

age_time_structured_population <- function(time_slice, max_birth_date, min_birth_date,
                                           time_step, max_age, recency_function,  birth_rate,
                                           tau_cutoff, BigT = BigT,
                                           base_mortality_function, pmtct_birth_rate,
                                           incidence_function, excess_mortality_function,
                                           reporting_bin = NA){
  if (is.na(reporting_bin) == T){

    reporting_bin  = time_step
    birth_dates <- seq(from = min_birth_date + (time_step/2), to = max_birth_date, by = time_step)

  } else{
    #adding the 0.5 to conduct the survey at mid points.

    birth_dates <- seq(from = min_birth_date + (reporting_bin/2), to = max_birth_date #+ (reporting_bin/2)
                       , by = reporting_bin)
  }

  populationprevalences <- data.frame(# date_birth = numeric(),
    dates = numeric(), age = numeric(), total = numeric(),
    # hivngtve = numeric(), hivptve_rec = numeric(), hivptve_nonrec = numeric(),
    prevalence_H = numeric(), prevalence_R = numeric(),
    prevalence_frr = numeric())

  # taupopulation <- list()

  for (dob in seq_along(birth_dates)){

    population = birth_cohort_simulation(date_of_birth = birth_dates[dob], time_slice = time_slice,
                                         time_step = time_step, max_age = max_age,  birth_rate = birth_rate,
                                         base_mortality_function =  base_mortality_function,
                                         pmtct_birth_rate = pmtct_birth_rate,
                                         incidence_function =  incidence_function,
                                         excess_mortality_function = excess_mortality_function,
                                         detailed = FALSE)
    if ((is.na(population) == T)) next
    # this line is required to ensure that the if the population(surveystatus
    # and age at survey) is NA the loop skips to the next iteration
    # and not break..............

    surveydates_prevalences <- prevalences_calculation(time_step = time_step, recency_function = recency_function,
                                                       tau_cutoff = tau_cutoff, BigT = BigT,
                                                       cohort = population)

    mortalitydata <-  estimate_excessmortality(populationdistribution = population,
                                               birth_dates = birth_dates[dob],
                                               time_step = time_step,
                                               base_mortality = base_mortality_function,
                                               excess_mortality = excess_mortality_function)

    populationprevalence <- data.frame(dates = birth_dates[dob] + population$age_at_survey,
                                       age = population$age_at_survey,
                                       total = surveydates_prevalences$totalcohort,
                                       prevalence_H =  surveydates_prevalences$prevalence_H,
                                       prevalence_R = surveydates_prevalences$prevalence_R,
                                       frr = surveydates_prevalences$prevalence_frr,
                                       mortality = mortalitydata$excessmortality)

    populationprevalences <- rbind(populationprevalences, populationprevalence)
  }

  populationprevalences <- populationprevalences[populationprevalences$age <= max_age, ]
return(populationprevalences)
}



#
# age_time_structured_population <- function(time_slice, max_birth_date, min_birth_date,
#                                            time_step, max_age, recency_function,  birth_rate,
#                                            base_mortality_function, pmtct_birth_rate,
#                                            incidence_function, excess_mortality_function,
#                                            reporting_bin = NA){
#   if (is.na(reporting_bin) == T){
#
#       reporting_bin  = time_step
#       birth_dates <- seq(from = min_birth_date + (time_step/2), to = max_birth_date, by = time_step)
#
#     } else{
#       #adding the 0.5 to conduct the survey at mid points.
#
#       birth_dates <- seq(from = min_birth_date + (reporting_bin/2), to = max_birth_date #+ (reporting_bin/2)
#                          , by = reporting_bin)
#     }
#
#   populationprevalences <- data.frame(# date_birth = numeric(),
#                                       dates = numeric(), age = numeric(), total = numeric(),
#                                       # hivngtve = numeric(), hivptve_rec = numeric(), hivptve_nonrec = numeric(),
#                                       prevalence_H = numeric(), prevalence_R = numeric())
#
#   taupopulation <- list()
#
#   for (dob in seq_along(birth_dates)){
#
#     population = birth_cohort_simulation(date_of_birth = birth_dates[dob], time_slice = time_slice,
#                                          time_step = time_step, max_age = max_age,  birth_rate = birth_rate,
#                                          base_mortality_function =  base_mortality_function,
#                                          pmtct_birth_rate = pmtct_birth_rate,
#                                          incidence_function =  incidence_function,
#                                          excess_mortality_function = excess_mortality_function,
#                                          detailed = FALSE)
#   if ((is.na(population) == T)) next
#     # this line is required to ensure that the if the population(surveystatus
#     # and age at survey) is NA the loop skips to the next iteration
#     # and not break..............
#
#     surveydates_prevalences <- prevalences_calculation(time_step = time_step, recency_function = recency_function,
#                                                        cohort = population)
#
#     taupopulation[[dob]] <-  extactcohorttau_distribution(birth_date = birth_dates[dob],
#                                                           reporting_bin = reporting_bin,
#                                                           population = population)
#     # discuss how we want to extract and save the output
#
#     populationprevalence <- data.frame(# date_birth = birth_dates[dob],
#                                        dates = birth_dates[dob] + population$age_at_survey,
#                                        age = population$age_at_survey,  total = surveydates_prevalences$totalcohort,
#                                        # hivngtve = surveydates_prevalences$totalngtve, hivptve_rec = surveydates_prevalences$totalrec,
#                                        # hivptve_nonrec = round(surveydates_prevalences$totalnonrec, digits = 9),
#                                        prevalence_H =  surveydates_prevalences$prevalence_H,
#                                        prevalence_R = surveydates_prevalences$prevalence_R)
#    populationprevalences <- rbind(populationprevalences, populationprevalence)
#
#    }
#
#    mortality <- estimate_excessmortality(populationdistribution = taupopulation,
#                                          reporting_bin = reporting_bin,
#                                          base_mortality = base_mortality_function,
#                                          excess_mortality = excess_mortality_function)[,3]
#
#    populationprevalences <- cbind(populationprevalences, mortality)
#
#    populationprevalences <- populationprevalences[populationprevalences$age <= max_age, ]
#
#   return(populationprevalences)
# }




# download_input_function
#
# @param url the link to the online script with all the global functions


# cacheEnv <- new.env()
#
# download_input_function <- function(url){
#   if (exists(url, envir = cacheEnv)){
#
#     return(get(url, envir=cacheEnv))
#
#     file <- content(GET(url))
#     assign(url, file, envir=cacheEnv)
#
#     file
#     }
#   }


# download_input_function("https://github.com/laurettemhlanga/utility/blob/master/global_params_template.R")
