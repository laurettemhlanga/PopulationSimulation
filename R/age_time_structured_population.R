#' age_time_structured_population
#'
#' @param time_slice dates the surveys are to be conducted
#' @param max_age the maximum age of each cohort
#' @param min_birth_date minimum date of birth of the cohorts
#' @param max_birth_date minimum date of birth of the cohorts
#' @param time_step  the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param type the type of function to be used in estimating the probability of testing recently infected
#' @param probability_of_recent_infection function in use in calculating  probability of being recently infected
#' @param birth_rate bith rate at the the give date
#' @param base_mortality_function base mortality rate ata a given age and time
#' @param excess_mortality_function excess  mortality rate ata a given age and time
#' @param pmtct_birth_rate rate of babies/proportion of the population born HIV positive
#' @param incidence_function  Incidence rate at a given time
#'
#'
#' @export



age_time_structured_population <- function(time_slice, max_birth_date, min_birth_date,
                                           time_step, max_age, type,  birth_rate,
                                           probability_of_recent_infection,
                                           base_mortality_function, pmtct_birth_rate,
                                           incidence_function, excess_mortality_function
                                           ){

 # browser()

  birth_dates <- seq(from = min_birth_date, to = max_birth_date, time_step)
  #birth_dates <- seq(from = (date_of_birth + time_step/2), by = time_step, length.out = n_age_steps)


  populationprevalences <- data.frame(date_birth = numeric(), dates = numeric(), age = numeric(),
                                      prevalence_H = numeric(), prevalence_R = numeric())

  for (dob in seq_along(birth_dates)){

    population = birth_cohort_simulation(date_of_birth = birth_dates[dob], time_slice = time_slice,
                                time_step = time_step, max_age = max_age,  birth_rate = birth_rate,
                                base_mortality_function =  base_mortality_function,
                                pmtct_birth_rate = pmtct_birth_rate,
                                incidence_function =  incidence_function,
                                excess_mortality_function = excess_mortality_function,
                                compact = FALSE)

   if (any(is.na(population) == T)) next


    surveydates_prevalences <- prevalences_calculation(time_step = time_step, type = type,
                                                       population = population,
                                                       probability_of_recent_infection = probability_of_recent_infection)


    populationprevalence <- data.frame(date_birth = birth_dates[dob], dates = birth_dates[dob] + population$age_at_survey, total = population$total,
                                        age = population$age_at_survey, prevalence_H =  surveydates_prevalences$prevalence_H,
                                        prevalence_R = surveydates_prevalences$prevalence_R)

    populationprevalences <- rbind(populationprevalences, populationprevalence)


    }

  return(populationprevalences)
}









# survey_dates = 5; max_age = 5; min_birth_date = 0
# max_birth_date = 5; time_step = 1
# type = "weibull";  birth_rate = constant_birth_rate
# probability_of_recent_infection = probability_of_recently_infected
# excess_mortality = step_excess_mortality
# base_mortality = step_mortality
# pmtct_birth_rate = constant_pmtct_rate
# incidence = step_incidence



#
#
#
#
# Y <- populationstatus_survey[populationstatus_survey$dates == 2002,]
# plot(Y$prevalence_H, type = "l")
#
#
# #example for time_step 1/52, start_date 1982,
# #last_date = 2032, max_age = 50, survey dates  = c(1997, 1999, 2002, 2022, 2032) incidenceand excess mortality function Mahiane
# write.csv(populationprevalences, file = "populationprevalences")
# X = read.csv("/home/laurette/Desktop/Github/FilesPopulationSimulation/files/populationprevalences")
# View(populationstatus_survey)
#
#
# y = matrix(1:10, ncol = 1)
#
# y[-1,]
#
# times <- seq(from = date_of_birth, to = (date_of_birth + max_age), by = time_step)
# ages <- seq(from = 0, to = max_age, by = time_step)
#
# lookup <- data.frame(times = times, ages = ages)
#
# date_survey <- c(1997, 1999, 2000)
#
# y = sapply(date_survey, function(x) which(lookup$times == x))
#
#
# actual_timeage <- lookup[y,]
#
# survey_status = age_cohort_status[,y]
#
# list(actual_timeage, survey_status)
# survey
#
#
#
# survey1 <-  data.frame(age = actual_timeage$ages[1],
#                          prevalence = surveydates_prevalences$prevalence_H[1],
#                          prevalence_recency = surveydates_prevalences$prevalence_H[1])
#
#
#
# survey2 <-  data.frame(age = actual_timeage$ages[2],
#                        prevalence = surveydates_prevalences$prevalence_H[2],
#                        prevalence_recency = surveydates_prevalences$prevalence_H[2])
#
#
#
#
# survey3 <-  data.frame(age = actual_timeage$ages[3],
#                        prevalence = surveydates_prevalences$prevalence_H[3],
#                        prevalence_recency = surveydates_prevalences$prevalence_H[3])


