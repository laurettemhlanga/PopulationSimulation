#' suveryimplemetation
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param survey_status demographic specifications of the survey.
#' @param prevalencesdata output from the simulation with ages, HIV prevalence, and prevalence of recency.
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#'
#'
#' @return individual level data with age, HIV status, and HIV recency status.
#'
#'
#' @export





suveryimplemetation <- function(survey_status,
                                prevalencesdata,
                                time_step)
  # takes the survey specifications, prevalence data from the simulation, and time step,
  # reads a row  at a time, creates a sequence of ages that corresponds to the ages prevalence data
  # samples the ages depending on the sample total required. Orders prevalence data by age and also sorts
  # the sampled ages and etakes one of each sampled value. for all simulated ages HIV status and HIV recency status
  # Recency status

{

  overallcombinedsurvey <- data.frame()

  for (surveyindex in 1:dim(survey_status)[1]){

    agespecifications <- survey_status[surveyindex, ]
    agebin <- seq(agespecifications[,2],(agespecifications[,3]- time_step), time_step)

    age <-  sample(agespecifications[,1], x = agebin , prob = rep(1/length(agebin), length(agebin)), replace = T)

    prevalencesdata <-  prevalencesdata[order(prevalencesdata$age),]

    ages <- sort(unique(age))
    combinedsurveydata <- data.frame()

    for (ageindex in seq_along(ages)){

      age_anchor <-  ((ages[ageindex] - min(prevalencesdata$age))/time_step) + 1

      surveydata <-  data.frame(ages = age[which(age == ages[ageindex])])
      simulinfor  <-  prevalencesdata[age_anchor,]

      surveydata$hivstatus <- sample(nrow(surveydata), x = c(1,0), prob = c(simulinfor[,4], (1 - simulinfor[,4])), replace = T)
      surveydata$recencystatus <- ifelse(surveydata$hivstatus == 1,
                                         sample(nrow(surveydata), x = c(1,0), prob = c(simulinfor[,5], (1 - simulinfor[,5])), replace = T),
                                         NA)
     combinedsurveydata <- rbind(combinedsurveydata, surveydata)
    }

    overallcombinedsurvey <- rbind(overallcombinedsurvey , combinedsurveydata )
  }

  return(overallcombinedsurvey)
}







 # suveryimplemetation <- function(survey_status,
 #                                 prevalencesdata,
 #                                 time_step)
 #
 #   {
 #
 #   overallcombinedsurvey <- data.frame()
 #
 #   for (surveyindex in 1:dim(survey_status)[1]){
 #
 #      agespecifications <- survey_status[surveyindex, ]
 #      agebin <- seq(agespecifications[,2],(agespecifications[,3]- time_step), time_step)
 #
 #      age = sample(agespecifications[,1], x = agebin , prob = rep(1/length(agebin), length(agebin)), replace = T)
 #
 #
 #      ages = sort(unique(age))
 #
 #      combinedsurveydata <- data.frame()
 #
 #      for (ageindex in seq_along(ages)){
 #
 #        surveydata <-  data.frame(ages = age[which(age == ages[ageindex])])
 #        simulinfor  <-  prevalencesdata[which(round(prevalencesdata$ages, 1) == ages[ageindex]),]
 #        surveydata$hivstatus <- sample(nrow(surveydata), x = c(1,0), prob = c(simulinfor[,2], (1 - simulinfor[,2])), replace = T)
 #        surveydata$recencystatus <- ifelse(surveydata$hivstatus == 1, sample(nrow(surveydata), x = c(1,0), prob = c(simulinfor[,3], (1 - simulinfor[,3])), replace = T), NA)
 #
 #        combinedsurveydata <- rbind(combinedsurveydata, surveydata)
 #      }
 #      overallcombinedsurvey <- rbind(overallcombinedsurvey , combinedsurveydata )
 #  }
 #
 #  return(overallcombinedsurvey)
 # }
 #


 # suveryimplemetation(survey_status = ,
 #                     prevalencesdata = populationstatus_survey,
 #                     time_step = )
 #

#
#  population_prevalence = 0.03
#  recent_positive = 0.2
#
#
#  survey_specs <- data.frame(number = c(1000, 400, 500, 300, 300),
#                              agesmin = c(0, 5, 10, 15, 20),
#                              agesmax = c(5, 10, 15, 20, 25))
#
#
#  prevelance_specs =  populationstatus_survey
#
#  prevalences_specs <- data.frame(ages = 0:24,
#                                hivstatus = rep(population_prevalence, 5),
#                                recenceystatus = rep(recent_positive, 5))
#
#  ind <-  suveryimplemetation()
#  sum(ind$hivstatus, na.rm = T)
#  sum(ind$recencystatus, na.rm = T)

