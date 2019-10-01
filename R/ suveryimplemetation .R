#' suveryimplemetation
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param  survey_specs  array containing excess mortality rates for a given time space.
#' @param prevalences_specs containing background rates for a given time space.
#'
#'
#' @return returns an array of dimensions time, age and time since infection - tau which is the probability of surviving the given time_step given that you are infected.
#'
#'
#' @export




 suveryimplemetation <- function(survey_status =   survey_specs,
                                 prevalencesdata =  prevalences_specs,
                                 time_step = 1)

   {

   overallcombinedsurvey <- data.frame()

  for (surveyindex in 1:dim(survey_status)[1]){

      agespecifications <- survey_status[surveyindex, ]
      agebin <- seq(agespecifications[,2],(agespecifications[,3]- time_step), time_step)

      age = sample(agespecifications[,1], x = agebin , prob = rep(1/length(agebin), length(agebin)), replace = T)


      ages = sort(unique(age))

      combinedsurveydata <- data.frame()

      for (ageindex in seq_along(ages)){

        surveydata <-  data.frame(ages = age[which(age == ages[ageindex])])
        simulinfor  <-  prevalencesdata[which(prevalencesdata$ages == ages[ageindex]),]
        surveydata$hivstatus <- sample(nrow(surveydata), x = c(1,0), prob = c(simulinfor[,2], (1 - simulinfor[,2])), replace = T)
        surveydata$recencystatus <- ifelse(surveydata$hivstatus == 1, sample(nrow(surveydata), x = c(1,0), prob = c(simulinfor[,3], (1 - simulinfor[,3])), replace = T), NA)

        combinedsurveydata <- rbind(combinedsurveydata, surveydata)
      }
      overallcombinedsurvey <- rbind(overallcombinedsurvey , combinedsurveydata )
  }

  return(overallcombinedsurvey)
 }


 # suveryimplemetation()
 # survey_specs <- data.frame(number = c(1000, 400, 500, 300, 300),
 #                             agesmin = c(0, 5, 10, 15, 20),
 #                             agesmax = c(5, 10, 15, 20, 25))
 #
 # prevalences_specs<- data.frame(ages = 0:24,
 #                               hivstatus = rep(population_prevalence, 5),
 #                               recenceystatus = rep(recent_positive, 5))
