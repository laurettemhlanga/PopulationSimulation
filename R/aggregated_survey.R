#' aggregated_survey
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param survey_specs demographic specifications of the survey.
#' @param prevalencesdata output from the simulation with ages, HIV prevalence, and prevalence of recency.
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param reporting_bin reporting age bin in a time slice
#'
#' @return individual level data with age, HIV status, and HIV recency status.
#'
#'
#' @export




aggregated_survey <- function(survey_specs, prevalencesdata,
                              time_step, reporting_bin = 1
                              ){

prevalencesdata <-  prevalencesdata[order(prevalencesdata$age),] #cautionary measure
overallaggregated_data <- data.frame(age = numeric(), weighted_prevalence_H = numeric(),
                                     weighted_prevalence_R = numeric())

for (surveyindex in 1:nrow(survey_specs)){

    agespecifications <- survey_specs[surveyindex, ]
    agesinbin <- seq(agespecifications[,2],agespecifications[,3], reporting_bin)
    anchor <- ((agesinbin - min(prevalencesdata$age))/ time_step)+1

    aggregated_data <- data.frame(age = numeric(), weighted_prevalence_H = numeric(),
                                  weighted_prevalence_R = numeric())

    for (anchorindex in 1:length(anchor)){

      upper_indice <- anchor[anchorindex +1]
      lower_indice <- anchor[anchorindex]

      prevalencedata <- subset(prevalencesdata, prevalencesdata$age >= prevalencesdata$age[lower_indice] &
                                 prevalencesdata$age < prevalencesdata$age[upper_indice])

      totalin_bin <- sum(prevalencedata$total)

      prevalencedata$weights_prev <- prevalencedata$total / totalin_bin

      weighted_prevalence_H <- sum(prevalencedata$weights_prev * prevalencedata$prevalence_H)/sum(prevalencedata$weights_prev)

      prevalencedata$nu_infected <- prevalencedata$prevalence_H * prevalencedata$total

      prevalencedata$weights_rec <-  prevalencedata$nu_infected / sum( prevalencedata$nu_infected, na.rm = T)

      weighted_prevalence_R <- sum(prevalencedata$weights_rec * prevalencedata$prevalence_R, na.rm = T)/sum(prevalencedata$weights_rec)

      #weighted_prevalence_R <- sum(prevalencedata$weights * prevalencedata$prevalence_H * prevalencedata$prevalence_R, na.rm = T)/sum(prevalencedata$weights)

      weighted_data <- data.frame(age = prevalencesdata$age[lower_indice], totalin_bin = totalin_bin,
                                  weighted_prevalence_H = weighted_prevalence_H, weighted_prevalence_R = weighted_prevalence_R)

      aggregated_data <- rbind(aggregated_data, weighted_data)
    }
      aggregated_data <- aggregated_data[-nrow(aggregated_data),]
      aggregated_data$weights_prev <- aggregated_data$totalin_bin/sum(aggregated_data$totalin_bin, na.rm = T)

      aggregated_data$survey_samples <- stats::rmultinom(n = 1, size = agespecifications[,1], prob = aggregated_data$weights_prev)

      overallaggregated_data <- rbind(overallaggregated_data, aggregated_data)

  }

    overallaggregated_data$hiv_ptve <- overallaggregated_data$weighted_prevalence_H * overallaggregated_data$survey_samples
    overallaggregated_data$hiv_rec <- overallaggregated_data$weighted_prevalence_R * overallaggregated_data$hiv_ptve
    overallaggregated_data$hiv_nonrec <- overallaggregated_data$hiv_ptve - (overallaggregated_data$weighted_prevalence_R * overallaggregated_data$hiv_ptve)

    output_surveydata <- data.frame(age = overallaggregated_data$age,sample_size = overallaggregated_data$survey_samples,
                                     hivngtve_count = overallaggregated_data$survey_samples - overallaggregated_data$hiv_ptve,
                                     hivptve_count = overallaggregated_data$hiv_ptve, hiv_nonrec = overallaggregated_data$hiv_nonrec,
                                     hiv_rec = overallaggregated_data$hiv_rec)


  return(output_surveydata)
}







# prevalencesdata <- populationstatus_survey_1
# time_step <- 1/12
# prevalencesdata <-  prevalencesdata[order(prevalencesdata$age),]
# survey_specs <- data.frame(number = c(4000, 4000, 5000, 5000),
#                            agesmin = c(0, 5, 10, 15),
#                            agesmax = c(5, 10, 15, 20))
#
# agggregate_survey(survey_specs = survey_specs, prevalencesdata = prevalencesdata ,
#                               time_step = time_step)
