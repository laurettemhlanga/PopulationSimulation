#' smoothing_surveydata
#'
#' a function that returns an array of probabilities of surviving in the infected state for each age, time and time since infection.
#'
#' @param age_0 output from the simulation with ages, HIV prevalence, and prevalence of recency.
#' @param time_0 reporting age bin in a time slice
#' @param distance the time difference btween two consecutive times or ages
#' @param Data the time difference btween two consecutive times or ages
#'
#' @return individual level data with age, HIV status, and HIV recency status.
#'
#'
#' @export

#
# surveydata1 <- data.frame(ages = 0:25, times = rep(5, 26), samplesize = rep(1000, 26),
#                          hivcounts = sample(50:100, 26, replace = T),
#                          hivreccount = sample(0:35, 26, replace = T))
#
#
# surveydata2 <- data.frame(ages = 5:30, times = rep(10, 26), samplesize = rep(1000, 26),
#                          hivcounts = sample(100:150, 26, replace = T),
#                          hivreccount = sample(0:35, 26, replace = T))
# surveydata <- list(surveydata1, surveydata2)



smoothing_surveydata  <- function(age_0, time_0,
                                  distance, Data )
  {
  # attempt at smoothing the data all the points within the specified distance have an equal
  # weight to determining the estimate of

  analysisdata <- data.frame(dates = numeric(), samplesize = numeric(), age = numeric(),
                             hivcounts = numeric(), hivrecency = numeric())

  for (surveys in 1:length(Data)){

    subsettingdata <- Data[[surveys]]

    if(time_0 > subsettingdata$times[1]){

      radius <- ceiling(sqrt(distance ^ 2 - (time_0 - subsettingdata$times[1]) ^ 2))

      age <-  age_0 - (time_0 - subsettingdata $times[1])
    }else{

      radius <- floor(sqrt(distance ^ 2 - ( subsettingdata$times[1] - time_0) ^ 2))

      age <- age_0 + (subsettingdata $times[1] - time_0)
    }

    index <- subsettingdata$age >= (age_0 - radius) & subsettingdata$age <= (age_0 + radius)
    subsettingdata  <- subsettingdata[index,]

    samplesize <- mean(subsettingdata$survey_sample_size)
    hivcounts <-  mean(subsettingdata$hivngtvecount)
    hivreccount <-  mean(subsettingdata$hivreccount)

    datasummary <- data.frame(dates = subsettingdata$times[1], age = age, samplesize = samplesize,
                              hivcounts = hivcounts, hivreccount = hivreccount)
    analysisdata <- rbind(analysisdata, datasummary)
  }

  fit_glmprevH <- glm2::glm2(formula = cbind(round(hivcounts), round(samplesize- hivcounts)) ~ 1 + I(dates),
                  data = analysisdata, family = stats::binomial(link = "identity"))


  coeff_glmprevH <- summary(fit_glmprevH)
  prevslope_est <- coeff_glmprevH$coefficients[2,1]
  prevslope_std <- coeff_glmprevH$coefficients[2,2]

  predicttime <- data.frame(dates = c(time_0))
  prev_predictions <- stats::predict(fit_glmprevH, newdata = predicttime, se.fit = TRUE)

  fit_glmprevR <- glm2::glm2(formula = cbind(round(hivreccount), round(hivcounts - hivreccount)) ~ 1 + I(dates),
                       data = analysisdata, family = stats::binomial(link = "identity"))

  prevrec_predictions <- stats::predict(fit_glmprevR, newdata = predicttime, se.fit = TRUE)

  param_inccalculation <- data.frame(age_0 = age_0, time_0 = time_0,
                                     prevalenceH = prev_predictions$fit, prevalenceR = prevrec_predictions$fit,
                                     prevalenceH_se = prev_predictions$se.fit, prevalenceR_se = prevrec_predictions$se.fit,
                                     prevalenceH_slope = prevslope_est, prevalenceH_slopese = prevslope_std)
  return(param_inccalculation)
}
