#' birth_cohort_simulation
#'
#' A wrapper function that returns a list of the the susceptible and infected population
#'
#' @param population the minimum date of birth for the birth cohorts
#' @param birth_date the date of birth of the cohort

#'
#'
#' @return a matrix of column length max_age and row length list_of_birth_times,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export
#'
#'

extactcohorttau_distribution <- function(birth_date,
                                         population)
  {
    populationtau <- population$survey_status
    ages <- population$age_at_survey

    if (is.vector(populationtau)){
      sub_divide <- (length(populationtau[-1]) - 2)/max(ages)
      population_dist <- populationtau[-1]
      splitcohort <- split(populationtau[-1], ceiling(seq_along(populationtau[-1])/sub_divide))
      populationdist <- sapply(splitcohort, sum, na.rm = TRUE)
      # corresponding_ages <-  data.frame( dates = birth_date + ages,
      #                                    age = ages)
      }else{
      sub_divide <- (nrow(populationtau[-1,]) - 1)/max(ages)
      populationdist <- matrix(NA, nrow = max(ceiling(ages)), ncol = length(ages) )

      # corresponding_ages <-  data.frame( dates = numeric(),
      #                                    age = numeric())

      for (columnindex in 1:ncol(populationtau)){
        population_dist <- populationtau[,columnindex][-1]
        splitcohort <- split(populationtau[,columnindex][-1], ceiling(seq_along(populationtau[,columnindex][-1])/sub_divide))
        populationdist[, columnindex] <- sapply(splitcohort, sum, na.rm = TRUE)

        }
      # corresponding_ages <-  rbind(corresponding_ages, corresponding_age)
      }
    corresponding_age <- data.frame( dates = birth_date + ages, age = ages)
    return(list(populationdist,
                corresponding_age))
  }




#' calculate_excessmortality
#'
#' A wrapper function that returns a list of the the susceptible and infected population
#'
#' @param base_mortality natural causes of death
#' @param excess_mortality excess mortality resulting from the infection
#' @param populationdistribution the cohort distribution of taus since infection
#' @param reporting_bin the size of the reporting bin default is 1
#'
#'
#' @return a data frame of ages, times and the corresponding excess mortality rate
#'
#'
#'
#' @export
#'

 estimate_excessmortality <- function(populationdistribution,
                                      excess_mortality, base_mortality,
                                      reporting_bin) {

   aggregatedexcess_mort <- data.frame(dates = numeric(), ages = numeric(),
                                    excessmortality = numeric())

   for (ageindex in seq_along(populationdistribution)){

    tau_populationdist = populationdistribution[[ageindex]]

    excessmortalitydata <- data.frame(dates = numeric(), ages = numeric(),
                                      excessmortality = numeric())
    if (is.vector(populationdistribution[[ageindex]][[1]])){

      tau_distribution <- tau_populationdist[[1]]
      datesages <-  tau_populationdist[[2]]
      ages =  datesages$age
      times = datesages$dates

      mortality <- base_mortality(ages =  ages, times = times) +
        excess_mortality(ages = ages, times = times,
                         times_since_i = seq(reporting_bin/2, datesages$age, reporting_bin))

      excessmortality <- (sum(mortality * tau_distribution ,na.rm = T)/sum(tau_distribution,na.rm = T)) -
        base_mortality(ages =  ages, times = times)

      excessmortality_data <- data.frame(dates = times, ages = ages,
                                         excessmortality = excessmortality)

      excessmortalitydata <- rbind(excessmortalitydata, excessmortality_data)
    }else{

      for (columnindex in 1:ncol(tau_populationdist[[1]])){

        tau_distribution <- tau_populationdist[[1]][, columnindex]
        datesages <-  tau_populationdist[[2]]
        ages =  datesages$age[columnindex]
        times = datesages$dates[columnindex]

        mortality <- base_mortality(ages =  ages, times = times) +
          excess_mortality(ages = ages, times = times,
                           times_since_i = seq(reporting_bin/2, datesages$age[columnindex], reporting_bin))

        excessmortality <- round(((sum(mortality * tau_distribution ,na.rm = T)/sum(tau_distribution,na.rm = T)) -
          base_mortality(ages =  ages, times = times)), digits = 10)

        excessmortality_data <- data.frame(dates = times, ages = ages,
                                          excessmortality = excessmortality)

        excessmortalitydata <- rbind(excessmortalitydata, excessmortality_data)

      }
    }
    aggregatedexcess_mort <- rbind(aggregatedexcess_mort, excessmortalitydata)
  }
   return(aggregatedexcess_mort)

}

#  mortalitydata <- estimate_excessmortality(populationdistribution = simulationprevalencedata$tau_populationdist,
#                            reporting_bin = 1, base_mortality = step_mortality,
#                            excess_mortality = excess_mahiane)
#
# View( mortalitydata)

