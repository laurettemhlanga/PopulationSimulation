#' collapse_populationarray
#'
#' A wrapper function that returns a list of the the susceptible and infected population
#'
#' @param data_set the minimum date of birth for the birth cohorts
#' @param list_of_birth_times the maximum date of birth for the birth cohorts
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param max_age maximum age attained by each birth cohort
#' @param counts takes in either true or false if true the function returns population counts, otherwise prevalence.
#'
#' @returnThe, by DOB of the cohorts, age, current date, ,
#' Values stored in the matrix are numeric double, from 0-1, which represent the probability of becoming infected at age and time
#'
#'
#' @export
#'
#'
#'
#'

collapse_populationarray <- function(data_set,
                                     list_of_birth_times,
                                     max_age,
                                     time_step,
                                     counts = FALSE)
  #  function takes in an array and returns a  dataframe that summarrises the entire population specific to the birth cohort


{

  time_since_infection <- seq(0, max_age, time_step)

  data_structure <- (t(data.frame(matrix(data_set, nrow=dim(data_set)[3], byrow=TRUE))))

  rownames(data_structure) <- NULL

  if (isTRUE(counts == TRUE)){

    colnames(data_structure) <- c("total_population",
                                  "susceptible",
                                  paste0(rep("infected", length(time_since_infection)), time_since_infection))

    #data_structure$total_infected <- rowSums(data_structure[ ,-(1:2)], na.rm = TRUE)


  }else{

    colnames(data_structure) <- c("total_prevalence",
                                  "susceptible_prevalence",
                                  paste0(rep("infected_prevalence", length(time_since_infection)), time_since_infection))


    data_structure <- data_structure/data_structure[,1]

  }

  index_table <- data.frame(cohort_d_o_b = rep(list_of_birth_times, length(seq(0, max_age, time_step))),
                            cohort_age = sort(rep(seq(0, max_age, time_step), length(list_of_birth_times))))


  index_table$cohort_calender_date <- index_table$cohort_d_o_b +index_table$cohort_age

  population <- cbind(index_table, data_structure)

  population$total_infected <- rowSums(population[ ,-(1:5)], na.rm = TRUE)




return(population)

}


# population  = specified_population()
#
#
# population[which(population$cohort_d_o_b == 1988), ]


