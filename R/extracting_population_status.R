
#' extract_population_status
#'
#' A function that extracts the population status at the specified survey date
#'
#' @param first_birth_date the minimum date of birth for the birth cohorts
#' @param survey_date date at which prevalences are required
#' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param population an array with the population history.
#'
#'
#' @return prevalences for ages at given current date.
#'
#'
#'
#' @export




extract_population_status <- function(survey_date,
                                      first_birth_date,
                                      time_step,
                                      population)

  # meant to extract the the corresponding states of the population from the susceptible to the infected at the specified
  # current date. it utilises the which function to provide the indices of the population[,, tau] matrix
  # entries with the same index number can easily indexed below tis one are related functions.

  # considerations on the most appropriate output a matrix of the eexact dimensions with some columns NAs throughout vs
  # tracking the columns lost by removing columns with NAs only  depending on the value of the anchor.


{
  anchor <- floor((survey_date - first_birth_date)/time_step)


  index <- (row(population[,,1]) + col(population[,,1])) - 1

  columns <- 1:length(which(index == anchor))

  populationdate <- matrix(NA, nrow = dim(population)[3], ncol = length(columns))

  for (row in 1:dim(population)[3]){


    populationdate[row, ] <-   population[, , row][index == anchor ]


  }

  if (anchor > dim(population)[1]){

    age_column1 = (anchor - dim(population)[1]) * time_step

    skipped_ages <- matrix(NA, nrow = dim(population)[3], ncol = length(seq(time_step, age_column1, time_step)))

    populationdate <- cbind(skipped_ages, populationdate)

  }


  return(populationdate)

}











##previous code




# extract_population_status <- function(survey_date,
#                                       first_birth_date,
#                                       time_step,
#                                       population)
#
#   # meant to extract the the corresponding states of the population from the susceptible to the infected at the specified
#   # current date. it utilises the which function to provide the indices of the population[,, tau] matrix
#   # entries with the same index number can easily indexed below tis one are related functions.
#
#   # considerations on the most appropriate output a matrix of the eexact dimensions with some columns NAs throughout vs
#   # tracking the columns lost by removing columns with NAs only  depending on the value of the anchor.
#
#
# {
#   anchor <- floor((survey_date - first_birth_date)/time_step)
#
#
#   index <- (row(population[,,1]) + col(population[,,1])) - 1
#
#   columns <- 1:length(which(index == anchor))
#
#   populationdate <- matrix(NA, nrow = dim(population)[3], ncol = length(columns))
#
#   for (row in 1:dim(population)[3]){
#
#
#     populationdate[row, ] <-   population[, , row][index == anchor ]
#
#
#   }
#
#   age_index_column1 = ifelse (anchor <= dim(population)[1], 1 ,(anchor - dim(population)[1])+1)
#
#   age_column1 = age_index_column1 * time_step
#
#   return(list(populationdate, age_column1))
#
# }
#
#
#







# extract_population_status <- function(survey_date = 2002,
#                                       first_birth_date = 1991,
#                                       time_step = 1,
#                                       population = x)
#
#   #extracts the population status at the specified survey date
#
# {
#   anchor <- floor((survey_date - first_birth_date)/time_step)
#
#   if (anchor < dim(population)[1] ) {
#
#     being_columns <- 1:anchor
#     being_rows <- anchor : 1
#     coordinates <- data.frame(rows = being_rows, columns = being_columns)
#
#   }else{
#
#     being_columns <- ((anchor - dim(population)[1])+1 ): dim(population)[2]
#     being_rows <- dim(population)[1] : ((anchor - dim(population)[1])+2)
#     coordinates <- data.frame(rows = being_rows, columns = being_columns)
#
#   }
#
#   populationdate <- data.frame(tau = 1:dim(population)[3]) # neeeds editing
#
#   for (row in 1:nrow(coordinates)){
#
#
#     populationdate1 <-  population[coordinates$rows[row], coordinates$columns[row], ]
#
#     populationdate <- cbind(populationdate, populationdate1)
#
#
#   }
#
#   return(populationdate)
#
# }
#




 # extract_population_status()
#
#
#
#
#
#
# extract_population_date<- function(population = x,
#                                    time_step  = 1,
#                                    first_birth_time = 1991)
#
#   #assigns the current date to the aerial view of the simulated popualtion
#
# {
#   ages <- seq(time_step, dim(population)[2] * time_step, time_step )
#
#   time_since_infection <- ages
#
#   list_of_date_births <- seq(first_birth_time, first_birth_time + max(ages), time_step)
#
#   current_date = matrix(NA, nrow = length(list_of_date_births), ncol = length(ages))
#   ages_current = matrix(NA, nrow = length(list_of_date_births), ncol = length(ages))
#
#
#   for (time_index in seq_along(list_of_date_births)){
#
#     for (age_index in seq_along(ages)){
#
#       current_date[time_index, age_index] <- list_of_date_births[time_index] + ages[age_index]
#
#       ages_current[time_index, age_index] <- ages[age_index]
#     }
#
#   }
#   return(list(ages_current, current_date))
# }
#
# y = extract_population_date()
#
# ##################################################################################################################################
#
# extract_population_history<- function(population = x,
#                                       survey_date = 2002,
#                                       look_up_list = y
# )
#   # extracts the population status at the required calender time
# {
#   current_date <- look_up_list[[2]]
#   ages_current <- look_up_list[[1]]
#
#   current_date_element_position <- which(current_date == floor(survey_date))
#   age_at_date <- as.vector(ages_current)[current_date_element_position]
#
#
#   current_date_element_position <- which(current_date == survey_date)
#   age_at_date <- as.vector(ages_current)[current_date_element_position]
#
#   population_age_at_date = data.frame(age = age_at_date)
#
#   for (tsi_index in 1:dim(population)[3]){
#
#     population_at_date <-  population[, , tsi_index][current_date_element_position]
#
#     #population_age_at_date[[tsi_index]] <- data.frame(age = age_at_date, population _count = population_at_date)
#
#     population_age_at_date <- cbind(population_age_at_date,  population_at_date)
#
#   }
#
#   return(data.frame(population_age_at_date))
#
#
# }
#
#
# extract_population_history()
# extract_population_status()

