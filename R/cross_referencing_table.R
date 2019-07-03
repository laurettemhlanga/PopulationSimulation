#' #'cross_referencing_table
#' #'
#' #' A function that pin points/enables cross referencing of tables
#' #'
#' #'
#' #'
#' #' @param vector_of_times_index index (indicies) corresponding to the birth times
#' #' @param vector_of_ages_index index (indicies) corresponding to ages
#' #' @param vector_of_times_since_infection_index index (indicies) corresponding to  time(s) since infection
#' #' @param first_birth_time the minimum date of birth for the birth cohorts
#' #' @param last_birth_time the maximum date of birth for the birth cohorts
#' #' @param time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' #' @param max_age maximum age each birth cohort is aged
#' #'
#' #'
#' #' @return The actual date of birth, ages, and time since infection for given times, ages and time since infection indexes
#' #'
#' #'
#' #'
#' #'
#' #' @export
#' #'
#'
#'
#' cross_referencing_table <-  function(vector_of_times_index = 1:2,
#'                                      vector_of_ages_index = 3:4,
#'                                      vector_of_times_since_infection_index = 1,
#'                                      first_birth_time,
#'                                      last_birth_time,
#'                                      time_step,
#'                                      max_age,
#'                                      current_date){
#'
#'
#'   list_of_date_births <- seq(first_birth_time, last_birth_time, time_step)
#'   list_of_ages <- seq(0, max_age, time_step)
#'   list_of_time_since_infection <- seq(time_step, max_age, time_step)
#'
#'   if (isTRUE(length(vector_of_times_index) > 1)){
#'
#'
#'     coresponding_date_births <- list_of_date_births[vector_of_times_index]
#'     coresponding_date_ages <- list_of_ages[vector_of_ages_index]
#'     coresponding_current_date <- coresponding_date_births + coresponding_date_ages
#'     coresponding_date_tsi <- list_of_time_since_infection[vector_of_times_since_infection_index]
#'
#'
#'     true_dates <- data.frame(d_o_b = coresponding_date_births,
#'                              current_date = coresponding_current_date,
#'                              ages =  coresponding_date_ages,
#'                              tsi = coresponding_date_tsi)
#'
#'   }else {
#'
#'     coresponding_date_births <- list_of_date_births[vector_of_times_index]
#'     coresponding_date_ages <- list_of_ages[vector_of_ages_index]
#'     coresponding_current_date <- coresponding_date_births + coresponding_date_ages
#'     coresponding_date_tsi <- list_of_time_since_infection[vector_of_times_since_infection_index]
#'
#'
#'     true_dates <- data.frame(d_o_b = coresponding_date_births,
#'                              current_date = coresponding_current_date,
#'                              ages =  coresponding_date_ages,
#'                              tsi = rep(vector_of_times_since_infection_index, length(coresponding_date_births)))
#'     }
#'
#'   return( true_dates)
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#' cross_referencing_table(vector_of_times_index = 1:5,
#'                         vector_of_ages_index = 3:7,
#'                         vector_of_times_since_infection_index = 3,
#'                         first_birth_time = 1985,
#'                         last_birth_time = 1990,
#'                         time_step = 0.5,
#'                         max_age = 4)
#'





# cross_referencing_table <-  function(vector_of_times_index = 1:2,
#                                      vector_of_ages_index = 3:4,
#                                      vector_of_times_since_infection_index = 1,
#                                      first_birth_time,
#                                      last_birth_time,
#                                      time_step,
#                                      max_age,
#                                      ...){
#
#   #the function looks longer than expected when are using it and how
#   #is it meant to exctract the real values at supplied indices or
#   #broader usage?????
#
#   list_of_date_births <- seq(first_birth_time, last_birth_time, time_step)
#   list_of_ages <- seq(0, max_age, time_step)
#   list_of_time_since_infection <- seq(time_step, max_age, time_step)
#
#   if (isTRUE(length(vector_of_times_index) > 1)){
#
#     coresponding_date_births <- list_of_date_births[vector_of_times_index]
#     coresponding_date_ages <- list_of_ages[vector_of_ages_index]
#     coresponding_date_tsi <- vector_of_times_since_infection_index[vector_of_times_since_infection_index]
#
#
#     true_dates <- data.frame(D_O_B = coresponding_date_births,
#                              Ages =  coresponding_date_ages,
#                              TSI = coresponding_date_tsi)
#   }else if (isTRUE(length(vector_of_times_index) == 1){
#
#     coresponding_date_births <- list_of_date_births[vector_of_times_index]
#     coresponding_date_ages <- list_of_ages[vector_of_ages_index]
#     coresponding_date_tsi <- vector_of_times_since_infection_index[vector_of_times_since_infection_index]
#
#
#     true_dates <- data.frame(D_O_B = coresponding_date_births,
#                              Ages =  coresponding_date_ages,
#                              TSI = rep(vector_of_times_since_infection_index, length(coresponding_date_births)))
#   }else if (){
#
#     true_dates <- 0
#   }else{
#     true_dates
#
#   }
#
#   return( true_dates)
#
# }
