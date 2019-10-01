#' calculate_prevalence
#'
#' A function that extracts prevalence by age and time since infection
#'
#'
#' @param population_at_date a matrix of age and times since infection at a specified calender date.
#' @param keeping_infection_time specifies the output of interest wether or not its the total prevalence per age group or
#' prevalence per specified age and time since infection.
#' total prevalence for all ranges at the current date.
#'
#'
#' @return prevalences for ages at given current date.
#'
#'
#'
#' @export


#population_at_date <- extract_population_status()




calculate_prevalence <- function(population_at_date,
                                 keeping_infection_time = FALSE)

 # Consider using the apply function around the for loop. is this neccessary
  #or we can produce a list through recency calculation.


{
  if (keeping_infection_time){

    age_prevalence <- matrix(NA, ncol = ncol(population_at_date), nrow = nrow(population_at_date))

    for (age_index in (1:ncol(population_at_date))){

    age_prevalence[ ,age_index] <- population_at_date[ ,age_index] / sum(population_at_date[ ,age_index], na.rm = T)

    }

 }else{


    age_prevalence <- colSums(population_at_date[-1,], na.rm = T) / colSums(population_at_date,  na.rm = T)

    }

  return(age_prevalence)
}












#
# calculate_prevalence_1 <- function(population_at_date,
#                                 keeping_infection_time = TRUE)
#     {
#
#
#   if (keeping_infection_time){
#
#     age_prevalence <- population_at_date / colSums(population_at_date, na.rm = T)
#
#
#
#   }else{
#
#     age_prevalence <- colSums(population_at_date[-1,], na.rm = T) / colSums(population_at_date,  na.rm = T)
#
#
#   }
#
#   return(age_prevalence)
#
#
# }
#
#
#

#calculate_prevalence(population_at_date = extract_population_status(), infection_time = T)




# prevalence <- extract_age_prevalence(time_step = 0.5,
#                                      first_birth_time = 1995,
#                                      survey_date = 1998,
#                                      population =  x,
#                                      infection_time = 2.5)
