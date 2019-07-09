#' calculate_prevalence
#'
#' A function that extracts prevalence by age and time since infection
#'
#'
#' @param population_at_date a matrix of age and times since infection at a specified calender date.
#' @param infection_time specifies the output of interest wether or not its the total prevalence per age group or
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
                                   infection_time = NULL)
    {


  if (is.null(infection_time)){

    age_prevalence <- colSums(population_at_date[-1,], na.rm = T) / sum(colSums(population_at_date,  na.rm = T))

  }else{

    age_prevalence <- population_at_date / sum(colSums(population_at_date, na.rm = T))


  }

  return(age_prevalence)


}



#calculate_prevalence(population_at_date = extract_population_status(), infection_time = T)




# prevalence <- extract_age_prevalence(time_step = 0.5,
#                                      first_birth_time = 1995,
#                                      survey_date = 1998,
#                                      population =  x,
#                                      infection_time = 2.5)
