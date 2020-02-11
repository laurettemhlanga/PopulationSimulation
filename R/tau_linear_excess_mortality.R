#' tau_linear_excess_mortality
#'
#' a function that takes as arguments age,time and tau (which indicates the average time since infection among the infected population)
#' and returns a numeric vector with length equivelent to the number of times indicated by the simulation
#' representing the excess mortality rate  induced by being infected
#'
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param times_since_i numeric, indicates a constant rate of mortality when
#' @param intercept numeric, indicates minimum age to be included in the simulation
#' @param slope numeric, indicates maximum age to be included in the simulation
#' @return a numeric vector that represents the mortality rate at t.
#'
#'
#'
#' @export

tau_linear_excess_mortality  <- function(ages, times,
                                         times_since_i,intercept = 0.05,
                                                    slope = 0)
{

# calculates the excess mortality rate as a linear function of the time since infcetion
# user provides the vector of ages and vector of times and the time since infection is taken in a value at  time
# user has to supply what they believe the intercept and slope is.

  tau <- times_since_i
  ncols <- length(ages)
  nrows <- length(ages)


    ex_mort_tau <- rep(intercept + (slope * tau), length(ages))


  return(ex_mort_tau)
}



# tau_linear_excess_mortality(ages = 5:10,
#                             times = 1980:1985,
#                             value_of_tau = 2,
#                             intercept = 0.01,
#                             slope = 0)
#









#' survival_prob_infected_mahiane
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param times_since_i umeric, indicates time since infection among the infected poplation
#' @param shape numeric, indicates minimum age to be included in the simulation
#' @param max_survival  numeric, indicates minimum mortality, which is at age_min
#' @param min_survival numeric, indicates maximum/final mortality at age_max, unless otherwise specified by user defined function
#' @param age_max numeric, indicates maximum age to be included in the simulation
#' @param age_min umeric, indicates minimum age to be included in the simulation
#' @return a numeric vector that represents the mortality rate at t.
#'
#' @export






excess_mahiane <- function(ages,times,
                           times_since_i, shape = 2,
                           max_survival = 16, min_survival = 6.6,
                           age_max = 50, age_min = 0)
{

  scale <- max_survival -
    ((max_survival - min_survival)/(age_min - age_max)) * ages

  excess_mortality <-  (times_since_i / scale) ^ shape

  return(excess_mortality)

}



#' step_excess_mortality
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param times_since_i/taus umeric, indicates time since infection among the infected poplation.
#'
#' @export




step_excess_mortality <- function(ages, tau_step = 1,
                                  times, mortality_step = 0.001,
                                  times_since_i){


  mortality <- ifelse(times_since_i < tau_step, 0, mortality_step)

  return(mortality)
}



#' weibull_hazard
#'
#' a function that takes as arguments age,time and tau - which indicates the average time since infection among the infected population - and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of excess mortality -i.e. among infected population relative to non-infected population - at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param times_since_i umeric, indicates time since infection among the infected poplation.
#' @param shape numeric, indicates minimum age to be included in the simulation
#' @param scale numeric, indicates minimum mortality, which is at age_min
#' @param base allows generic use as a hazard function excess mort or base mortality
#' @param ... extra parameters
#' @return a numeric vector that represents the mortality rate at
#'
#' @export

# weibull_hazard

weibull_hazard <- function(ages,
                           times,
                           times_since_i, shape = 2,
                           scale = 5, base = T, ...)
                         # max_survival = 16, min_survival = 6.6
                         # age_max = 50, age_min = 0, ...)
{
  # scale <- max_survival -
  #   ((max_survival - min_survival)/(age_min - age_max)) * ages
  if (base == T){

    mortality <-  ((1/scale) * shape ) * (ages/ scale) ^ (shape)

    }else{

    mortality <-  ((1/scale) * shape ) * (times_since_i / scale) ^ (shape - 1)

   }

  return(mortality)

}




###################################

#' no_basemortality
#'
#' a function that takes as arguments age and time and returns a numeric vector of length equivelent to the number of times indicated by the simulation
#' representing a rate of mortality at the indicated age and time
#' The generate_mortality function is required as an argument for the package's do_simulation function
#' The function may be user defined and stored as an R object. Otherwise a default value - entered as "default" - is provided by the package
#'
#'
#' @param times numeric, indicates time or times at which the incidence rate is desired
#' @param ages  numeric, indicates age or ages at which the incidence rate is desired
#' @param times_since_i numeric, indicates age or ages at which the incidence rate is desired
#' @return a numeric vector that represents the mortality rate at t.
#'
#' @export

#Option 1




no_excessmortality <- function(ages,times,
                               times_since_i){

  mortality =  rep( 0, length(ages))

  return(mortality)
}







# @param min_survival numeric, indicates maximum/final mortality at age_max, unless otherwise specified by user defined function
# @param age_max numeric, indicates maximum age to be included in the simulation
# @param age_min umeric, indicates minimum age to be included in the simulation


# excess_mahiane(ages = 5:10,
#                times = 1980:1985,
#                times_since_i = 2, shape = 2,
#                max_survival = 16, min_survival = 6.6,
#                age_max = 50, age_min = 0)







#option A

# excess_mortality_fun <- function(vector_of_age, vector_of_time, vector_of_time_since_i, constant = 0, age_min = 0,
#                                  age_max = 50,
#                                  mort_min =0.01,
#                                  mort_max = 0.05)
# {
#
#   # calculates excess mortality as a function of age and is constant in time. The excess mortality resulting is a
#   # linear function. Note if a non zero value is provided for the variable constant then a
#   # constant incidence is obtatined i.e.  base mortality (age, time) = constant.
#
#   age <- ages
#   time_since_i <- vector_of_time_since_i
#
#   if (constant > 0) {
#
#     return(vector(rep(constant,ncol(age) * nrow(age)),  ncol = ncol(age), nrow = nrow(age)))
#
#   }else{
#
#     Ex_mort_tau = ifelse(age <= age_min, 0,
#                          ifelse(age <= age_max, mort_min + ((mort_max - mort_min)/(age_max - age_min)) * ta *(age - age_min),
#                                 0))
#   }
#   return(Ex_mort_tau)
# }




