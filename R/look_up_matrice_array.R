#' lookup
#'
#' A wrapper function that returns a a list of the the susceptible and infected population
#'
#' @param data_structure the number of steps to age the population
#' @param list_of_times a numeric vectors of length min:max; indicates the range of age to be included in simulation. Note that date format is not used.
#' @param time_step the time step between consecurtivelist_of_times
#' @param max_age maximum age each birth cohort is to be aged
#'
#' @return  a data frame with date of birth, age, and current date  if they susceptibles and for infecteds returns a data frame with date of
#'  birth, age, time since infection, and current date
#'
#'
#' @export


look_up <- function(data_structure = matrice,
                   list_of_times = 1985:1990,
                   max_age = 3,
                   time_step)
  {

  lookup_index <-  data.frame( date_of_birth = NULL,
                               age = NULL)
  ages = seq(0, max_age, time_step)

  for (index in 1:nrow(data_structure)){

    lookup <- data.frame(date_of_birth = rep(list_of_times[index], length(ages)),
                                      age =  ages)


    lookup_index <- rbind(lookup_index, lookup)
  }

  return(lookup_index )
}


lookup_array <- function(data_structure = do_one_simulation()$infected,
                         list_of_times = 1985:1990,
                         max_age = 3,
                         time_step = 1)

  {

  lookup_array_index <-  data.frame( date_of_birth = NULL,
                               age = NULL, time_since_infection = NULL)
  ages = seq(0, max_age, time_step)

  for (index in 1:dim(data_structure)[3]){

    lookup_array <- look_up(data_structure[, ,index],
                      list_of_times,
                      max_age,
                      time_step)

   lookup_array$time_since_infection <- rep(index, length(ages))

   lookup_array_index <- rbind(lookup_array_index, lookup_array)

  }

  return(lookup_array_index)
}
































# 7*/+
#
# first_birth_time = 1985; last_birth_time = 1990; time_step = 1
#
#
# data_structure  <- do_one_simulation()$susceptible
#
# list_of_times <- seq(first_birth_time , last_birth_time, time_step)
#
# if (is.matrix(data_structure) == T && nrow(data_structure) == length(list_of_times)){
#
#  <- (list_of_times)
#
#   lookup <- data.frame(date_of_birth = reference, ages = +)
#
# }else{
#
# }
#
#
# m = matrix(1:6,2,3)
#
# rownames(m) <-  c("d", "e" )
#
# colnames(m) <-  c("a", "b", "c" )
#
# matrix(m, dimnames = list(t(outer(colnames(m), rownames(m), FUN=paste))))
#

