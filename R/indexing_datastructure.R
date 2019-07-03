#' indexing_datastructure
#'
#' a function that takes in a matrix with a pararellogram format and transforms it into a rectangular format and assigns column and row names.
#'
#' @param data_structure data sttructure to be indexed
#' @param date_of_birth vector of dates of births to be extracted
#' @param ages vector of ages to be extracted
#' @param times_since_infection vector times since infections to be extracted
#' @param first_birth_time date of birth of the first cohort
#' @param last_birth_time date of birth of the last cohort
#' @param time_step time_step the time step between consecurtive dates or the length of the time between date of births of cohorts
#' @param max_age max_age maximum age attained by each birth cohort
#'
#'
#'
#'
#' @return returns the data structure with respect to indexes provided.
#'
#'
#'
#'
#' @export


indexing_datastructure <- function(data_structure,
                                   date_of_birth,
                                   ages,
                                   times_since_infection,
                                   first_birth_time,
                                   last_birth_time,
                                   time_step,
                                   max_age)
{
 # the function helps with indexing the array but some aspects need reviewing
 # ie length(time since infection) since we have the total population and the susceptible
 # as part of the third dimensions. and currently is assuming the third dimension is dim[3] - 1
 #
  list_of_date_births <- seq(first_birth_time, last_birth_time, time_step)
  list_of_ages <- seq(0, max_age, time_step)
  list_of_times_since_infection <- seq(time_step, max_age, time_step)


  if ((is.null(ages) && is.null(times_since_infection)) == T) {

    # idx <- which(v == x[1])
    # idx[sapply(idx, function(i) all(v[i:(i+(length(x)-1))] == x))]

    vec <-  which(list_of_date_births == date_of_birth[1])
    vec[sapply(vec, function(i) all(list_of_date_births[i:(i+(length(date_of_birth)-1))] == date_of_birth))]

    output <-  data_structure[vec, , ]

  } else if ( (is.null(date_of_birth) && is.null(times_since_infection)) == T) {

    vec <-  which(list_of_ages == ages)

    output <-  data_structure[, vec, ]

  } else if ((is.null(date_of_birth) && is.null(ages)) == T) {

    vec <-  which(list_of_times_since_infection == times_since_infection )
    output <-  data_structure[, , vec]

  } else if ( is.null(times_since_infection) == T) {

    vec1 <-  which(list_of_date_births == date_of_birth)
    vec2 <-  which(list_of_ages == ages)

    output <-  data_structure[ vec1, vec2, ]

  } else if ( is.null(ages) == T) {

    vec1 <-  which(list_of_date_births == date_of_birth)
    vec2 <- which(list_of_times_since_infection == times_since_infection)

    output <-  data_structure[ vec1, , vec2]

  }else if ( is.null(date_of_birth) == T) {

    vec1 <-  which(list_of_ages == ages)
    vec2 <-  which(list_of_times_since_infection == times_since_infection)

    output <-  data_structure[ , vec1, vec2]

  } else {

    vec1 <-  which(list_of_date_births == date_of_birth)
    vec2 <-  which(list_of_ages == ages)
    vec3 <-  which(list_of_times_since_infection == times_since_infection)

    output <-  data_structure[ vec1, vec2, vec3]
  }
  return(output)
}
