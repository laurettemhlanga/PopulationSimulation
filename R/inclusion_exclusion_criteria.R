#' inclusion_exclusion_criteria
#'
#' a functions that partitions prevalences of given birth cohorts at time 1 and time 2 to m clusters
#'
#' @param  required_ages is a vector of age specific prevalence  at time t1
#' @param  prevalence is a vector of age specific prevalence  at time t2
#' @param  time_step the time step between consecurtive dates or the length of the time between date of births of cohorts.
#' @param  radii inclusion magnitude in years
#'
#' @return a vector or matrice of the specified prevalence or about the age ranges required
#'
#' @export
#'
#'







inclusion_exclusion_criteria <- function(required_ages,
                                         prevalence,
                                         time_step,
                                         radii
                                         )
  {

  prevalence_ages <- (1:ncol(prevalence))* time_step

  if (is.vector(prevalence)){

    reduced_vector <- rep(NA, length(required_ages))

    for (ages_index in 1:length(required_ages)){

    reduced_vector[ages_index] <-  mean(prevalence[which((required_ages[ages_index] - radii) < prevalence_ages & prevalence_ages<= (required_ages[ages_index] + radii))], na.rm = T)

    }

  }else{

    reduced_vector <- matrix(NA, nrow = nrow(prevalence), ncol = length(required_ages))

    for (ages_index in 1:length(required_ages)){

    reduced_vector[ ,ages_index] <-  rowMeans(prevalence[ , which((required_ages[ages_index] - radii) < prevalence_ages & prevalence_ages<= (required_ages[ages_index] + radii))], na.rm = T)
    #reduced_vector[ ,ages_index] <-  rowSums(prevalence[ ,which((required_ages[2] - radii) < prevalence_ages & prevalence_ages<= (required_ages[2] + radii))], na.rm = T)
    }
  }

  return(reduced_vector)
}


