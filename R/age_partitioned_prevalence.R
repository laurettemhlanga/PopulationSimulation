#' age_partitioned_prvalence
#'
#' a functions that partitions prevalences of given birth cohorts at time 1 and time 2 to m clusters
#'
#' @param  overall_prevalence1 is a vector of age specific prevalence  at time t1
#' @param  overall_prevalence2 is a vector of age specific prevalence  at time t2
#' @param  sigma_prevalence is a vector with the  standard errors associated with age specific prevalence  at time t1
#' @param  cluster_number unique id number of the cluster
#' @param  cluster_size sizes of the clusters
#' @param  ages the ages being sampled
#'
#'
#'
#' @return returns an array of dimensions time t , age a and time since infection - tau
#'
#' @export
#'
#'







age_partitioned_prevalence <-  function(overall_prevalence1,
                                        overall_prevalence2,
                                        sigma_prevalence,
                                        cluster_number,
                                        cluster_size,
                                        ages
){

  # Given the prevalences in a birth cohort at time 1 and time 2 and the associated variances,
  # The function partitions the prevalences of the birth cohort at respective times to
  # the enumeration areas (EAs) of the country N with the constraint that the
  # overall_prevalence1  and overall_prevalence2 are equivalent to the weighted output of the simulated prevalence
  # in the EAs.
  # This is a wrapper function to partition_prevalence (see below)
  # runs a loop  through a vector of prevalences.

  if (length(ages) == 1){

    partitioned_age_prevalance <- partition_prevalence(overall_prevalence1 = overall_prevalence1,
                                                                  overall_prevalence2 = overall_prevalence2,
                                                                  sigma_prevalence = sigma_prevalence,
                                                                  cluster_number = (1:length(cluster_size)),
                                                                  cluster_size)[[2]]
  }else{

    partitioned_age_prevalance <-  as.list(rep(NA, length(ages)))

    counter = 1

    for (counter in 1:length(ages)){


      partitioned_age_prevalance[[counter]] <- partition_prevalence(overall_prevalence1 = overall_prevalence1[counter],
                                                                    overall_prevalence2 = overall_prevalence2[counter],
                                                                    sigma_prevalence = sigma_prevalence[counter],
                                                                    cluster_number = (1:length(cluster_size)),
                                                                    cluster_size)[[2]]

      partitioned_age_prevalance[[counter]] <- cbind(age = rep(ages[counter], length(cluster_size)),
                                                     partitioned_age_prevalance[[counter]])

      counter = counter + 1

    }

    partitioned_age_prevalance <- dplyr::bind_rows(partitioned_age_prevalance)

    }

  return(partitioned_age_prevalance)

}





partition_prevalence <- function(overall_prevalence1,
                                 overall_prevalence2,
                                 sigma_prevalence,
                                 cluster_number,
                                 cluster_size


){
  # Given the prevalences in a birth cohort at time 1 and time 2 and the associated variances,
  # The function partitions the prevalences of the birth cohort at respective times to
  # the enumeration areas (EAs). The constraint is that the overall_prevalence1  and overall_prevalence2
  # are equivalent to the weighted output of the simulated prevalence
  # in the EAs.

  CoV_overall =  sigma_prevalence/overall_prevalence1

  cluster_prevalences =  stats::rnorm(length(cluster_number),
                                      mean = overall_prevalence1,
                                      sd = sigma_prevalence)

  # to avoid negative prevalences check if there are any negatives in the prevalence vector, replace
  # them by the mean of the simulated cluster_prevalences, otherwise continue with the calculation.

  if (any(cluster_prevalences < 0) == T){

    cluster_prevalences = replace(cluster_prevalences, which(cluster_prevalences<0), mean(cluster_prevalences))

  }else{

    cluster_prevalences = cluster_prevalences
  }



  cluster_CoV = stats::sd(cluster_prevalences) / mean(cluster_prevalences)

  cluster_prevalences_R_Adj = mean( cluster_prevalences ) + ((cluster_prevalences - mean(cluster_prevalences )) * (CoV_overall / cluster_CoV))


  Cluster_factor = sum(cluster_size * cluster_prevalences_R_Adj) / sum(cluster_size)
  cluster_prevalences_final1 = (cluster_prevalences_R_Adj / Cluster_factor ) * overall_prevalence1

  cluster_prevalences_final2 = (cluster_prevalences_final1)*(overall_prevalence2 / overall_prevalence1)

  prevcheck = c(sum(cluster_size * cluster_prevalences_final1) / sum(cluster_size), sum(cluster_size * cluster_prevalences_final2) / sum(cluster_size))

  return(list(prevcheck, data.frame( cluster_id = cluster_number,
                                     cluster_prevalence_t1 = cluster_prevalences_final1,
                                     cluster_prevalence_t2 = cluster_prevalences_final2 )))

}




