#' age_partitioned_prvalence
#'
#' a functions that partitions realised prevalence per age into the regions of a stipulated.
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

  #functions takes in two vectors of prevalences from time 1 and 2, partitions prevalences to
  #the number of regions. This is a wrapper function to partition_prevalence
  #runs a loop  through a vector of prevalences.

  partitioned_age_prevalance <-  as.list(rep(NA, length(ages)))

  counter = 1

  for (counter in 1:length(ages)){


    partitioned_age_prevalance[[counter]] <- partition_prevalence(overall_prevalence1 = overall_prevalence1[counter],
                                                                  overall_prevalence2 = overall_prevalence1[counter],
                                                                  sigma_prevalence = sigma_prevalence[counter],
                                                                  cluster_number = (1:length(cluster_size)),
                                                                  cluster_size)[[2]]

    partitioned_age_prevalance[[counter]] <- cbind(age = rep(ages[counter], length(cluster_size)),
                                                   partitioned_age_prevalance[[counter]])

    counter = counter + 1

  }


  partitioned_age_prevalance <- dplyr::bind_rows(partitioned_age_prevalance)

  return(partitioned_age_prevalance)

}
