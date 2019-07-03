#' partition_prevalence
#'
#' a functions that partitions realised prevalence per age into the regions of a stipulated.
#'
#' @param  overall_prevalence1 is a vector of age specific prevalence  at time t1
#' @param  overall_prevalence2 is a vector of age specific prevalence  at time t2
#' @param  sigma_prevalence is a vector with the  standard errors associated with age specific prevalence  at time t1
#' @param  cluster_number unique id number of the cluster
#' @param  cluster_size sizes of the clusters
#'
#'
#'
#' @return returns an array of dimensions time t , age a and time since infection - tau
#'
#' @export
#'
#




partition_prevalence <- function(overall_prevalence1,
                                 overall_prevalence2,
                                 sigma_prevalence,
                                 cluster_number,
                                 cluster_size
                                 #function that divides/partitions  the overall prevalence and assigns to clusters
                                 #into given clusters

){

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

  cluster_prevalences_R_Adj = mean( cluster_prevalences ) + (cluster_prevalences - mean(cluster_prevalences )) * (CoV_overall / cluster_CoV)


  Cluster_factor = sum(cluster_size * cluster_prevalences_R_Adj) / sum(cluster_size)
  cluster_prevalences_final1 = (cluster_prevalences_R_Adj / Cluster_factor ) * overall_prevalence1

  cluster_prevalences_final2 = (cluster_prevalences_final1)*(overall_prevalence2 / overall_prevalence1)

  prevcheck = c(sum(cluster_size * cluster_prevalences_final1) / sum(cluster_size), sum(cluster_size * cluster_prevalences_final2) / sum(cluster_size))

  return(list(prevcheck, data.frame( cluster_id = cluster_number,
                                     cluster_prevalence_t1 = cluster_prevalences_final1,
                                     cluster_prevalence_t2 = cluster_prevalences_final2 )))

}



