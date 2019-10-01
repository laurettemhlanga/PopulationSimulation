#' pps_survey_selfweighting_structured
#'
#' conducts surveys using Probability propotional to size (pps) in given regions.
#'
#' @param  overall_prevalence1 is a vector of age specific prevalence  at time t1
#' @param  overall_prevalence2 is a vector of age specific prevalence  at time t2
#' @param  sigma_prevalence is a vector with the  standard errors associated with age specific prevalence  at time t1
#' @param  cluster_number unique id number of the cluster
#' @param  cluster_size sizes of the clusters
#' @param  ind_per_cluster individuals to sample per cluster
#' @param  num_cluster_sample the number of people in the cluster
#' @param ages ages of the birth cohorts at time one of the survey
#'
#'
#'
#' @return returns an array of dimensions time t , age a and time since infection - tau
#'
#' @export



pps_survey_selfweighting_structured <- function(cluster_number,
                                                cluster_size ,
                                                num_cluster_sample,
                                                ind_per_cluster,
                                                overall_prevalence1,
                                                overall_prevalence2,
                                                sigma_prevalence,
                                                ages
){

  # applies Probability propotional to size(pps) and  ensures equal sampling weights
  # per individual (calculation of basic weights)
  ##### provide survey data vs calling the age partitioned fuction within here

  survey_data = data.frame(cluster_id = cluster_number,
                           cluster_population = cluster_size)

  survey_data$cumulative_sum = cumsum(survey_data$cluster_population)


  survey_data = merge(survey_data, age_partitioned_prevalence(overall_prevalence1 = overall_prevalence1,
                                                        overall_prevalence2 = overall_prevalence2,
                                                        sigma_prevalence = sigma_prevalence,
                                                        cluster_number = cluster_number,
                                                        cluster_size = cluster_size,
                                                        ages = ages))


  sampling_interval = survey_data$cumulative_sum[length(cluster_number)] / num_cluster_sample

  threshold = survey_data$cumulative_sum[max(which(survey_data$cumulative_sum < sampling_interval))]

  #threshold added to avoid getting NAs this ensures the first random number is not greater than
  # the sampling interval

  random_number = sample(1 : threshold, 1)

  cluster_series = cumsum(c(random_number, rep(sampling_interval, num_cluster_sample - 1)))

  id_clusters_sampled = as.vector(rep(NA, num_cluster_sample))

  counter = 1
  for (tt in cluster_series){

    id_clusters_sampled[counter] = which(survey_data$cumulative_sum > tt)[1]

    counter = counter + 1
  }

  survey_data = survey_data[id_clusters_sampled, ]

  survey_data$cluster_series = cluster_series

  survey_data$sampling_fraction_1 = (survey_data$cluster_population * num_cluster_sample) / sum(cluster_size)

  survey_data$ind_per_cluster = rep(ind_per_cluster, length(id_clusters_sampled))

  survey_data$sampling_fraction_2 = survey_data$ind_per_cluster / survey_data$cluster_population

  survey_data$overall_weight = 1 / (survey_data$sampling_fraction_1 * survey_data$sampling_fraction_2)

  return(survey_data)
}





