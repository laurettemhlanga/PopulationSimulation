#' partition_prevalence
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#'# @param  overall_prevalence1 is a vector of age specific prevalence  at time t1
#'##Z @param  sigma_prevalence is a vector with the  standard errors associated with age specific prevalence  at time t1
#' #@param  cluster_number
#' #@param  cluster_size
#' #@param  num_cluster_sample
#' #@param  ind_per_cluster
#' #@param  sigma_prevalence
#' #@param  cluster_number
#' #@param  cluster_size
#'
#' #@return returns an array of dimensions time t , age a and time since infection - tau
#'
#' #@export

#library(tidyverse)

# partition_prevalence <- function(overall_prevalence1,
#                                  overall_prevalence2,
#                                  sigma_prevalence,
#                                  cluster_number,
#                                  cluster_size
# #function that divides/partitions  the overall prevalence and assigns to clusters
# #into given clusters
#
# ){
#
#   CoV_overall =  sigma_prevalence/overall_prevalence1
#
#   cluster_prevalences = rnorm(length(cluster_number),
#                               mean = overall_prevalence1,
#                               sd = sigma_prevalence)
#   # to avoid negative prevalences we check is there are any negatives in the prevalences vector and if they are we replace
#   #the negative values by the mean of the simulated cluster_prevalences
#
#   if (any(cluster_prevalences < 0) == T){
#
#     cluster_prevalences = replace(cluster_prevalences, which(cluster_prevalences<0), mean(cluster_prevalences))
#
#   }else{
#
#     cluster_prevalences = cluster_prevalences
#   }
#
#
#
#   cluster_CoV = sd(cluster_prevalences) / mean(cluster_prevalences)
#
#   cluster_prevalences_R_Adj = mean( cluster_prevalences ) + (cluster_prevalences - mean(cluster_prevalences )) * (CoV_overall / cluster_CoV)
#
#
#   Cluster_factor = sum(cluster_size * cluster_prevalences_R_Adj) / sum(cluster_size)
#   cluster_prevalences_final1 = (cluster_prevalences_R_Adj / Cluster_factor ) * overall_prevalence1
#
#   cluster_prevalences_final2 = (cluster_prevalences_final1)*(overall_prevalence2 / overall_prevalence1)
#
#   prevcheck = c(sum(cluster_size * cluster_prevalences_final1) / sum(cluster_size), sum(cluster_size * cluster_prevalences_final2) / sum(cluster_size))
#
#   return(list(prevcheck, data.frame( cluster_id = cluster_number,
#                                      cluster_prevalence_t1 = cluster_prevalences_final1,
#                                      cluster_prevalence_t2 = cluster_prevalences_final2 )))
#
# }
#
#
#
# age_partitioned_prvalence <-  function(prevalence_1,
#                                        prevalence_2,
#                                        sigma_prevalence_1,
#                                        cluster_number,
#                                        cluster_size,
#                                        ages
#                                        ){
#
# #functions takes in a vector of prevalences from time 1 and time 2 and partitions to the prevalences to
# #the number of regions. this a wrapper function to partition_prevalence
# #runs a loop  through a a vector of prevalences.
#
# partitioned_age_prevalance <-  as.list(rep(NA, length(ages)))
#
# counter = 1
#
# for (counter in 1:length(ages)){
#
#
#   partitioned_age_prevalance[[counter]] <- partition_prevalence(overall_prevalence1 = prevalence_1[counter],
#                                                      overall_prevalence2 = prevalence_2[counter],
#                                                      sigma_prevalence = sigma_prevalence_1[counter],
#                                                      cluster_number = (1:length(cluster_size)),
#                                                      cluster_size)[[2]]
#
#   partitioned_age_prevalance[[counter]] <- cbind(age = rep(ages[counter], length(cluster_size)),
#                                                  partitioned_age_prevalance[[counter]])
#
#   counter = counter + 1
#
# }
#
#
#   partitioned_age_prevalance <- bind_rows(partitioned_age_prevalance)
#
#   return(partitioned_age_prevalance)
#
# }
#
#
#
#
#
# Survey_pps <- function(cluster_number,
#                        cluster_size ,
#                        num_cluster_sample,
#                        ind_per_cluster,
#                        overall_prevalence1,
#                        overall_prevalence2,
#                        sigma_prevalence
# ){
#
#   #Steps in appling Probability propotional to size(pps) and ways to ensure equal sampling weights
#   #individual (calculation of basic weights)
#
#   survey_data = data.frame(cluster_id = cluster_number,
#                            cluster_population = cluster_size)
#
#   survey_data$cumulative_sum = cumsum(survey_data$cluster_population)
#
#
#   survey_data = merge(survey_data, partition_prevalence(overall_prevalence1 = overall_prevalence1,
#                                                         overall_prevalence2 = overall_prevalence2,
#                                                         sigma_prevalence = sigma_prevalence,
#                                                         cluster_number = cluster_number,
#                                                         cluster_size = cluster_size)[[2]])
#
#
#   sampling_interval = survey_data$cumulative_sum[length(cluster_number)] / num_cluster_sample
#
#   threshold = survey_data$cumulative_sum[max(which(survey_data$cumulative_sum < sampling_interval))]
#
#   #threshold added to avoid getting NAs this ensures the first random number is not greater than
#   # the sampling interval
#
#   random_number = sample(1 : threshold, 1)
#
#   cluster_series = cumsum(c(random_number, rep(sampling_interval, num_cluster_sample-1)))
#
#   id_clusters_sampled = as.vector(rep(NA, num_cluster_sample))
#
#   counter = 1
#   for (tt in cluster_series){
#
#     id_clusters_sampled[counter] = which(survey_data$cumulative_sum > tt)[1]
#
#     counter = counter + 1
#   }
#
#   survey_data = survey_data[id_clusters_sampled, ]
#
#   survey_data$cluster_series = cluster_series
#
#   survey_data$sampling_fraction_1 = (survey_data$cluster_population * num_cluster_sample) / sum(cluster_size)
#
#   survey_data$ind_per_cluster = rep(ind_per_cluster, length(id_clusters_sampled))
#
#   survey_data$sampling_fraction_2 = survey_data$ind_per_cluster / survey_data$cluster_population
#
#   survey_data$overall_weight = 1 / (survey_data$sampling_fraction_1 * survey_data$sampling_fraction_2)
#
#   return(survey_data)
# }
#
#
#
#
# #example
# z = age_partitioned_prvalence(prevalence_1 = c(0.195, 0.247, 0.3115, 0.35),
#                               prevalence_2 = c(0.25, 0.26, 0.343, 0.387),
#                               sigma_prevalence_1 = c(0.00019, 0.00022, 0.00021, 0.0002),
#                               cluster_number =  1:4,
#                               cluster_size =  c(800, 300, 325, 1000),
#                               ages = 15:18)
#
#
# Survey_pps(cluster_number =  1:4,
#            cluster_size =   c(800, 300, 325, 1000),
#            num_cluster_sample = 2,
#            ind_per_cluster = 100,
#            overall_prevalence1 = c(0.195, 0.247, 0.3115, 0.35),
#            overall_prevalence2 = c(0.25, 0.26, 0.343, 0.387),
#            sigma_prevalence = c(0.00019, 0.00022, 0.00021, 0.0002))
#
# filter(z, cluster_id == c(1,3))
