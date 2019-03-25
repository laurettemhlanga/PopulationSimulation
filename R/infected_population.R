
#' infected_population 
#'
#' a function that returns a matrix of probabilities of mortality for each age and time step of the simulation
#'
#' @param susceptible the susceptible population counts 
#' @param  incidence_matrix_mod the normalised incidence with respect to the exponetial used in approximating the probability of surving infection or death in the susceptible state 
#' @param cumulative_infected_survival the survival probability of  being aged a at time t having been infected for tau years.
#' @return returns an array of dimensions time t , age a and time since infection - tau
#'
#' @export





infected_population <- function(susceptible = susceptible_pop_counts[,-ncol(susceptible_pop_counts)],
                                incidence_matrix_mod = mod_incidence,
                               cumulative_infected_survival = Cum_prob_survival_i)
  
{


  infected <- array(NA, dim = dim(cumulative_infected_survival))

  infected[, , 1] <- susceptible *  incidence_matrix_mod * cumulative_infected_survival[, , 1]

  for (aa in 1:dim(cumulative_infected_survival)[2]){
    
    for (ts in 2:dim(cumulative_infected_survival)[3]){
    
    
      if (aa - ts < 0 ){
        
        infected[, aa , ts] <- NA 
        
      }else{
        
        infected[, aa , ts] <-  infected[,aa - (ts-1), 1] * cumulative_infected_survival[ , aa, ts]
        }
      }
  }
  
  return(infected)
}













# infected_population <- function(susceptible = susceptible_pop_counts,
#                                 modified_incidence = mod_incidence,
#                                 cumulative_survival = Cum_prob_survival_i)
#   
# {
#   #fix the bug on calcilating the last column 
#   susceptible <- susceptible[,-ncol(susceptible)]
#   
#   infected_population_array <- array(NA, dim = dim(cumulative_survival))
#   
#   infected_population_array[, , 1] <- susceptible * mod_incidence * cumulative_survival[, , 1]
#   
#   for (ages in 1: dim(cumulative_survival)[2]){
#     for (tau in 2:dim(cumulative_survival)[3]){
#       
#       if (ages - tau <= 0){
# 
#         
#         infected_population_array[ , ages , tau] <- NA
#         
#         
#       }else{
#       
#         infected_population_array[ , ages , tau ] <- susceptible[ , ages - tau ] * modified_incidence[ , ages - tau] * cumulative_survival[ , ages, tau]
#     }
#     
#   }
# }
#   
#   return(infected_population_array)
# }
