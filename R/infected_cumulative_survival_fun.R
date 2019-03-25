
#'cumulative_infected_survival_probability
#'
#' a function that returns an array of the cumulative survival probability of beign aged a and time t and having been infected
#' for a time since infection tau
#'
#' @param infected_survival_prob the survival probability of being infected (which is an array)
#' @return returns the cumulative survival probability of being infected which is an array of dimensions time, age and time since infection - tau
#'
#' @export
#'
#'
#'

cumulative_infected_survival_probability <- function(infected_survival_prob =  probability_surviving)
  
{
  # the function calculated that calculates cumulative survival probability of beign aged a and time t and having been infected 
  # for a time since infection tau. Takes in the survival probability of being infected (which is an array) and  yields the cumulative 
  # survival probability of being infected
  
  cumulative_infected_survival <-  array(NA, dim  = dim(infected_survival_prob))
  
  cumulative_infected_survival[, , 1] <- matrix(rep(1, dim(infected_survival_prob)[1]*dim(infected_survival_prob)[2] ),
                                                nrow = dim(infected_survival_prob)[1], 
                                                ncol = dim(infected_survival_prob)[2])
  for (aa in 2:dim(infected_survival_prob)[2]){
    
    for (ts in 2:dim(infected_survival_prob)[3]){
      
      cumulative_infected_survival[, aa, ts] <- cumulative_infected_survival[, aa - 1, ts -1 ] * infected_survival_prob[, aa, ts]
      
    }
    
    
  }
  
  return(cumulative_infected_survival)
  
}




# cumulative_infected_survival_probability <- function(infected_survival_prob =  probability_surviving)
#   
#   {
#    # the dunction calculated the cumulative survival probability of beign aged a and time t and having been infected 
#    # for a time since infection tau. Takes in the survival probability of being infected (which is an array) and  yields the cumulative 
#    # survival probability of being infected
# 
#   cumulative_infected_survival_prob <- array(NA, dim = c(dim(infected_survival_prob)[1],
#                                                          dim(infected_survival_prob)[2],
#                                                          (dim(infected_survival_prob)[3])))
# 
#   cumulative_infected_survival_prob[, , 1] <- matrix(rep(1, (dim(infected_survival_prob)[1] * dim(infected_survival_prob)[2])),
#                                               nrow = dim(infected_survival_prob)[1],
#                                               ncol = dim(infected_survival_prob)[2])
# 
#   for (tt in (1:dim(infected_survival_prob)[1])){
# 
#     for (ta in (1:dim(infected_survival_prob)[2])){
# 
#       for (ts in (2:dim(infected_survival_prob)[3])){
# 
# 
#         cumulative_infected_survival_prob[tt, ta, ts] <-  cumulative_infected_survival_prob[tt , ta , ts - 1] * infected_survival_prob[tt, ta, ts]
# 
#        
# 
#         }
# 
# 
# 
#       }
# 
#     }
# 
#   }
# 
#   return(cumulative_infected_survival_prob)
# }
