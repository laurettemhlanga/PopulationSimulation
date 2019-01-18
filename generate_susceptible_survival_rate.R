

#'calculates the survival probabilities in the susceptibles 
#' @param  incidence_matrix probability matrix of getting infected in the susceptibles  
#' @param  base_mortality_matrix probability matrix of getting infected in the sinfected  
#' @return returns a probability of not getting infected or dying within  susceptibles from \code{incidence_matrix} and \code{base_mortality_matrix}
#' @examples
#'  generate_susceptible_surv_rate(matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8), matrix(seq(0.01, 0.16, 0.01), ncol = 2 , nrow = 8))





generate_susceptible_surv_rate <- function(incidence_matrix, 
                                           base_mortality_matrix
                                           ){
  
  

    
    susceptible_surv_rate = 1 - ( base_mortality_matrix * base_mortality_matrix)

  
   return(susceptible_surv_rate)
  
}




z = generate_susceptible_surv_rate(incidence_matrix = x, base_mortality_matrix = y)
