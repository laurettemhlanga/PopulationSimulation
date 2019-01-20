#' generate_incidence_matrix is a function that returns a matrix of probabilities of infection accross indicated ranges of age and time
#' 
#' @param age_step numeric, indicates time at which the incidence rate is desired
#' @param birth_dates ?
#' @param generate_incidence a user defined or package default function which takes as arguments age and time and returns a numberic rate of incidence
#' @return a matrix of column length \code{age_step} and row length \code{birth_dates} . 
#' Values in matrix are numeric from 0-1, which represent the probability of becoming infected at \code{t} and \code{age_step}
#' @examples To be entered
#' 

generate_incidence_matrix <- function(age_step,
                                      birth_dates, 
                                      generate_incidence)
  {
  
  incidence_matrix  = matrix(NA, nrow = length(birth_dates) + age_step, ncol =  length(1:age_step))
  times  = 0:length(birth_dates)
  
  for (aa in 1:age_step){
    
    incidence_matrix[times + aa, aa] =  generate_incidence(times + aa, aa)
    
  }
  
  return(incidence_matrix)
}

# Create .Rd document 
devtools::document()

# Example, doesn't currently work 
y <-  generate_incidence_matrix (age_step = 2,
                                birth_dates = 10:20, generate_incidence)

