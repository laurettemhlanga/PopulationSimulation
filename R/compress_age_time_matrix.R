#' compress_age_time_matrix
#'
#' a function that takes in a matrix with a pararellogram format and transforms it into a rectangular format and assigns column and row names.
#'
#' @param age_time_matrix the matrix to be transformed
#'
#' @return returns a matrix with a rectangular format
#'
#'
#'
#'
#' @export

compress_age_time_matrix <- function(age_time_matrix )
  {

  number_of_rows_new_data <- nrow(age_time_matrix)-(ncol(age_time_matrix)) + 1
  #matrix_index_seq <- 1:((nrow(age_time_matrix) - ncol(age_time_matrix)) + 1) #create a vector of positive indices

  new_data <- matrix(NA, ncol = ncol(age_time_matrix), nrow = number_of_rows_new_data)

  matrix_index <- (row(age_time_matrix) - col(age_time_matrix)) #index each matrix entry

  matrix_index_seq <- 1:number_of_rows_new_data

#t(sapply(seq_along(matrix_index_seq), function(x) age_time_matrix[matrix_index == x - 1]))

  for (ii in matrix_index_seq){


      new_data[ii, ] <- age_time_matrix[matrix_index == ii - 1]


  }

  return(new_data)

}

