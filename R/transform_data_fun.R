#' transform_data
#'
#' a function that takes in a matrix with a pararellogram format and transforms it into a rectangular format and assigns column and row names.
#'
#' @param data_to_transform the matrix to be transformed
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @return returns a matrix with a rectangular format, labelled rows and columns
#'
#'
#'
#'
#' @export

transform_data <- function(data_to_transform)
  {

  new_data <- matrix(NA, ncol = ncol(data_to_transform), nrow =  nrow(data_to_transform)-(ncol(data_to_transform)) + 1)

  matrix_index <- row(data_to_transform) - col(data_to_transform) #index each matrix entry

  matrix_index_seq <- 1:((nrow(data_to_transform) - ncol(data_to_transform)) + 1) #create a vector of positive indices

  for (ii in matrix_index_seq){


      new_data[ii, ] <- data_to_transform[matrix_index == ii - 1]


  }

  return(new_data)

}
