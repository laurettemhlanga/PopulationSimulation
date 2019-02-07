#' transform_data
#'
#' a function that takes in a matrix with a pararellogram format and transforms it into a rectangular format and assigns column and row names.
#'
#' @param age_max denotes the maximum age reached by each cohort.
#' @param date_of_birth a vector with two numbers giving the minimum and maximum calender dates.
#' @param data_to_transform the matrix to be transformed
#' @param small_delta the time difference between consecutive dates of birth (if not specified defaults to 1).
#' The user-defined or package default function should be called by name when included as an argument in the generate_base_mortality_matrix function.
#' @return returns a matrix with a rectangular format, labelled rows and columns
#'
#'
#'

transform_data <- function(age_max,
                           data_to_transform,
                           date_of_birth,
                           small_delta = 1
)
  {


  date_birth <- seq(date_of_birth[1], date_of_birth[2], small_delta)
  age <- seq(0, age_max, small_delta)
  new_data <- matrix(NA, ncol = length(age), nrow = length(date_birth))

  matrix_index <- row(data_to_transform) - col(data_to_transform)

  matrix_index_seq <- 1:((nrow(data_to_transform) - ncol(data_to_transform)) + 1)

  for (ii in matrix_index_seq){


      new_data[ii, ] <- data_to_transform[matrix_index == ii - 1]


  }

  colnames(new_data) <-  paste0(age)
  rownames(new_data) <- paste0(date_birth)

  return(new_data)

}
