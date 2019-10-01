

extract_survey_status <- function(survey_date,
                                  first_birth_time,
                                  time_step,
                                  matrices)

  # meant to extract the the corresponding states of the population from the susceptible to the infected at the specified
  # current date. it utilises the which function to provide the indices of the population[,, tau] matrix
  # entries with the same index number can easily indexed below tis one are related functions.

  # considerations on the most appropriate output a matrix of the eexact dimensions with some columns NAs throughout vs
  # tracking the columns lost by removing columns with NAs only  depending on the value of the anchor.
{
  #matrix_state <- list()

  matrix_state <- matrix(NA, nrow = length(survey_date), ncol = ncol(matrices))

  for (index in 1:length(survey_date)){

    matrix_states  <- extract_matrice_statussurvey(survey_date = survey_date[index],
                                                                    first_birth_time = first_birth_time,
                                                                    time_step = time_step ,
                                                                    matrices = matrices)

    matrix_state[index, ] <- c( matrix_states, rep(NA, ncol(matrices) - length(matrix_states)))

    }
  return(matrix_state)

}



# extract_survey_status (survey_date = c(1986, 1987.25, 1989),
#                                   first_birth_date = 1985,
#                                   time_step = 0.25,
#                                   matrices = m)









extract_matrice_statussurvey <- function(survey_date = 1986,
                                      first_birth_time = 1985,
                                      time_step = 0.25,
                                      matrices = incidence_matrix)

  # meant to extract the the corresponding states of the matrices at survey date


{
  anchor <- floor((survey_date - first_birth_time)/time_step)


  index <- (row(matrices) + col(matrices)) - 2

  columns <- 1:length(which(index == anchor))

  #survey_values <- matrix(NA, nrow = length(survey_date), ncol = length(columns))


    survey_values <-   matrices[index == anchor ]



  if (anchor > nrow(matrices)){

    age_column1 = (anchor - nrow(matrices)) * time_step

    skipped_ages <- matrix(NA, nrow = length(survey_date), ncol = length(seq(time_step, age_column1, time_step)))

    survey_values <- cbind(skipped_ages, survey_values)

  }


  return(survey_values)

}


