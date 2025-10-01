#' Dummy Data
#'
#' @description
#' The dummy data frame contains a few randomly generated variables like year, sex,
#' age, income and weight to test out functionalities. It can be generated with the desired
#' number of observations.
#'
#' @param no_obs Number of observations.
#' @param monitor FALSE by default. If TRUE outputs two charts to visualize the functions time consumption.
#'
#' @return Returns a dummy data table.
#'
#' @examples
#' my_data <- dummy_data(1000)
#'
#' @export
dummy_data <- function(no_obs, monitor = FALSE){
    # Measure the time
    start_time <- Sys.time()

    monitor_df <- NULL |> monitor_start("Preparation")

    # Prepare years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    start_year   <- current_year - 2

    # Prepare household generation
    if (no_obs < 1000){
        number_of_households <- max(1, no_obs)
    }
    else if (no_obs < 1000000){
        number_of_households <- max(1, as.integer(no_obs / 5))
    }
    else{
        number_of_households <- max(1, as.integer(no_obs / 10))
    }
    persons_per_household <- sample(1:6, size = number_of_households, replace = TRUE)

    # Prepare probabilities so that each state has a different number of obs
    state_probs <- c(3.5, 2.2, 9.6, 0.8, 21.6, 7.6, 5.0, 13.5, 15.8, 1.2, 4.4, 3.0, 1.9, 4.9, 2.6, 2.5)
    sample(1:16, 1, replace = TRUE, prob = state_probs)

    monitor_df <- monitor_df |> monitor_next("Generate households")

    years         <- numeric()
    household_ids <- numeric()
    person_ids    <- numeric()
    states        <- numeric()

    for (year in start_year:current_year){
        years_current <- rep(rep(year, number_of_households), times = persons_per_household)

        household_id_current <- rep(1:number_of_households,
                            times = persons_per_household)

        person_id_current <- unlist(lapply(persons_per_household, function(n) 1:n))

        state_temp     <- sample(1:16, number_of_households, replace = TRUE, prob = state_probs)
        states_current <- rep(state_temp, times = persons_per_household)

        years         <- c(years, years_current)
        household_ids <- c(household_ids, household_id_current)
        person_ids    <- c(person_ids, person_id_current)
        states        <- c(states, states_current)
    }

    orig_obs <- no_obs
    no_obs   <- length(household_ids)

    # Prepare variable to identify the first person per household
    first_person <- data.table::fifelse(person_ids == 1, 1, 0)

    # Prepare weight per household
    monitor_df <- monitor_df |> monitor_next("Generate weights")

    household_weights <- stats::runif(number_of_households, 0.15, 0.35)
    weight <- rep(household_weights,
                  times = persons_per_household)

    #Prepare age
    monitor_df <- monitor_df |> monitor_next("Generate age")

    age <-                                             stats::runif(no_obs, min = 0L,  max = 100L)
    age <- data.table::fifelse(person_ids == 1,        stats::runif(no_obs, min = 18L, max = 100L), age)
    age <- data.table::fifelse(person_ids %in% c(3,4), stats::runif(no_obs, min = 0L,  max = 25L),  age)
    age <- data.table::fifelse(person_ids %in% c(5,6), stats::runif(no_obs, min = 50L, max = 100L), age)
    age <- insert_nas(age)

    # Prepare personal income
    monitor_df <- monitor_df |> monitor_next("Generate income")

    income <- stats::runif(no_obs, 0, 5000)
    income <- data.table::fifelse(age < 18, 0, income)
    income <- insert_nas(income)

    # Prepare a probability variable with values between 0 and 1 (including both)
    monitor_df <- monitor_df |> monitor_next("Generate probability")

    prob_temp <- sample(0:2, no_obs, replace = TRUE)
    prob_temp[prob_temp == 2] <- stats::runif(sum(prob_temp == 2), 0, 1)

    # Put data frame together with some basic variables to play with
    monitor_df <- monitor_df |> monitor_next("Create data table")

    new_dummy_data <- data.table::data.table(
        year = as.integer(years),
        household_id = as.integer(household_ids),
        person_id = as.integer(person_ids),
        first_person = as.integer(first_person),
        state = as.integer(states),
        sex = as.integer(insert_nas(sample(1:2, no_obs, replace = TRUE))),
        age = as.integer(age),
        education = insert_nas(sample(c("low", "middle", "high"), no_obs, replace = TRUE)),
        income = income,
        probability = prob_temp,
        weight = weight) |>
        data.table::setorder("year", "household_id", "person_id", "state")

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'dummy_data' execution time: ", end_time, " seconds\n")

    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)

    utils::head(new_dummy_data, orig_obs)

}


#' Randomly Insert NA Values
#'
#' @description
#' Randomly insert NA values into variable.
#'
#' @param values Variable in which to insert NA values.
#'
#' @return
#' Returns a variable with randomly inserted NA values.
#'
#' @noRd
insert_nas <- function(values) {
    # Get random positions to inject NAs
    nobs <- length(values)
    ratio <- stats::runif(1, 0.01, 0.1)

    na_position <- sample(seq_len(nobs), size = as.integer(ratio * nobs))

    # Inject NAs at random positions
    values[na_position] <- NA
    values
}
