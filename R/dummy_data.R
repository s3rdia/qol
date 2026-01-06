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
dummy_data <- function(no_obs, monitor = .qol_options[["monitor"]]){
    # Measure the time
    start_time <- Sys.time()

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Preparation")
    #-------------------------------------------------------------------------#
    message(" > Generating dummy file. Computing ...\n",
            "   + years")

    # Prepare years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    start_year   <- current_year - 2

    # Prepare household generation
    message("   + households and states")

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

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate households")
    #-------------------------------------------------------------------------#

    years         <- numeric()
    household_ids <- numeric()
    person_ids    <- numeric()
    states        <- numeric()

    for (year in start_year:current_year){
        years_current <- rep(rep(year, number_of_households), times = persons_per_household)

        household_id_current <- rep(1:number_of_households,
                            times = persons_per_household)

        person_id_current <- sequence(persons_per_household)

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
    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate weights")
    #-------------------------------------------------------------------------#
    message("   + weights")

    household_weights <- stats::runif(number_of_households, 0.15, 0.35)
    weight <- rep(household_weights,
                  times = persons_per_household)

    #Prepare age
    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate age")
    #-------------------------------------------------------------------------#
    message("   + age")

    age <- stats::runif(no_obs, min = 0L,  max = 100L)

    id_1  <- which(person_ids == 1)
    id_34 <- which(person_ids %in% c(3, 4))
    id_56 <- which(person_ids %in% c(5, 6))

    age[id_1]  <- stats::runif(length(id_1),  min = 18, max = 100)
    age[id_34] <- stats::runif(length(id_34), min =  0, max =  25)
    age[id_56] <- stats::runif(length(id_56), min = 50, max = 100)

    age <- collapse::na_insert(age, prop = 0.05)

    # Prepare personal income
    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate income")
    #-------------------------------------------------------------------------#
    message("   + income")

    income <- stats::runif(no_obs, 0, 5000)
    income <- data.table::fifelse(age < 18, 0, income)
    income <- collapse::na_insert(income, prop = 0.05)

    # Prepare a probability variable with values between 0 and 1 (including both)
    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate probability")
    #-------------------------------------------------------------------------#
    message("   + probability")

    prob_temp <- sample(0:2, no_obs, replace = TRUE)

    id_2 <- which(prob_temp == 2)
    prob_temp[id_2] <- stats::runif(length(id_2), 0, 1)

    # Put data frame together with some basic variables to play with
    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Create data table")
    #-------------------------------------------------------------------------#
    message("   + sex\n",
            "\n > Putting data together")

    new_dummy_data <- data.table::data.table(
        year = as.integer(years),
        household_id = as.integer(household_ids),
        person_id = as.integer(person_ids),
        first_person = as.integer(first_person),
        state = as.integer(states),
        sex = as.integer(collapse::na_insert(sample(1:2, no_obs, replace = TRUE), prop = 0.05)),
        age = as.integer(age),
        education = collapse::na_insert(sample(c("low", "middle", "high"), no_obs, replace = TRUE), prop = 0.05),
        income = income,
        probability = prob_temp,
        weight = weight) |>
        data.table::setorder("year", "household_id", "person_id", "state")

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'dummy_data' execution time: ", end_time, " seconds\n")

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #-------------------------------------------------------------------------#

    utils::head(new_dummy_data, orig_obs)
}
