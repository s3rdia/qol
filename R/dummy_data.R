#' Dummy Data
#'
#' @description
#' The dummy data frame contains a few randomly generated variables like year, sex,
#' age, income and weight to test out functionalities. It can be generated with the desired
#' number of observations.
#'
#' @param no_obs Number of observations.
#' @param insert_na TRUE by default. Inserts random NA values into variables.
#' @param monitor FALSE by default. If TRUE outputs two charts to visualize the functions time consumption.
#'
#' @return Returns a dummy data table.
#'
#' @examples
#' my_data <- dummy_data(1000)
#'
#' @export
dummy_data <- function(no_obs    = 25000,
                       insert_na = TRUE,
                       monitor   = .qol_options[["monitor"]]){
    # Measure the time
    print_start_message()

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Preparation")
    #-------------------------------------------------------------------------#
    print_step("MAJOR", "Generating dummy file. Computing ...")

    # Reduce the number of observations to the number of households, because different
    # household sizes will be generated which basically expands this number to the number
    # of observations again. Additionally the number of households is divided by three
    # because at the end the cases are doubled up for three years. Working with this
    # reduced number significantly speeds up the case generation.
    if (no_obs < 1000){
        number_of_households <- max(1, no_obs)
    }
    else if (no_obs < 100000){
        number_of_households <- as.integer(no_obs / (1.8 * 5))
    }
    else{
        number_of_households <- as.integer(no_obs / (2.2 * 4))
    }

    # Prepare probabilities so that each state has a different number of observations
    state_probs <- c(3.5, 2.2, 9.6, 0.8, 21.6, 7.6, 5.0, 13.5, 15.8, 1.2, 4.4, 3.0, 1.9, 4.9, 2.6, 2.5)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate households")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "households and states")

    # Household sizes are generated with different probabilities, so that smaller numbers
    # are generated more frequently.
    persons_per_household <- sample(1:7, size = number_of_households, replace = TRUE,
                                    prob = c(0.4, 0.27, 0.15, 0.08, 0.06, 0.03, 0.01))

    # Generate unique household ids and a running number for each person inside the
    # household
    dummy_temp <- data.table::data.table(
        household_id = rep(1:number_of_households, times = persons_per_household))

    dummy_temp[["person_id"]]         <- sequence(persons_per_household)
    dummy_temp[["number_of_persons"]] <- as.integer(rep(persons_per_household, times = persons_per_household))

    # Randomly generate sixteen states
    dummy_temp[["state"]] <- as.integer(rep(sample(1:16, number_of_households,
                                                   replace = TRUE, prob = state_probs),
                                                   times   = persons_per_household))

    # Save the original number of observations to later trim the data frame to this number
    orig_obs <- no_obs
    no_obs   <- collapse::fnrow(dummy_temp)

    # Some variables will be generated on the level of households. Prepare a group for this.
    group_hh <- collapse::GRP(dummy_temp[["household_id"]])

    # Prepare variable to identify the first person per household
    dummy_temp[["first_person"]] <- data.table::fifelse(dummy_temp[["person_id"]] == 1, 1, 0)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate weights")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "weights")

    # Generate unique weights per person and also pass on the first weight per household
    # onto all other persons in the same household.
    scale_factor <- 25000 / orig_obs
    dummy_temp[["weight"]] <- stats::runif(no_obs, 2.9, 3.7) * scale_factor
    dummy_temp[["weight"]] <- dummy_temp[["weight"]] |> collapse::ffirst(g = group_hh, TRA = "fill")
    dummy_temp[["weight_per_year"]] <- dummy_temp[["weight"]] * 5

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate sex")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "sex")

    # Generate sex variable, but let each state have a different distribution
    sex_probs <- stats::runif(16, 0.3, 0.7)

    dummy_temp[["sex"]] <- 1 + stats::rbinom(no_obs, 1, sex_probs[dummy_temp[["state"]]])

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate age")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "age")

    # Generate random ages
    dummy_temp[["age"]] <- floor(stats::runif(no_obs, min = 25, max = 55))

    # Get the positions of certain persons depending on their person id
    id_1  <- which(dummy_temp[["person_id"]] == 1)
    id_34 <- which(dummy_temp[["person_id"]] %in% c(3, 4))
    id_56 <- which(dummy_temp[["person_id"]] %in% c(5, 6))

    # Bring a little variety to the age distribution by generating younger and older
    # persons in specific spots
    dummy_temp[id_1,  "age"] <- floor(stats::runif(length(id_1),  min = 18, max = 95))
    dummy_temp[id_34, "age"] <- floor(stats::runif(length(id_34), min =  0, max =  25))
    dummy_temp[id_56, "age"] <- floor(stats::runif(length(id_56), min = 50, max = 95))

    # Bring in more variety by making people younger or older depending on the state
    # they live in.
    dummy_temp[["age"]] <- data.table::fifelse(dummy_temp[["state"]] >= 11, dummy_temp[["age"]] + 5, dummy_temp[["age"]] - 5)
    dummy_temp[["age"]] <- data.table::fifelse(dummy_temp[["sex"]] == 1,    dummy_temp[["age"]] + 3, dummy_temp[["age"]] - 2)
    dummy_temp[["age"]] <- data.table::fifelse(dummy_temp[["age"]] < 0,     0,                       dummy_temp[["age"]])
    dummy_temp[["age"]] <- data.table::fifelse(dummy_temp[["age"]] > 95,    95,                      dummy_temp[["age"]])

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate income")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "income")

    # Generate random incomes and bring a bit variety to the distribution depending
    # on certain aspects like age, sex, and state.
    dummy_temp[["income"]] <- stats::runif(no_obs, 0, 5000)
    dummy_temp[["income"]] <- data.table::fifelse(dummy_temp[["age"]] < 18,                             0,                             dummy_temp[["income"]])
    dummy_temp[["income"]] <- data.table::fifelse(dummy_temp[["age"]] >= 30 & dummy_temp[["age"]] < 50, dummy_temp[["income"]] * 1.2,  dummy_temp[["income"]])
    dummy_temp[["income"]] <- data.table::fifelse(dummy_temp[["age"]] >= 50 & dummy_temp[["age"]] < 65, dummy_temp[["income"]] * 1.5,  dummy_temp[["income"]])
    dummy_temp[["income"]] <- data.table::fifelse(dummy_temp[["age"]] >= 65,                            dummy_temp[["income"]] * 0.75, dummy_temp[["income"]])
    dummy_temp[["income"]] <- data.table::fifelse(dummy_temp[["sex"]] == 1,                             dummy_temp[["income"]] * 0.8,  dummy_temp[["income"]])
    dummy_temp[["income"]] <- data.table::fifelse(dummy_temp[["state"]] >= 11,                          dummy_temp[["income"]] * 0.7,  dummy_temp[["income"]])

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate expenses and balance")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "expenses and balance")

    # Generate random expenses and bring a bit variety to the distribution depending
    # on certain aspects like age, sex, and state.
    dummy_temp[["expenses"]] <- stats::runif(no_obs, -5000, 0)
    dummy_temp[["expenses"]] <- data.table::fifelse(dummy_temp[["age"]] < 18,                             0,                               dummy_temp[["expenses"]])
    dummy_temp[["expenses"]] <- data.table::fifelse(dummy_temp[["age"]] >= 30 & dummy_temp[["age"]] < 50, dummy_temp[["expenses"]] * 1.2,  dummy_temp[["expenses"]])
    dummy_temp[["expenses"]] <- data.table::fifelse(dummy_temp[["age"]] >= 50 & dummy_temp[["age"]] < 65, dummy_temp[["expenses"]] * 1.5,  dummy_temp[["expenses"]])
    dummy_temp[["expenses"]] <- data.table::fifelse(dummy_temp[["age"]] >= 65,                            dummy_temp[["expenses"]] * 0.75, dummy_temp[["expenses"]])
    dummy_temp[["expenses"]] <- data.table::fifelse(dummy_temp[["sex"]] == 1,                             dummy_temp[["expenses"]] * 0.8,  dummy_temp[["expenses"]])
    dummy_temp[["expenses"]] <- data.table::fifelse(dummy_temp[["state"]] >= 11,                          dummy_temp[["expenses"]] * 0.7,  dummy_temp[["expenses"]])

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate income classes")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "income classes")

    dummy_temp[["income_class"]] <- as.character(cut(dummy_temp[["income"]],
        breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, Inf),
        labels = c("01.         under  500", "02.  500 to under 1000", "03. 1000 to under 1500", "04. 1500 to under 2000",
                   "05. 2000 to under 2500", "06. 2500 to under 3000", "07. 3000 to under 3500", "08. 3500 to under 4000",
                   "09. 4000 to under 4500", "10. 4500 to under 5000", "11. 5000 and more"), right = FALSE))

    dummy_temp[dummy_temp[["income"]] == 0, "income_class"] <- "00. no income"

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate probability")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "probability")

    # Generate unique probabilities per person and also pass on the first probability
    # per household onto all other persons in the same household.
    prob_temp <- sample(0:2, no_obs, replace = TRUE)

    # Probability can be 0, 1 or something in between
    id_2            <- which(prob_temp == 2)
    prob_temp[id_2] <- stats::runif(length(id_2), 0, 1)

    dummy_temp[["probability"]] <- prob_temp |> collapse::ffirst(g = group_hh, TRA = "fill")

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate education")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "education")

    male_id   <- which(dummy_temp[["sex"]] == 1)
    female_id <- which(dummy_temp[["sex"]] == 2)

    dummy_temp[male_id, "education"] <- sample(c("low", "middle", "high"),
                                               length(male_id),
                                               replace = TRUE,
                                               prob    = c(0.35, 0.4, 0.25))

    dummy_temp[female_id, "education"] <- sample(c("low", "middle", "high"),
                                                 length(female_id),
                                                 replace = TRUE,
                                                 prob    = c(0.2, 0.45, 0.35))

    # People of younger ages should all have low education
    dummy_temp[dummy_temp[["age"]] < 18, "education"] <- "low"

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate body height")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "body_height")

    # Younger people should be smaller and grow steadily
    growth_factor       <- rep(1, no_obs)
    id20                <- which(dummy_temp[["age"]] < 21)
    growth_factor[id20] <- pmin(1, ((dummy_temp[id20, "age"] + 1) / 20) ^ 0.6)

    # Generate basic body height around fixed "maximum" values
    dummy_temp[["body_height"]] <- stats::rnorm(no_obs, data.table::fifelse(dummy_temp[["sex"]] == 1,
                                                                            178, 165),
                                         7) * growth_factor

    # Generate smaller and bigger spikes in steps of five and ten
    probs <- stats::runif(no_obs)

    dummy_temp[["body_height"]] <- as.integer(
        data.table::fcase(probs < 0.5, round(dummy_temp[["body_height"]] / 10) * 10,
                          probs < 0.8, round(dummy_temp[["body_height"]] / 5) * 5,
                          default = dummy_temp[["body_height"]]))

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate body weight")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "body_weight")

    # Generate some variety in body weight
    bmi                         <- stats::rlnorm(no_obs, log(24), 0.25)
    dummy_temp[["body_weight"]] <- 5 + (bmi * (dummy_temp[["body_height"]] / 100) ^ 2)

    # Generate smaller and bigger spikes in steps of five and ten
    dummy_temp[["body_weight"]] <- as.integer(
        data.table::fcase(probs < 0.5, round(dummy_temp[["body_weight"]] / 10) * 10,
                          probs < 0.8, round(dummy_temp[["body_weight"]] / 5) * 5,
                          default = dummy_temp[["body_weight"]]))

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate NUTS3")
    #-------------------------------------------------------------------------#
    print_step("MINOR", "NUTS2 and NUTS3")

    # Read in NUTS3 codes with individual probabilities
    nuts_file            <- suppressMessages(import_data(system.file("extdata", "qol_nuts.csv", package = "qol")))
    nuts_file[["NUTS3"]] <- nuts_file |> concat("NUTS3", padding_char = "0", padding_length = 5)

    # Split up codes by state
    nuts_by_state <- split(nuts_file[["NUTS3"]],    substr(nuts_file[["NUTS3"]], 1, 2))
    prob_by_state <- split(nuts_file[["nuts_prob"]], substr(nuts_file[["NUTS3"]], 1, 2))

    # Generate subset of dummy data which only contains the first person per household.
    # Regions will only be created for them.
    region_temp <- dummy_temp |>
        collapse::fselect(c("state", "household_id", "first_person")) |>
        collapse::fsubset(first_person == 1)

    # Select random regions for the different states based on probabilities
    for (state in seq_along(1:16)){
        ids <- region_temp[["state"]] == state
        region_temp[["NUTS3"]][ids] <- sample(nuts_by_state[[state]],
                                              size    = collapse::fsum(ids),
                                              replace = TRUE,
                                              prob    = prob_by_state[[state]])
    }

    # Join regions back to all cases. Regions are joined on households so that every
    # person within a household receives the same region.
    dummy_temp <- collapse::join(dummy_temp, region_temp,
                                 on       = c("state", "household_id"),
                                 how      = "left",
                                 verbose  = FALSE,
								 overid   = 2)

    # Extract NUTS2 region from NUTS3 code
    dummy_temp[["NUTS2"]] <- substr(dummy_temp[["NUTS3"]], 3, 3)

    if (insert_na){
        #-------------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next("Insert NA values")
        #-------------------------------------------------------------------------#
        print_step("MAJOR", "Insert NA values")

        dummy_temp[["age"]]          <- collapse::na_insert(dummy_temp[["age"]],    prop = 0.05)
        dummy_temp[["sex"]]          <- collapse::na_insert(dummy_temp[["sex"]],    prop = 0.05)
        dummy_temp[["income"]]       <- collapse::na_insert(dummy_temp[["income"]], prop = 0.05)
        dummy_temp[["income_class"]] <- data.table::fifelse(is.na(dummy_temp[["income"]]), NA, dummy_temp[["income_class"]])
        dummy_temp[["expenses"]]     <- data.table::fifelse(is.na(dummy_temp[["income"]]), NA, dummy_temp[["expenses"]])
        dummy_temp[["education"]]    <- collapse::na_insert(dummy_temp[["education"]],   prop = 0.05)
        dummy_temp[["body_height"]]  <- collapse::na_insert(dummy_temp[["body_height"]], prop = 0.05)
        dummy_temp[["body_weight"]]  <- collapse::na_insert(dummy_temp[["body_weight"]], prop = 0.05)
    }

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Generate years")
    #-------------------------------------------------------------------------#
    print_step("MAJOR", "Expand years")

    # Sort and renumber household ids inside states
    dummy_temp <- dummy_temp |> data.table::setorderv(c("state", "household_id", "person_id"))
    dummy_temp[["household_id"]] <- stats::ave(dummy_temp[["household_id"]],
                                               dummy_temp[["state"]],
                                               FUN = function(case) match(case, unique(case)))

    # Prepare years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    start_year   <- current_year - 4

    years_seq <- data.table::data.table(temp. = 1,
                                        year  = start_year:current_year,
                                        age_factor      = 0:4,
                                        income_factor   = c(1, 1.05, 1.1, 1.2, 1.4),
                                        expenses_factor = c(1, 1.1, 1.15, 1.2, 1.3))

    # Create cartesian product with years to multiply cases
    dummy_temp[["temp."]] <- 1
    dummy_temp <- collapse::join(years_seq, dummy_temp,
                                 on       = "temp.",
                                 how      = "left",
                                 multiple = TRUE,
                                 verbose  = FALSE,
								 overid   = 2)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Advance values")
    #-------------------------------------------------------------------------#
    print_step("MAJOR", "Advance values in later years")

    # Up the age and income/expenses
    dummy_temp <- dummy_temp |> collapse::fmutate(age      = age      + age_factor,
                                                  income   = income   * income_factor,
                                                  expenses = expenses * expenses_factor,
                                                  balance  = income   + expenses)

    # Up the body height for people under 18
    dummy_temp[["body_height"]] <- as.integer(data.table::fifelse(
        dummy_temp[["year"]] > start_year & dummy_temp[["age"]] < 18,
        dummy_temp[["body_height"]] + (dummy_temp[["age_factor"]] * dummy_temp[["income_factor"]] * 5),
        dummy_temp[["body_height"]]))

    # Bring random variety into body weights
    dummy_temp[["body_weight"]] <- as.integer(data.table::fifelse(
        dummy_temp[["year"]] > start_year & dummy_temp[["age"]] < 18,
        dummy_temp[["body_weight"]] + (dummy_temp[["age_factor"]] * dummy_temp[["income_factor"]] * 5),
        pmax(5, dummy_temp[["body_weight"]] + sample(-10:10, no_obs, replace = TRUE))))

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Finish")
    #-------------------------------------------------------------------------#
    print_step("MAJOR", "Finish")

    random_sample <- sort(sample.int(collapse::fnrow(dummy_temp), orig_obs))

    dummy_temp <- dummy_temp |> collapse::fsubset(random_sample) |>
        keep("year", "state", "NUTS2", "NUTS3", "household_id", "person_id",
             "number_of_persons", "first_person", "age",
             "sex", "education", "body_height", "body_weight",
             "income_class", "income", "expenses", "balance", "probability",
             "weight", "weight_per_year", order_vars = TRUE)

    print_closing(5)

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #-------------------------------------------------------------------------#

    dummy_temp
}
