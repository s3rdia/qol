test_env <- function(){
    envir <- new.env(parent = emptyenv())

    envir[["numeric1"]]   <- 1
    envir[["numeric2"]]   <- 2
    envir[["character"]]  <- "text"
    envir[["data_frame"]] <- dummy_data(10)
    envir[["another_frame"]] <- dummy_data(10)

    envir
}


test_that("Free memory by names", {
    env <- test_env()

    removed <- free_memory(c("numeric1", "character"), envir = env)

    expect_false(exists("numeric1",  envir = env))
    expect_true(exists("numeric2",   envir = env))
    expect_false(exists("character", envir = env))
    expect_true(exists("data_frame", envir = env))

    expect_equal(removed, c("numeric1", "character"))
})


test_that("Free memory by object types", {
    env <- test_env()

    removed <- free_memory(types = "numeric", envir = env)

    expect_false(exists("numeric1",  envir = env))
    expect_false(exists("numeric2",  envir = env))
    expect_true(exists("character",  envir = env))
    expect_true(exists("data_frame", envir = env))

    expect_setequal(removed, c("numeric1", "numeric2"))
})


test_that("Free memory by names and object types", {
    env <- test_env()

    removed <- free_memory("numeric1", "numeric", envir = env)

    expect_false(exists("numeric1",  envir = env))
    expect_true(exists("numeric2",   envir = env))
    expect_true(exists("character",  envir = env))
    expect_true(exists("data_frame", envir = env))

    expect_equal(removed, "numeric1")
})


test_that("Free memory by name ranges", {
    env1 <- test_env()
    env2 <- test_env()
    env3 <- test_env()

    removed1 <- free_memory("num:",   envir = env1)
    removed2 <- free_memory(":frame", envir = env2)
    removed3 <- free_memory(":a:",    envir = env3)

    expect_equal(removed1, c("numeric1", "numeric2"))
    expect_equal(removed2, c("another_frame", "data_frame"))
    expect_equal(removed3, c("another_frame", "character", "data_frame"))
})

###############################################################################
# Abort checks
###############################################################################

test_that("Free memory aborts, if names are not characters", {
    expect_message(free_memory(names = 123, envir = test_env()),
                    " X ERROR: <Names> have to be provided as character. Objects will remain untouched.")
})


test_that("Free memory aborts, if types are not characters", {
    expect_message(free_memory(types = 123, envir = test_env()),
                    " X ERROR: <Types> have to be provided as character. Objects will remain untouched.")
})


test_that("Free memory aborts, if name has to many columns", {
    expect_message(free_memory(names = ":a:b:", envir = test_env()),
                    " X ERROR: <Name> has more than two colons which is not allowed. Objects will remain untouched.")
})

test_that("Free memory aborts, if no object is found", {
    expect_message(free_memory(names = "test", envir = test_env()),
                    " X ERROR: No object found. Objects will remain untouched.")
})
