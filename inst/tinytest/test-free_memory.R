set_no_print(TRUE)

test_env <- function(){
    envir <- new.env(parent = emptyenv())

    envir[["numeric1"]]   <- 1
    envir[["numeric2"]]   <- 2
    envir[["character"]]  <- "text"
    envir[["data_frame"]] <- dummy_data(10)
    envir[["another_frame"]] <- dummy_data(10)

    envir
}


# Free memory by names
env <- test_env()

removed <- free_memory(c("numeric1", "character"), envir = env)

expect_false(exists("numeric1",  envir = env), info = "Free memory by names")
expect_true(exists("numeric2",   envir = env), info = "Free memory by names")
expect_false(exists("character", envir = env), info = "Free memory by names")
expect_true(exists("data_frame", envir = env), info = "Free memory by names")

expect_equal(removed, c("numeric1", "character"), info = "Free memory by names")


# Free memory by object types
env <- test_env()

removed <- free_memory(types = "numeric", envir = env)

expect_false(exists("numeric1",  envir = env), info = "Free memory by object types")
expect_false(exists("numeric2",  envir = env), info = "Free memory by object types")
expect_true(exists("character",  envir = env), info = "Free memory by object types")
expect_true(exists("data_frame", envir = env), info = "Free memory by object types")

expect_equal(sort(removed), sort(c("numeric1", "numeric2")), info = "Free memory by object types")


# Free memory by names and object types
env <- test_env()

removed <- free_memory("numeric1", "numeric", envir = env)

expect_false(exists("numeric1",  envir = env), info = "Free memory by names and object types")
expect_true(exists("numeric2",   envir = env), info = "Free memory by names and object types")
expect_true(exists("character",  envir = env), info = "Free memory by names and object types")
expect_true(exists("data_frame", envir = env), info = "Free memory by names and object types")

expect_equal(removed, "numeric1", info = "Free memory by names and object types")


# Free memory by name ranges
env1 <- test_env()
env2 <- test_env()
env3 <- test_env()

removed1 <- free_memory("num:",   envir = env1)
removed2 <- free_memory(":frame", envir = env2)
removed3 <- free_memory(":a:",    envir = env3)

expect_equal(removed1, c("numeric1", "numeric2"), info = "Free memory by name ranges")
expect_equal(removed2, c("another_frame", "data_frame"), info = "Free memory by name ranges")
expect_equal(removed3, c("another_frame", "character", "data_frame"), info = "Free memory by name ranges")

###############################################################################
# Abort checks
###############################################################################

# Free memory aborts, if names are not characters
free_memory(names = 123, envir = test_env())
expect_error(print_stack_as_messages("ERROR"), "<Names> have to be provided as character. Objects will remain untouched.", info = "Free memory aborts, if names are not characters")


# Free memory aborts, if types are not characters
free_memory(types = 123, envir = test_env())
expect_error(print_stack_as_messages("ERROR"), "<Types> have to be provided as character. Objects will remain untouched.", info = "Free memory aborts, if types are not characters")


# Free memory aborts, if name has to many columns
free_memory(names = ":a:b:", envir = test_env())
expect_error(print_stack_as_messages("ERROR"), "<Name> has more than two colons which is not allowed. Objects will remain untouched.", info = "Free memory aborts, if name has to many columns")


# Free memory aborts, if no object is found
free_memory(names = "test", envir = test_env())
expect_error(print_stack_as_messages("ERROR"), "No object found. Objects will remain untouched.", info = "Free memory aborts, if no object is found")


set_no_print()
