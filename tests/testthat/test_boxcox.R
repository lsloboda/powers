context("Squaring non-numerics")

test_that("At least numeric values work.", {
    num_vec <- c(0, -4.6, 3.4)
    expect_identical(square(numeric(0)), numeric(0))
    expect_identical(square(1:3), c(1,4,9))
})

test_that("Logicals automatically convert to numeric.", {
    logic_vec <- c(TRUE, TRUE, FALSE)
    expect_identical(square(logic_vec), c(1,1,0))
})

#don't need to manually run this script; bc it's called test_something, it will run automatically with testthat
#to test it, go to Build > More > Test or Check (will not be captured in the normal Check!)
#good practice to write test scripts and check if you have any mistakes!
