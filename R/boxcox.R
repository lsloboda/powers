#' Simplified Box-Cox Transformation
#'
#' This function applies an estimate of the power transformation factor (lambda) in a deterministic power function to turn non-normal data into normal data.
#' However, since lamba is user-defined in this function, it may not necessarily result in a normal distribution. A more complex method, such as
#' maximum likelihood estimate (MLE) or Newton-Raphson, may be used to estimate the value of lamba required to achieve a Gaussian distribution, however
#' this is beyond the scope of this assignment.
#'
#' @param x Initial data set.
#' @param lambda Estimate of power transformation factor to make \code{x} a normal distribution.
#' @param delta Amount to shift x by to ensure positive values.
#' @param plot_it Display a plot of \code{x} vs the output. Use logical.
#' @return The vector \code{x}, transformed by the Box-Cox transformation.

boxcox <- function(x, lambda, delta, plot_it) {
    #Test if x >0; if not, add a shift then proceed
    if (delta != 0){
        #Find min(x) -> take absolute value to get difference between min(x) and 0
        #Add delta to x
        x <- x + delta
    }

    #Transform y
    #If abs(lambda) > 3; then return error (this is to stop inappropriate use of the function, which is meant for only small transformations)

    #Apply Box-Cox transformation
    if (lambda == 0){
        y <- log(x)
    } else {
        y <- ((x^lambda) - 1) / lambda
    }

    #Create plot
    if (plot_it) print(
        ggplot2::qplot(x, y)
        )

    return(y)
}

#' @rdname boxcox
#' @export

#Test 1: Check that negative x is corrected to positive values. (I.e. see that the shift value does what is expected)
#Test 2: Test that lamba = 0  applies the correct function
#Test 3: Test that lambda != 0 applies the correct function
#Test 4: Could compare it to the BoxCox function already in MASS

#Test data set: mtcars
#lm_model <- lm(mpg ~ cyl, data = mtcars)
#plot(lm_model)

#y <- boxcox(mtcars$cyl, -1, 0, FALSE)
#boxcox_model <- lm(y ~ cyl, data = mtcars)
#plot(boxcox_model)

