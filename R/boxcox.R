#' Simplified Box-Cox Transformation
#'
#' This function applies an estimate of the power transformation factor (lambda) in a deterministic power function to turn non-normal data into normal data.
#' However, since lamba is user-fed in this function, it may not necessarily result in a normal distribution. A more complex method such as
#' maximum likelihood estimate (MLE) or Newton-Raphson may be used to estimate the value of lamba required to achieve a Gaussian distribution, however
#' this is beyond the scope of this assignment.
#'
#' @param x Vector to raise to some power.
#' @param lambda Power to raise \code{x} by.
#' @param delta Amount to shift x by to ensure positive values
#' @param plot_it Display a plot of \code{x} vs the output? Use logical.
#'
#' @return The vector \code{x}, raised to the power of \code{a}.
boxcox <- function(x, lambda, delta, plot_it) {
    #Test if x >0; if not, add a shift then proceed
    #If delta not equal zero, x = x + delta

    #Transform y
    #If abs(lambda) > 3; then return error (this is to stop inappropriate use of the function, which is meant for only small transformations)

    #If lambda = 0
    y <- log(x)

    #Else (lambda not equal 0)
    y <- ((x ** lamda) - 1.) / lambda

    #Create plot
    if (plot_it) print(ggplot2::qplot(x, y))

    return(y)
}



#The Box-Cox transformation is used to make a non-normal distribution into a normal distribution, allowing normal-dependent tests (e.g. t-tests) to be conducted on the data.
#It plots the exponent of the Box-Cox formula against the standard deviation.
#Different methods of applying the transformation apply to different scenarios, therefore implementing the entire Box-Cox algorithm would require a family of functions.
#For simplification, only a specific transformation function will be provided in this example.

#General Box-Cox Method
            #1. Estimate the power transformation factor (lambda) using the maximum likelihood estimatiom (MLE) method:
            #i) Select a sequence of candidate values at a regular interval (e.g. 1/10)
            #ii) Check that every element is positive ELSE add a shift.
            #iii) Apply transformation to obtain several lambdas
            #iv) Check normality using a goodness of fit test
            #v) Select lambda for max or min test
            #vi) Check if normality is satisfied using a selected test
            #vii) If none found, increase range of lambda and repeat procedure

            #2. Apply deterministic power function to the data using the estimated lambda.
