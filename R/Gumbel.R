#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)
library(shiny)

Gumbel <- R6Class(
  "Gumbel",
  inherit = EVModel,
  public = list(
    
    initialize = function(data) {
      self$setData(data)
      private$DISTRIBUTION_NAME = "Gumbel"
    },
    
    description = function(dataType = private$data$type) {
      tagList(
        withMathJax(
          p(sprintf(
            "The probability of %s exceeding a threshold \\(x\\)
            is given by the formula,
							$$\\mathrm{Pr}(X > x) =
							  1 - \\exp\\left\\{
							    -\\exp\\left[
							      -\\left(
							        \\frac{x - \\mu}{\\sigma}
							      \\right)
							   \\right]
							 \\right\\}\\text{,}$$
						where:", tolower(dataType))),
          tags$ul(
            tags$li("\\(\\mu\\) is the ", em("location"), " parameter,"),
            tags$li("\\(\\sigma\\) is the ", em("scale"), " parameter,"),
            tags$li("\\(X\\) is our ", em("random variable"), ","),
            tags$li("\\(x\\) is the ", em("value"), " of our random variable,"),
            tags$li("\\(\\exp\\) is the ", em("exponential function"), ".")
          )
        )
      )
    },
    
    exceedanceProb = function(x) {
      private$optimiseIfNeeded()
      return(1 - exp(-exp(-((x - private$theta[1]) / private$theta[2]))))
    },
    
    fittedParameterDescription = function(showStandardError = TRUE) {
      params <- super$getFittedTheta()
      se <- super$getSE()
      withMathJax(p(
        sprintf(
          "For the data you provided, we have found that \\(\\mu=%0.3f%s\\)
					and \\(\\sigma=%0.3f%s\\) (Both values given to 3 decimal places).",
          params[1],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[1]), ""),
          params[2],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[2]), "")
        )
      ))
    },
    
    logLikelihood = function(theta, negative = FALSE) {
      mu <- theta[1]
      sigma <- theta[2]
      ll <- -length(private$data$getData()) * log(sigma) -
              sum(exp(-((private$data$getData() - mu) / sigma))) -
              sum((private$data$getData() - mu) / sigma)
      if (negative) {
        ll <- (-ll)
      }
      return(ll)
    },
    
    probabilityCalcEquation = function(x,
                                       timeframe,
                                       units = private$data$units,
                                       dataType = private$data$type) {
      params <- self$getFittedTheta()
      return(
        withMathJax(
          p(
            sprintf(
              "The probability of observing a %1$s greater than
							\\(x=%2$0.2f\\) %3$s every %4$s is given by
							$$\\mathrm{Pr}(X>%2$0.2f)=
								1-\\exp\\left[
									-\\exp\\left\\{
										-\\left(
											\\frac{%2$0.2f-%5$0.3f}{%6$0.3f}
										\\right)
									\\right\\}
								\\right]=%7$0.4f\\text{ (to 4 decimal places).}$$",
              tolower(dataType), #1
              x, #2
              units, #3
              timeframe, #4
              params[1], #5
              params[2], #6
              self$exceedanceProb(x) #7
            )
          )
        )
      )
    },
    
    returnLevelEquation = function(r,
                                   standardError = TRUE,
                                   units = private$data$units) {
      params <- self$getFittedTheta()
      return(withMathJax(
        sprintf(
          "$$z_{%3$.0f}=%1$0.3f-%2$0.3f\\log\\left[
						-\\log\\left(1-\\frac{1}{%3$.0f}\\right)
					\\right]=%4$0.2f%5$s\\text{ %6$s (to 2 decimal places).}$$",
          params[1], #1
          params[2], #2
          r, #3
          self$returnLevel(r), #4
          ifelse(standardError,
                 sprintf("\\ (%.2f)", self$returnLevelSE(r)),
                 ""), #5
          units #6
        )
      ))
    },
    
    returnLevel = function(r) {
      private$optimiseIfNeeded()
      rl <- private$theta[1] - private$theta[2] * log(-log(1 - (1/r)))
      return(rl)
    },
    
    returnLevelSE = function(r) {
      private$optimiseIfNeeded()
      h <- solve(private$hess)
      del <- matrix(c(1, -log(-log(1 - (1/r)))), ncol = 1, nrow = 2)
      se <- sqrt(t(del) %*% h %*% del)
      return(se)
    },
    
    setData = function(newData) {
      super$setData(newData)
      private$theta <- c(mean(private$data$getData()),
                       sd(private$data$getData()))
    }
  )
)