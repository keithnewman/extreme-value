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