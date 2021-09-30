#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)
library(shiny)
library(numDeriv)

Normal <- R6Class(
  "Normal",
  inherit = EVModel,
  public = list(
    
    initialize = function(data) {
      self$setData(data)
      private$DISTRIBUTION_NAME = "Normal"
    },
    
    description = function(dataType = private$data$type) {
      tagList(
        withMathJax(
          p(sprintf(
            "The probability of %s exceeding a threshold \\(x\\)
              is given by the formula,
							$$\\mathrm{Pr}(X>x)=1-\\Phi\\left(\\frac{x-\\mu}{\\sigma}\\right)=\\frac{1}{\\sqrt{2\\pi}}\\int_{\\frac{x-\\mu}{\\sigma}}^{\\infty}{\\exp\\left(-\\frac{t^2}{2}\\right)\\mathop{dt}}\\text{,}$$
							where:", tolower(dataType)),
            tags$ul(
              tags$li("\\(\\mu\\) is the ", em("mean"), " parameter,"),
              tags$li("\\(\\sigma\\) is the ", em("standard deviation"), " parameter,"),
              tags$li("\\(X\\) is our ", em("random variable"), ","),
              tags$li("\\(x\\) is the ", em("value"), " of our random variable,"),
              tags$li("\\(\\exp\\) is the ", em("exponential function"), ","),
              tags$li("\\(\\Phi\\) is the ", em("cumulative distribution function"), "of the standard Normal distribution, which can be found in statistical tables.")
            )
          )
        )
      )
    },
    
    exceedanceProb = function(x) {
      private$optimiseIfNeeded()
      return(1 - pnorm(x, private$theta[1], private$theta[2]))
    },
    
    # Override the usual method as parameters can be reliably found
    fit = function() {
      private$theta <- c(mean(private$data$getData()),
                         sd(private$data$getData()))
      private$nlmCode <- 1
      private$hess <- numDeriv::hessian(func = self$logLikelihood,
                                        x = private$theta,
                                        negative = TRUE)
      private$makePlotData()
      return(list(theta = private$theta,
                  code = private$nlmCode,
                  hess = private$hess))
    },
    
    fittedParameterDescription = function(showStandardError = TRUE) {
      params <- private$theta
      se <- super$getSE()
      withMathJax(p(
        sprintf(
          "For the data you provided, we have found that \\(\\mu=%0.3f%s\\) and
					\\(\\sigma=%0.3f%s\\) (Both values given to 3 decimal places).",
          params[1],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[1]), ""),
          params[2],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[2]), "")
        )
      ))
    },
    
    logLikelihood = function(theta, negative = FALSE) {
      ll <- sum(dnorm(private$data$getData(), theta[1], theta[2], log = TRUE))
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
								1-\\Phi\\left(\\frac{%2$0.2f-%5$0.3f}{%6$0.3f}\\right)
								=%7$0.4f\\text{ (to 4 decimal places).}$$",
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
          "$$z_{%3$.0f}=%2$0.3f\\times
						\\Phi^{-1}\\left(\\frac{1}{%3$.0f}\\right)+%1$0.3f=
						%4$0.2f%5$s\\text{ %6$s (to 2 decimal places).}$$",
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
      return(qnorm(1 - (1/r), private$theta[1], private$theta[2]))
    },
    
    returnLevelSE = function(r) {
      private$optimiseIfNeeded()
      h <- solve(private$hess)
      return(sqrt(diag(h)))
    },
    
    setData = function(newData) {
      super$setData(newData)
      self$fit()
    }
  )
)