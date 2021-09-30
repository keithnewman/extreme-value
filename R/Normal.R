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