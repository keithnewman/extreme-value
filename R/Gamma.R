#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)

Gamma <- R6Class(
  "Gamma",
  inherit = EVModel,
  public = list(
    
    initialize = function(data) {
      self$setData(data)
      private$DISTRIBUTION_NAME = "Gamma"
    },
    
    description = function(dataType = private$data$type) {
      tagList(
        withMathJax(
          p(sprintf(
            "The probability of %s exceeding a threshold \\(x\\)
            is given by the formula,
							$$\\mathrm{Pr}(X>x)=\\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)}\\int_{x}^{\\infty}{t^{\\alpha-1}\\exp\\left(-\\beta t\\right)\\mathop{dt}}\\text{,}$$
							where:", tolower(dataType))
          ),
          tags$ul(
            tags$li("\\(\\beta\\) is the ", em("rate"), " parameter,"),
            tags$li("\\(\\alpha\\) is the ", em("shape"), " parameter,"),
            tags$li("\\(X\\) is our ", em("random variable"), ","),
            tags$li("\\(x\\) is the ", em("value"), " of our random variable,"),
            tags$li("\\(\\exp\\) is the ", em("exponential function"), ".")
          )
        )
      )
    },
    
    exceedanceProb = function(x) {
      private$optimiseIfNeeded()
      return(1 - pgamma(x, private$theta[1], private$theta[2]))
    },
    
    fittedParameterDescription = function(showStandardError = TRUE) {
      params <- super$getFittedTheta()
      se <- super$getSE()
      withMathJax(p(
        sprintf(
          "For the data you provided, we have found that \\(\\alpha=%0.3f%s\\)
					and \\(\\beta=%0.3f%s\\)
					(Both values given to 3 decimal places).",
          params[1],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[1]), ""),
          params[2],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[2]), "")
        )
      ))
    },
    
    logLikelihood = function(theta, negative = FALSE) {
      alpha = theta[1]
      beta = theta[2]
      
      delta = sqrt(.Machine$double.eps)
      if (alpha < delta || beta < delta) {
          ll <- (-.Machine$double.xmax)
      } else {
        ll = private$data$length() * alpha * log(beta) -
          private$data$length() * lgamma(alpha) +
          (alpha - 1) * sum(log(private$data$getData())) -
          beta * private$data$length() * mean(private$data$getData())
      }
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
								\\frac{%6$0.3f^{%5$0.3f}}
								{\\Gamma(%5$0.3f)}
								\\int_{%2$0.2f}^{\\infty}{t^{%5$0.3f-1}
								\\exp\\left(-%6$0.3f t\\right)\\mathop{dt}}
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
          "$$z_{%3$.0f}
					=\\mathrm{F}^{-1}\\left(\\frac{1}{%3$.0f}, %1$0.3f, %2$0.3f\\right)
					=%4$0.2f\\text{ %5$s (to 2 decimal places).}$$",
          params[1], #1
          params[2], #2
          r, #3
          self$returnLevel(r), #4
          units #5
        )
      ))
    },
    
    returnLevel = function(r) {
      private$optimiseIfNeeded()
      return(qgamma(1 - (1 / r), private$theta[1], private$theta[2]))
    },
    
    returnLevelSE = function(r) {
      stop("Method not available for Gamma model!")
    },
    
    setData = function(newData) {
      super$setData(newData)
      m <- mean(private$data$getData())
      s <- sd(private$data$getData())
      private$theta <- c(m^2 / s^2, m / s^2)
    }
  )
)