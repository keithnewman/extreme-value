#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)

GEV <- R6Class(
  "GEV",
  inherit = EVModel,
  public = list(
    
    initialize = function(data) {
      self$setData(data)
      private$DISTRIBUTION_NAME = "GEV"
    },
    
    description = function(dataType = private$data$type) {
      tagList(
        withMathJax(
          p(sprintf(
            "The probability of %s exceeding a threshold \\(x\\)
            is given by the formula,
							$$\\mathrm{Pr}(X>x)=1-\\exp\\left\\{-\\left[1+\\xi\\left(\\frac{x-\\mu}{\\sigma}\\right)\\right]^{-\\frac{1}{\\xi}}\\right\\}\\text{,}$$
										where:", tolower(dataType)),
            tags$ul(
              tags$li("\\(\\mu\\) is the ", em("location"), " parameter,"),
              tags$li("\\(\\sigma\\) is the ", em("scale"), " parameter,"),
              tags$li("\\(\\xi\\) is the ", em("shape"), " parameter,"),
              tags$li("\\(X\\) is our ", em("random variable"), ","),
              tags$li("\\(x\\) is the ", em("value"), " of our random variable,"),
              tags$li("\\(\\exp\\) is the ", em("exponential function"), ".")
            )
          )
        )
      )
    },
    
    exceedanceProb = function(x) {
      private$optimiseIfNeeded()
      return(1 - exp(-(1 + private$theta[3] * ((x - private$theta[1])/private$theta[2]))^(-1/private$theta[3])))
    },
    
    fittedParameterDescription = function(showStandardError = TRUE) {
      params <- super$getFittedTheta()
      se <- super$getSE()
      withMathJax(p(
        sprintf(
          "For the data you provided, we have found that \\(\\mu=%0.3f%s\\),
					\\(\\sigma=%0.3f%s\\) and \\(\\xi=%0.3f%s\\)
					(All values given to 3 decimal places).",
          params[1],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[1]), ""),
          params[2],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[2]), ""),
          params[3],
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se[3]), "")
        )
      ))
    },
    
    logLikelihood = function(theta, negative = FALSE) {
      mu <- theta[1]
      sigma <- theta[2]
      xi <- theta[3]
      m <- min((1 + (xi * (private$data$getData() - mu) / sigma)))
      delta <- sqrt(.Machine$double.eps)
      if (m < delta) return(.Machine$double.xmax)
      if (sigma < delta) return(.Machine$double.xmax)
      if (base::isTRUE(base::all.equal(xi, 0, tolerance = delta))) {
        ll = -length(private$data$getData()) * log(sigma) -
          sum((private$data$getData() - mu) / sigma) -
          sum(exp(-((private$data$getData() - mu) / sigma)))
      } else {
        ll = -length(private$data$getData()) * log(sigma) -
          (1/xi + 1)*sum(log(1 + (xi * (private$data$getData() - mu)/sigma))) -
          sum((1 + (xi * (private$data$getData() - mu)/sigma))**(-1/xi))
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
								1-\\exp\\left\\{-\\left[1+%7$0.3f
									-\\left(
										\\frac{%2$0.2f-%5$0.3f}{%6$0.3f}
									\\right)\\right]^{-\\frac{1}{%7$0.3f}}
								\\right\\}
								=%8$0.4f\\text{ (to 4 decimal places).}$$",
              tolower(dataType), #1
              x, #2
              units, #3
              timeframe, #4
              params[1], #5
              params[2], #6
              params[3], #7
              self$exceedanceProb(x) #8
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
          "$$z_{%4$.0f}=%1$0.3f+\\frac{%2$0.3f}{%3$0.3f}
					\\left\\{
						\\left[
							\\log\\left(\\frac{%4$.0f}{%4$.0f-1}\\right)
						\\right]^{-(%3$0.3f)}-1
					\\right\\}=%5$0.2f%6$s\\text{ %7$s (to 2 decimal places).}$$",
          params[1], #1
          params[2], #2
          params[3], #3
          r, #4
          self$returnLevel(r), #5
          ifelse(standardError,
                 sprintf("\\ (%.2f)", self$returnLevelSE(r)),
                 ""), #6
          units #7
        )
      ))
    },
    
    returnLevel = function(r) {
      private$optimiseIfNeeded()
      mu <- private$theta[1]
      sigma <- private$theta[2]
      xi <- private$theta[3]
      rl <- mu + ((sigma / xi) * ((log(r / (r - 1)))^(-xi) - 1))
      return(rl)
    },
    
    returnLevelSE = function(r) {
      private$optimiseIfNeeded()
      sigma <- private$theta[2]
      xi <- private$theta[3]
      h <- solve(private$hess)
      y = -log(1 - (1/r))
      del <- matrix(c(1,
                      -(xi^(-1)) * (1 - y^(-xi)),
                      (sigma * xi^(-2) * (1 - y^(-xi))) -
                        (sigma * xi^(-1) * y^(-xi) * log(y))),
                    ncol = 1, nrow = 3)
      se <- sqrt(t(del) %*% h %*% del)
      return(se)
    },
    
    setData = function(newData) {
      super$setData(newData)
      private$theta <- c(mean(private$data$getData()),
                         sd(private$data$getData()),
                         0.1)
    }
  )
)