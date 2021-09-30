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