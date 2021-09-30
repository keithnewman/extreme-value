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
    
    exceedanceProb = function(x) {
      private$optimiseIfNeeded()
      return(1 - exp(-(1 + xi * ((x - private$theta[1])/private$theta[2]))^(-1/private$theta[3])))
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
                    ncol = 1, nrow = 2)
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