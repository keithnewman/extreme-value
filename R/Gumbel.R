#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)

Gumbel <- R6Class(
  "Gumbel",
  inherit = EVModel,
  public = list(
    
    initialize = function(data) {
      self$setData(data)
      private$DISTRIBUTION_NAME = "Gumbel"
    },
    
    exceedanceProb = function(x) {
      private$optimiseIfNeeded()
      return(1 - exp(-exp(-((x - private$theta[1]) / private$theta[2]))))
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