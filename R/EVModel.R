#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)

EVModel <- R6Class(
  "EVModel",
  public = list(
    
    initialize = function() {
      stop("Cant construct from parent class!")
    },

    logLikelihood = function(theta, negative = FALSE) {
      stop("Not implemented in the parent class!")
    },
    
    fit = function() {
      initialTheta <- private$theta
      m <- nlm(self$logLikelihood(), intialTheta, hessian = TRUE, negative = TRUE)
      private$theta <- m$est
      private$nlmCode <- m$code
      private$optimised <- TRUE
      private$hess <- m$hessian
      return(m)
    },
    
    getFittedTheta = function() {
      private$optimiseIfNeeded()
      return(private$theta)
    },
    
    getSE = function() {
      private$optimiseIfNeeded()
      return(sqrt(diag(solve(private$hess))))
    },
    
    setData = function(newData) {
      # TODO: Assert that the new data is of the Dataset class
      private$data = newData
    }
  ),
  private = list(
    data = NULL,
    theta = numeric(length = 0L),
    optimised = FALSE,
    nlmCode = NULL,
    hess = NULL,
    
    optimiseIfNeeded = function() {
      if (is.null(private$nlmCode)) {
        message("Model has not yet been optimised. Attempting to optimise...")
        self$fit()
        if (private$nlmCode == 1) {
          message("success!")  
        }
      }
      if (private$nlmCode > 1) {
        warning(paste("Model could not be optimised! nlm exited with code:", private$nlmCode))
      }
      return(private$nlmCode)
    }
  )
)