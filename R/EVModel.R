#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)
library(tibble)
library(dplyr)
library(plotly)

EVModel <- R6Class(
  "EVModel",
  public = list(
    
    initialize = function() {
      stop("Cant construct from parent class!")
    },
    
    exceedanceProb = function() {
      stop("Abstract method not implemented in parent class!")
    },

    logLikelihood = function(theta, negative = FALSE) {
      stop("Not implemented in the parent class!")
    },
    
    fit = function() {
      initialTheta <- private$theta
      m <- nlm(self$logLikelihood, initialTheta, hessian = TRUE, negative = TRUE)
      private$theta <- m$est
      private$nlmCode <- m$code
      private$hess <- m$hessian
      private$makePlotData()
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
    
    plotly = function(units = private$data$units) {
      private$optimiseIfNeeded()
      return(plot_ly(private$plotData, x = ~x, y = ~y) %>%
               add_lines(line = list(shape = "spline"),
                         name = private$DISTRIBUTION_NAME,
                         hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
                                                units,
                                                ") = %{y:.4f}")) %>%
               layout(yaxis = list(title = "Probability")))
    },
    
    probabilityCalcEquation = function() {
      stop("Abstract method not implemented in parent class!")
    },
    
    returnLevelEquation = function() {
      stop("Abstract method not implemented in parent class!")
    },
    
    tableOfProbabilities = function() {
      private$optimiseIfNeeded()
      tab <- tibble::tibble(x = private$findPrettyBreaks()) %>%
        mutate(`Probability of exceeding x` = self$exceedanceProb(x))
      return(tab)
    },
    
    returnLevel = function(r) {
      stop("Abstract method not implemented in parent class!")
    },
    
    setData = function(newData) {
      # TODO: Assert that the new data is of the Dataset class
      private$data <- newData
      private$nlmCode <- NULL
      private$plotData <- NULL
      private$hess <- NULL
      invisible(private$data)
    }
    
  ),
  private = list(
    DISTRIBUTION_NAME = NULL,
    data = NULL,
    theta = NULL,
    nlmCode = NULL,
    hess = NULL,
    plotData = NULL,
    
    optimiseIfNeeded = function() {
      if (is.null(private$nlmCode)) {
        message("Model has not yet been optimised. Attempting to optimise...")
        self$fit()
        if (private$nlmCode == 1) {
          message("...success!")
        }
      }
      if (private$nlmCode > 1) {
        warning(paste("Model could not be optimised! nlm exited with code:", private$nlmCode))
      }
      return(private$nlmCode)
    },
    
    makePlotData = function() {
      private$optimiseIfNeeded()
      private$plotData <- tibble::tibble(x = pretty(private$findPrettyBreaks(),
                                                    n = 200)) %>%
        mutate(y = self$exceedanceProb(x))
      invisible(private$plotData)
    },
    
    findPrettyBreaks = function() {
      prettyBreaks <- pretty(x = private$data$getData(),
                             n = 2 * nclass.FD(private$data$getData()))
      prettyBreakInt <- diff(prettyBreaks)[[1]]
      return(seq(from = min(prettyBreaks) - 2 * prettyBreakInt,
                 to   = max(prettyBreaks) + 3 * prettyBreakInt,
                 by   = prettyBreakInt))
    }
  )
)