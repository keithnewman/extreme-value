#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)

Exponential <- R6Class(
  "Exponential",
  inherit = EVModel,
  public = list(
    
    initialize = function(data) {
      self$setData(data)
      private$DISTRIBUTION_NAME = "Exponential"
    },
    
    description = function(dataType = private$data$type) {
      tagList(
        withMathJax(
          p(sprintf(
            "The probability of %s exceeding a threshold \\(x\\)
            is given by the formula,
							$$\\mathrm{Pr}(X>x)=\\exp\\{-\\lambda x\\}\\text{,}$$
										where:", tolower(dataType)),
            tags$ul(
              tags$li("\\(\\lambda\\) is the ", em("rate"), " parameter,"),
              tags$li("\\(X\\) is our ", em("random variable"), ","),
              tags$li("\\(x\\) is the ", em("value"), " of our random variable,"),
              tags$li("\\(\\exp\\) is the ", em("exponential function"), ",")
            )
          )
        )
      )
    },
    
    exceedanceProb = function(x) {
      private$optimiseIfNeeded()
      return(1 - pexp(x, private$theta[1]))
    },
    
    # Override the usual method as parameters can be reliably found
    fit = function() {
      private$theta <- 1.0 / mean(private$data$getData())
      private$nlmCode <- 1
      private$hess <- NULL
      private$makePlotData()
      return(list(theta = private$theta,
                  code = private$nlmCode))
    },
    
    fittedParameterDescription = function(showStandardError = TRUE) {
      params <- private$theta
      se <- self$getSE()
      withMathJax(p(
        sprintf(
          "For the data you provided, we have found that \\(\\lambda=%0.3f%s\\)
					(Given to 3 decimal places).",
          params,
          ifelse(showStandardError, sprintf(" \\left(%0.3f\\right)", se), "")
        )
      ))
    },
    
    # Override the parent method
    getSE = function() {
      return(private$theta / sqrt(private$data$length()))
    },
    
    logLikelihood = function(theta, negative = FALSE) {
      assertthat::assert_that(length(theta) == 1)
      ll <- sum(dexp(private$data$getData(), theta, log = TRUE))
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
								\\exp(-%5$0.3f\\times{}%2$0.2f)
								=%6$0.4f\\text{ (to 4 decimal places).}$$",
              tolower(dataType), #1
              x, #2
              units, #3
              timeframe, #4
              params[1], #5
              self$exceedanceProb(x) #6
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
          "$$z_{%2$.0f}=%1$0.3f
						\\log\\left(%2$.0f\\right)=
						%3$0.2f%4$s\\text{ %5$s (to 2 decimal places).}$$",
          params[1], #1
          r, #2
          self$returnLevel(r), #3
          ifelse(standardError,
                 sprintf("\\ (%.2f)", self$returnLevelSE(r)),
                 ""), #4
          units #5
        )
      ))
    },
    
    returnLevel = function(r) {
      private$optimiseIfNeeded()
      return(qexp(1 - (1/r), private$theta))
    },
    
    returnLevelSE = function(r) {
      del = -(private$theta ^ (-2)) * log(r)
      hess = (private$theta ^ 2) / private$data$length()
      se <- sqrt(t(del) %*% hess %*% del)
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