library(R6)

StructBreakEstimator = R6Class(classname = 'StructBreakEstimator',
                      
                      public = list(
                        
                        pars_curve = NULL,
                        
                        initialize = function()
                        {
                          packages = c('foreach',
                                       'quantmod',
                                       'Rcpp',
                                       'strucchange')
                          lapply(X = packages,
                                 FUN = function(package){
                                   if (!require(package = package,
                                                character.only = TRUE))
                                   {
                                     install.packages(pkgs = package,
                                                      repos = "https://cloud.r-project.org")
                                     library(package = package,
                                             character.only = TRUE)
                                   } else {
                                     library(package = package,
                                             character.only = TRUE)    
                                   }
                                 })
                          sourceCpp('coupling_regression.cpp')
                        },
                        
                        make_pars_curve = function(X, fun, ...)
                        {
                          private$X = X
                          pars_curve = foreach(width = 1:nrow(X), .combine = rbind.data.frame) %do%
                            {
                              tryCatch(
                                expr = {
                                  X_ = tail(X, width)
                                  est = fun(X_, ...)
                                  return( cbind(width, est) )
                                }, 
                                error = function(e)
                                { 
                                  value = cbind(NA, NA)
                                  colnames(value) = c('width', 'est')
                                  return( value )
                                }
                              )
                            }
                          self$pars_curve = as.data.frame(pars_curve)
                          return( self )
                        },
                        
                        coupling_regression = function(min_sample = 25, max_sample = 750)
                        {
                          pars_curve = na.omit(self$pars_curve)
                          max_sample = min(max_sample, nrow(pars_curve))
                          pars_curve = pars_curve[order(pars_curve$width, decreasing = TRUE), ]
                          x = pars_curve$est
                          y = pars_curve$width
                          coupling_coefficients = coupling_regression(y, cbind(x, 1), min_sample, max_sample)
                          idx_max = which.max(coupling_coefficients[, 2])
                          private$memory = c(coupling_coefficients[idx_max, 1], pars_curve$est[idx_max])
                          names(private$memory) = c('Memory', 'Est')
                          return( self )
                        },
                        
                        smooth_curve = function(min_sample = 1, max_sample = 750, ...)
                        {
                          pars_curve = self$pars_curve
                          pars_curve = pars_curve[order(pars_curve$width, decreasing = FALSE), ]
                          pars_curve = pars_curve[min_sample:max_sample, ]
                          pars_curve_fun = loess(formula = est ~ width, data = pars_curve, ...)
                          max_curve = findPeaks(pars_curve_fun$fitted)
                          min_curve = findValleys(pars_curve_fun$fitted)
                          idx = min(max_curve, min_curve)
                          private$memory = c(idx, pars_curve$est[idx])
                          names(private$memory) = c('Memory', 'Est')
                          return( self )
                        },
                        
                        fit = function(X, fun, ...)
                        {
                          self$make_pars_curve(X, fun, ...)$smooth_curve()
                          return( self )
                        },
                        
                        predict = function()
                        {
                          return( private$memory )
                        }
                        
                      ),
                      
                      private = list(
                        
                        memory = NULL,
                        X = NULL
                        
                      ))
