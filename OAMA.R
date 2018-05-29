package <- c("compiler",
             "quantmod",
             "dygraphs",
             "plyr",
             "devtools",
             "PerformanceAnalytics",
             "doParallel")
lapply(X = package,
       FUN = function(this.package){
         if (!require(package = this.package,
                      character.only = TRUE))
         {
           install.packages(pkgs = this.package,
                            repos = "https://cloud.r-project.org")
           library(package = this.package,
                   character.only = TRUE)
         } else {
           library(package = this.package,
                   character.only = TRUE)    
         }
       })
# install_github("braverock/FinancialInstrument")
# install_github("braverock/blotter")
# install_github("braverock/quantstrat")
# install_github("braverock/PerformanceAnalytics")
require(quantstrat)

# +------------------------------------------------------------------
# | The registerDoParallel() function is used to register the 
# | parallel backend with the foreach package. detectCores() attempts
# | to detect the number of CPU cores on the current host.
# +------------------------------------------------------------------

registerDoParallel(detectCores())

# +------------------------------------------------------------------
# | Level 4:
# | - maOmega
# | - smoothTable
# +------------------------------------------------------------------

maOmega <- Vectorize(FUN = cmpfun(function(prices, n)
  {
  sma <- SMA(x = prices,
             n = n)
  R <- CalculateReturns(prices = prices)
  R <- ifelse(lag(prices) >= lag(sma), R, 0)
  value <- c(n, Omega(R = R))
  if (is.na(value) || is.nan(value))
    value <- 1
  return(value)
  }),
  vectorize.args = 'n')

# +------------------------------------------------------------------

smoothTable <- cmpfun(function(value.table)
{
  value <- tryCatch(
    expr = {
      data <- as.data.frame(value.table)
      model <- loess(formula = Omega ~ Period,
                     data = data)
      fitted <- predict(object = model,
                        newdata = data$Period)
      if (length(fitted) == nrow(data))
        data$Omega <- fitted
      return(data)
    },
    error = function(e){
      return(value.table)
    },
    finally = {}
  )
  return(value)
})

# +------------------------------------------------------------------
# | Level 3:
# | - omegaTable
# +------------------------------------------------------------------

omegaTable <- cmpfun(function(prices, min.n, max.n, by.n)
{
  value <- t(maOmega(prices = prices,
                     n = seq(from = min.n,
                             to = max.n,
                             by = by.n)))
  colnames(value) <- c('Period', 'Omega')
  value <- smoothTable(value.table = value)
  return(value)
})

# +------------------------------------------------------------------
# | Level 2:
# | - OAMA
# +------------------------------------------------------------------

OAMA <- cmpfun(function(prices, min.n, max.n, by.n)
{
  x <- omegaTable(prices = prices, 
                  min.n = min.n,
                  max.n = max.n,
                  by.n = by.n)
  n <- x[which.max(x = x[, 2]), 1]
  value <- last(SMA(x = prices,
                    n = n))
  message(last(index(prices)))
  return(value)
})

# +------------------------------------------------------------------
# | Level 1
# +------------------------------------------------------------------

runOAMA <- cmpfun(function(data, width, min.n, max.n, by.n)
{
  x <- rollapplyr(data = data,
                  width = width,
                  FUN = OAMA,
                  min.n = min.n,
                  max.n = max.n, 
                  by.n = by.n)
  value <- reclass(x = x,
                   match.to = data)
  dygraph(cbind(Cl(TLT), SMA(value, 20))) %>% dyCandlestick()
  return(value)
})

# +------------------------------------------------------------------
# | Main: Omega Adaptive Moving Average
# +------------------------------------------------------------------

Symbols <- c("SPY", "GLD", "TLT")
getSymbols(Symbols = Symbols,
           from = '1950-01-01')

