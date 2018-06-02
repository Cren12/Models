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
# | Level 1:
# | - expWPR
# +------------------------------------------------------------------

expWPR <- cmpfun(function(HLC, n)
{
  x <- rep(NA, nrow(HLC))
  l_ply(.data = n:nrow(HLC),
        .fun = function(i){
          x[i] <<- last(WPR(HLC = HLC[1:i, ],
                            n = nrow(HLC[1:i, ])))
        },
        .progress = 'text')
  value <- reclass(x = x,
                   match.to = HLC)
  return(value)
})

osTotEq <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  if (orderqty == "all" && !(ruletype %in% c("exit", "risk")) || 
      orderqty == "trigger" && ruletype != "chain") {
    stop(paste("orderqty 'all'/'trigger' would produce nonsense, maybe use osMaxPos instead?\n", 
               "Order Details:\n", "Timestamp:", timestamp, "Qty:", 
               orderqty, "Symbol:", symbol))
  }
  endEq <- getEndEq(Account = portfolio,
                    Date = timestamp) / length(Symbols)
  refPrice <- Cl(mktdata)[timestamp, ]
  orderqty <- floor(endEq / refPrice) 
  return(orderqty)
}

# +------------------------------------------------------------------
# +------------------------------------------------------------------
# | A speculative approach to financial markets
# +------------------------------------------------------------------
# +------------------------------------------------------------------

Symbols <- c("LQD",
             "HYG",
             "SPY",
             "GLD",
             "TLT",
             "XLF",
             "XLV",
             "DIA")
getSymbols(Symbols = Symbols,
           from = Sys.Date() - 365 * 30)

# +------------------------------------------------------------------
# | Parameters
# +------------------------------------------------------------------

name <- 'Trading'
currency <- 'USD'
initEq <- 10000 * length(Symbols)
TxnFees <- -4
Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | Inizialization
# +------------------------------------------------------------------

rm.strat(name = name)
initPortf(name = name,
          symbols = Symbols,
          currency = currency)
initAcct(name = name,
         portfolios = name,
         initEq = initEq)
initOrders(portfolio = name,
           symbols = Symbols)
currency(primary_id = currency)
strategy(name = name,
         store = TRUE)
for(primary_id in Symbols)
{
  stock(primary_id = primary_id,
        currency = currency)
}

# +------------------------------------------------------------------
# | Indicators
# +------------------------------------------------------------------

add.indicator(strategy = name,
              name = 'volatility',
              arguments = list(OHLC = quote(OHLC(mktdata)),
                               n = 5,
                               calc = 'yang.zhang',
                               N = 1),
              label = 'sigma',
              store = TRUE)
add.indicator(strategy = name,
              name = 'WPR',
              arguments = list(HLC = quote(HLC(mktdata)),
                               n = 14),
              label = 'wpr',
              store = TRUE)
# add.indicator(strategy = name,
#               name = 'expWPR',
#               arguments = list(HLC = quote(HLC(mktdata)),
#                                n = 250),
#               label = 'wpr',
#               store = TRUE)

# +------------------------------------------------------------------
# | Signals
# +------------------------------------------------------------------

add.signal(strategy = name,
           name = 'sigThreshold',
           arguments = list(label = 'wpr',
                            column = 'wpr',
                            threshold = .2,
                            relationship = 'gte',
                            cross = TRUE),
           label = 'wpr.buy',
           store = TRUE)

# +------------------------------------------------------------------
# | Rules
# +------------------------------------------------------------------

add.rule(strategy = name,
         name = 'ruleSignal',
         arguments = list(sigcol = 'wpr.buy',
                          sigval = TRUE,
                          orderqty = 1,
                          ordertype = 'market',
                          orderside = 'long',
                          osFUN = osTotEq,
                          TxnFees = TxnFees),
         label = 'wpr.buy.enter',
         type = 'enter',
         store = TRUE)
add.rule(strategy = name,
         name = 'ruleSignal',
         arguments = list(sigcol = 'wpr.buy',
                          sigval = TRUE,
                          orderqty = 'all',
                          ordertype = 'stoptrailing',
                          orderside = 'long',
                          orderset = 'stop',
                          threshold = quote(-mktdata[timestamp, 'X1.sigma']),
                          tmult = TRUE,
                          TxnFees = TxnFees),
         label = 'wpr.buy.stoptrailing',
         type = 'chain',
         parent = 'wpr.buy.enter',
         store = TRUE)

# +------------------------------------------------------------------
# | Performance Analysis
# +------------------------------------------------------------------

applyStrategy(strategy = name,
              portfolios = name)
updatePortf(Portfolio = name,
            Symbols = Symbols)
for (symbol in Symbols)
{
  dev.new()
  try(chart.Posn(Portfolio = name,
                 Symbol = symbol))
}
R <- PortfReturns(Account = name)
R$Tot.DailyEqPl <- rowSums(R)
charts.PerformanceSummary(R = R,
                          ylog = TRUE,
                          main = 'Trading performance')
getOrderBook(portfolio = name)
