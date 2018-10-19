package <- c("compiler",
             "quantmod",
             "plyr",
             "devtools",
             "PerformanceAnalytics")

# +------------------------------------------------------------------
# | library and require load and attach add-on packages. Download and
# | install packages from CRAN-like repositories.
# +------------------------------------------------------------------

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
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

source('WinDoPar.R')
source('LOESS_trendIndicator.R')

# +------------------------------------------------------------------

osVarSize <- function(
  data, 
  timestamp, 
  orderqty, 
  ordertype, 
  orderside, 
  portfolio,
  symbol, 
  ruletype, 
  digits = 0,
  acct.name,
  ...
)
{
  if (orderqty == "all" && !(ruletype %in% c("exit", "risk")) || 
      orderqty == "trigger" && ruletype != "chain") {
    stop(paste("orderqty 'all'/'trigger' would produce nonsense, maybe use osMaxPos instead?\n", 
               "Order Details:\n", "Timestamp:", timestamp, "Qty:", 
               orderqty, "Symbol:", symbol))
  }
  
  # +------------------------------------------------------------------
  # | The updatePortf function goes through each symbol and calculates
  # | the PL for each period prices are available.
  # +------------------------------------------------------------------
  
  updatePortf(Portfolio = portfolio,
              Symbols = symbol)
  
  # +------------------------------------------------------------------
  # | Constructs the equity account calculations from the portfolio 
  # | data and corresponding close prices.
  # +------------------------------------------------------------------
  
  updateAcct(name = acct.name)
  
  # +------------------------------------------------------------------
  # | Calculates End.Eq and Net.Performance.
  # +------------------------------------------------------------------
  
  updateEndEq(Account = acct.name)
  
  # +------------------------------------------------------------------
  # | Get a portfolio object conssting of either a nested list 
  # | (getPortfolio).
  # +------------------------------------------------------------------
  
  portfolio.object <- blotter::getPortfolio(portfolio)
  
  # +------------------------------------------------------------------
  # | Retrieves an account object from the .blotter environment. Useful
  # | for local examination or charting, or storing interim results for
  # | later reference.
  # +------------------------------------------------------------------
  
  account <- getAccount(acct.name)
  
  equity <- as.numeric(account$summary$End.Eq[as.character(timestamp), ])
  if (length(equity) == 0)
  {
    equity <- as.numeric(account$summary$End.Eq[1, ])
  }
  avail.liq <- equity - as.numeric(portfolio.object$summary$Gross.Value[as.character(timestamp), ])
  if (length(avail.liq) == 0)
  {
    avail.liq <- equity
  }
  theor.value <- ifelse(is.na(mktdata[timestamp, 5]), 0, mktdata[timestamp, 5] * equity)
  pos.qty <- max(c(0, as.numeric(portfolio.object$symbols[[symbol]]$posPL$Pos.Qty[as.character(timestamp), ])))
  pos.avg.cost <- max(c(0, as.numeric(portfolio.object$symbols[[symbol]]$posPL$Pos.Avg.Cost[as.character(timestamp), ])))
  pos.value <- pos.qty * pos.avg.cost
  to.trade.value <- theor.value - pos.value
  to.trade.value <- ifelse(to.trade.value > 0, min(c(avail.liq, to.trade.value)), to.trade.value)
  to.trade.shares <- ifelse(to.trade.value >= 0, floor(to.trade.value / Cl(mktdata[timestamp, ])), floor(to.trade.value / Cl(mktdata[timestamp, ])))
  orderqty <- to.trade.shares
  return(orderqty)
}

# +------------------------------------------------------------------

osTotSize <- function(
  data, 
  timestamp, 
  orderqty, 
  ordertype, 
  orderside, 
  portfolio,
  symbol, 
  ruletype, 
  digits = 0,
  acct.name,
  ...
)
{
  if (orderqty == "all" && !(ruletype %in% c("exit", "risk")) || 
      orderqty == "trigger" && ruletype != "chain") {
    stop(paste("orderqty 'all'/'trigger' would produce nonsense, maybe use osMaxPos instead?\n", 
               "Order Details:\n", "Timestamp:", timestamp, "Qty:", 
               orderqty, "Symbol:", symbol))
  }
  
  # +------------------------------------------------------------------
  # | The updatePortf function goes through each symbol and calculates
  # | the PL for each period prices are available.
  # +------------------------------------------------------------------
  
  updatePortf(Portfolio = portfolio,
              Symbols = symbol)
  
  # +------------------------------------------------------------------
  # | Constructs the equity account calculations from the portfolio 
  # | data and corresponding close prices.
  # +------------------------------------------------------------------
  
  updateAcct(name = acct.name)
  
  # +------------------------------------------------------------------
  # | Calculates End.Eq and Net.Performance.
  # +------------------------------------------------------------------
  
  updateEndEq(Account = acct.name)
  
  # +------------------------------------------------------------------
  # | Get a portfolio object conssting of either a nested list 
  # | (getPortfolio).
  # +------------------------------------------------------------------
  
  portfolio.object <- blotter::getPortfolio(portfolio)
  
  # +------------------------------------------------------------------
  # | Retrieves an account object from the .blotter environment. Useful
  # | for local examination or charting, or storing interim results for
  # | later reference.
  # +------------------------------------------------------------------
  
  account <- getAccount(acct.name)
  
  equity <- as.numeric(account$summary$End.Eq[as.character(timestamp), ])
  if (length(equity) == 0)
  {
    equity <- as.numeric(account$summary$End.Eq[1, ])
  }
  avail.liq <- equity - as.numeric(portfolio.object$summary$Gross.Value[as.character(timestamp), ])
  if (length(avail.liq) == 0)
  {
    avail.liq <- equity
  }
  to.trade.value <- avail.liq
  to.trade.value <- ifelse(to.trade.value > 0, min(c(avail.liq, to.trade.value)), to.trade.value)
  to.trade.shares <- ifelse(to.trade.value >= 0, floor(to.trade.value / Cl(mktdata[timestamp, ])), ceiling(to.trade.value / Cl(mktdata[timestamp, ])))
  orderqty <- to.trade.shares
  return(orderqty)
}

# +------------------------------------------------------------------

Symbols <- c("LQD",
             "HYG",
             "SPY",
             "GLD",
             "TLT",
             "XLF",
             "XLV",
             "DIA")

require(Rblpapi)
require(foreach)
con <- blpConnect()
ts <- bdh(securities = paste(Symbols, 'US Equity'),
          fields = c('PX_OPEN', 'PX_HIGH', 'PX_LOW', 'PX_LAST'),
          start.date = Sys.Date() - 365* 30)
foreach(i = 1:length(ts)) %do%
{
  ts.xts <- xts(x = ts[[i]][, -1],
                order.by = ts[[i]][, 1])
  colnames(ts.xts) <- c('Open', 'High', 'Low', 'Close')
  assign(x = Symbols[i],
         value = ts.xts)
  return(Symbols[i])
}

# +------------------------------------------------------------------
# | Functions to load and manage Symbols in specified environment. 
# | Called for its side-effect with env set to a valid environment 
# | and auto.assign=TRUE, getSymbols will load into the specified env
# | one object for each Symbol specified, with class defined by 
# | return.class.
# +------------------------------------------------------------------

# getSymbols(Symbols = Symbols,
#            from = Sys.Date() - 365 * 30)

# for (symbol in Symbols)
# {
#   # +------------------------------------------------------------------
#   # | Search by name for an object. The object found is returned.
#   # +------------------------------------------------------------------
#   
#   symbol.data <- get(symbol)
#   
#   # +------------------------------------------------------------------
#   # | Adjust all columns of an OHLC object for split and dividend.
#   # +------------------------------------------------------------------
#   
#   symbol.data.adj <- adjustOHLC(x = symbol.data,
#                                 use.Adjusted = TRUE)
#   
#   # +------------------------------------------------------------------
#   # | Assign a value to a name in an environment. This function is 
#   # | invoked for its side effect, which is assigning value to the 
#   # | variable x.
#   # +------------------------------------------------------------------
#   
#   assign(x = symbol,
#          value = symbol.data.adj)
# }

name <- 'Trading'
currency <- 'USD'
initEq <- 100000 * length(Symbols)
TxnFees <- 0
Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | Remove the order_book, account, and portfolio of given name.
# +------------------------------------------------------------------

rm.strat(name = name)

# +------------------------------------------------------------------
# | Constructs and initializes a portfolio object, which is used to
# | contain transactions, positions, and aggregate level values.
# +------------------------------------------------------------------

initPortf(name = name,
          symbols = Symbols,
          currency = currency)

# +------------------------------------------------------------------
# | Inputs portfolios: a list of portfolio object names to attach to
# | the account. initDate: date prior to the first close price given,
# | used to contain initial account equity and initial position.
# | initEq: initial equity or starting capital, default is 100,000.
# +------------------------------------------------------------------

initAcct(name = name,
         portfolios = name,
         initEq = initEq)

# +------------------------------------------------------------------
# | This function sets up the order container by portfolio.
# +------------------------------------------------------------------

initOrders(portfolio = name,
           symbols = Symbols)

# +------------------------------------------------------------------
# | All 'currency' instruments must be defined before instruments of
# | other types may be defined.
# +------------------------------------------------------------------

currency(primary_id = currency)

# +------------------------------------------------------------------
# | Variables passed in dots will be added to the strategy object, 
# | and may be used by initialization and wrapup functions, as well 
# | as indicators, signals, and rules.
# +------------------------------------------------------------------

strategy(name = name,
         store = TRUE)

for(primary_id in Symbols)
{
  stock(primary_id = primary_id,
        currency = currency)
}

# +------------------------------------------------------------------
# | Indicators are typically standard technical or statistical
# | analysis outputs, such as moving averages, bands, or pricing 
# | models.
# +------------------------------------------------------------------

add.indicator(strategy = name,
              name = 'WinDoPar',
              arguments = list(x = quote(OHLC(mktdata)),
                               n = 500,
                               w = 'run',
                               fun = LOESS_trendIndicator),
              label = 'pti',
              store = TRUE)
add.indicator(strategy = name,
              name = 'volatility',
              arguments = list(OHLC = quote(OHLC(mktdata)),
                               n = 5,
                               calc = 'yang.zhang',
                               N = 3),
              label = 'sigma',
              store = TRUE)

# +------------------------------------------------------------------
# | This adds a signal definition to a strategy object.
# +------------------------------------------------------------------

add.signal(strategy = name,
           name = 'sigThreshold',
           arguments = list(column = 'pti',
                            threshold = 0,
                            relationship = 'gte',
                            cross = FALSE),
           label = 'pti.buy',
           store = TRUE)
add.signal(strategy = name,
           name = 'sigThreshold',
           arguments = list(column = 'pti',
                            threshold = 0,
                            relationship = 'gte',
                            cross = FALSE),
           label = 'pti.sell',
           store = TRUE)

# +------------------------------------------------------------------
# | Rules will be processed in a very particular manner, so it bears
# | going over.
# +------------------------------------------------------------------

add.rule(strategy = name,
         name = 'ruleSignal',
         arguments = list(sigcol = 'pti.buy',
                          sigval = TRUE,
                          orderqty = 1,
                          ordertype = 'market',
                          orderside = 'long',
                          replace = TRUE,
                          osFUN = osVarSize,
                          acct.name = name,
                          TxnFees = TxnFees),
         label = 'pti.buy.enter',
         type = 'enter',
         store = TRUE)
add.rule(strategy = name,
         name = 'ruleSignal',
         arguments = list(sigcol = 'pti.sell',
                          sigval = TRUE,
                          orderqty = 1,
                          ordertype = 'market',
                          orderside = 'long',
                          replace = TRUE,
                          osFUN = osVarSize,
                          acct.name = name,
                          TxnFees = TxnFees),
         label = 'pti.buy.enter',
         type = 'exit',
         store = TRUE)
# add.rule(strategy = name,
#          name = 'ruleSignal',
#          arguments = list(sigcol = 'pti.buy',
#                           sigval = TRUE,
#                           orderqty = 'all',
#                           ordertype = 'stoptrailing', # stoplimit # stoptrailing
#                           orderside = 'long',
#                           orderset = 'stop',
#                           threshold = quote(-mktdata[timestamp, 'X1.sigma']),
#                           tmult = TRUE,
#                           TxnFees = TxnFees),
#          label = 'pti.buy.chain',
#          type = 'chain',
#          parent = 'pti.buy.enter',
#          store = TRUE)

# +------------------------------------------------------------------
# | This function is the wrapper that holds together the execution of
# | a strategy.
# +------------------------------------------------------------------

applyStrategy(strategy = name,
              portfolios = name)

# +------------------------------------------------------------------
# | The updatePortf function goes through each symbol and calculates
# | the PL for each period prices are available.
# +------------------------------------------------------------------

updatePortf(Portfolio = name,
            Symbols = Symbols)

# +------------------------------------------------------------------
# | Constructs the equity account calculations from the portfolio 
# | data and corresponding close prices.
# +------------------------------------------------------------------

updateAcct(name = name)

# +------------------------------------------------------------------
# | Calculates End.Eq and Net.Performance.
# +------------------------------------------------------------------

updateEndEq(Account = name)

for (symbol in Symbols)
{
  dev.new()
  
  # +------------------------------------------------------------------
  # | Produces a three-panel chart of time series charts that contains
  # | prices and transactions in the top panel, the resulting position
  # | in the second, and a cumulative profit-loss line chart in the 
  # | third.
  # +------------------------------------------------------------------
  
  try(chart.Posn(Portfolio = name,
                 Symbol = symbol))
}

# +------------------------------------------------------------------
# | This function (for now) calculates return on initial equity for 
# | each instrument in the portfolio or portfolios that make up an 
# | account. These columns will be additive to return on capital of 
# | each portfolio, or of the entire account.
# +------------------------------------------------------------------

R <- PortfReturns(Account = name)

R$Tot.DailyEqPl <- rowMeans(R)

# +------------------------------------------------------------------
# | Retrieves an account object from the .blotter environment. Useful
# | for local examination or charting, or storing interim results for
# | later reference.
# +------------------------------------------------------------------

account <- getAccount(Account = name)

plot(cumsum(account$summary$Realized.PL)[cumsum(account$summary$Realized.PL) != 0], main = 'Realized PL')

# +------------------------------------------------------------------
# | For a set of returns, create a wealth index chart, bars for 
# | per-period performance, and underwater chart for drawdown.
# +------------------------------------------------------------------

charts.PerformanceSummary(R = R,
                          ylog = TRUE,
                          main = 'Trading performance')

# +------------------------------------------------------------------
# | Get the order book object.
# +------------------------------------------------------------------

getOrderBook(portfolio = name)
