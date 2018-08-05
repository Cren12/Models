package <- c('compiler',
             'quantmod',
             'dygraphs',
             'devtools',
             'FRAPO',
             'foreach',
             'doParallel',
             'doFuture',
             'plyr')
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

Sys.setenv(TZ ="UTC")

# +------------------------------------------------------------------
# | Creates a set of copies of R running in parallel and 
# | communicating over sockets. makeCluster creates a cluster of one
# | of the supported types. The default type, "PSOCK", calls 
# | makePSOCKcluster.
# +------------------------------------------------------------------

# clusters <- makeCluster(detectCores())

# +------------------------------------------------------------------
# | The registerDoParallel function is used to register the parallel 
# | backend with the foreach package.
# +------------------------------------------------------------------

# registerDoParallel()

# +------------------------------------------------------------------
# | Register the doFuture parallel adaptor to be used by the foreach 
# | package.
# +------------------------------------------------------------------

# registerDoFuture()

# +------------------------------------------------------------------
# | This function allows the user to plan the future, more 
# | specifically, it specifies how future():s are resolved, e.g. 
# | sequentially or in parallel.
# +------------------------------------------------------------------

# plan(multiprocess)

# +------------------------------------------------------------------
# | Level 1
# +------------------------------------------------------------------

getData <- function(Symbols, env)
{
  getSymbols(Symbols = Symbols,
             env = env,
             from = '1950-01-01')
  args <- eapply(env = env,
                 FUN = Cl)[Symbols]
  value <- do.call(what = merge,
                   args = args)
  for (symbol in Symbols)
  {
    assign(x = symbol,
           value = get(x = symbol,
                       envir = env),
           envir = .GlobalEnv)
  }
  colnames(value) <- Symbols
  return(na.omit(value))
}

getDividendsData <- function(Symbols)
{
  dividends <- getDividends(Symbol = Symbols[1])
  for (symbol in Symbols[-1])
  {
    dividends <- cbind(dividends, getDividends(Symbol = symbol))
  }
  return(dividends)
}

runPMD.weights <- function(data, width)
{
  value <- matrix(data = NA,
                  nrow = nrow(data),
                  ncol = ncol(data))
  pmd.weights <- foreach(i = width:nrow(data), .combine = rbind) %dopar%
  {
    PMD.weights(data[(i - width + 1):i, ])
  }
  value[width:nrow(data), ] <- pmd.weights
  value <- reclass(x = value,
                   match.to = data)
  colnames(value) <- colnames(data)
  return(value)
}

RebalancePortfolio <- function(acct.name, portf.name, prices, width, dividends)
{
  # +------------------------------------------------------------------
  # | Extract index values of a given xts object corresponding to the
  # | last observations given a period specified by on. A numeric 
  # | vector of endpoints beginning with 0 and ending with the a value
  # | equal to the length of the x argument is returned.
  # +------------------------------------------------------------------
  
  end.of.months <- index(prices)[endpoints(x = prices,
                                           on = 'weeks')]
  dividend.dates <- index(dividends)
  
  foreach(i = width:nrow(prices)) %dopar%
  {
    today <- as.Date(index(prices[i, ]))
    portf.symbols <- colnames(prices)
    
    # +------------------------------------------------------------------
    # | Get a portfolio object consisting of a nested list.
    # +------------------------------------------------------------------
    
    portfolio <- getPortfolio(portf.name)
    
    # +------------------------------------------------------------------
    # | Retrieves an account object from the .blotter environment.
    # +------------------------------------------------------------------
    
    account <- getAccount(acct.name)
    
    equity <- as.numeric(account$summary$End.Eq[1, ])
    avail.liq <- as.numeric(account$summary$End.Eq[as.character(today), ]) - as.numeric(portfolio$summary$Gross.Value[as.character(today), ])
    if (length(avail.liq) == 0)
    {
      avail.liq <- equity
    }
    
    if (today %in% end.of.months && !(today %in% dividend.dates))
    {
      message(paste0(today, ': è il momento di ribilanciare...'))
      theor.weights <- PMD.weights(prices[(i - width + 1):i, ]) / 100
      theor.value <- theor.weights * equity
      pos.value <- rep(NA, length(portf.symbols))
      names(pos.value) <- portf.symbols
      for (j in 1:length(portf.symbols))
      {
        symbol <- portf.symbols[j]
        pos.qty <- max(c(0, as.numeric(portfolio$symbols[[symbol]]$posPL$Pos.Qty[as.character(today), ])))
        pos.avg.cost <- max(c(0, as.numeric(portfolio$symbols[[symbol]]$posPL$Pos.Avg.Cost[as.character(today), ])))
        pos.value[j] <- pos.qty * pos.avg.cost
      }
      to.trade.value <- theor.value - pos.value
      to.trade.value <- ifelse(to.trade.value > 0, pmin(avail.liq / ncol(prices), to.trade.value), to.trade.value)
      to.trade.shares <- ifelse(to.trade.value >= 0, floor(to.trade.value / prices[today, ]), ceiling(to.trade.value / prices[today, ]))
      for (j in 1:length(portf.symbols))
      {
        symbol <- portf.symbols[j]
        if (as.numeric(to.trade.shares[today, symbol]) != 0)
        {
          # +------------------------------------------------------------------
          # | When a trade or adjustment is made to the Portfolio, the addTxn 
          # | function calculates the value and average cost of the 
          # | transaction, the change in position, the resulting positions 
          # | average cost, and any realized profit or loss (net of fees) from
          # | the transaction. Then it stores the transaction and calculations
          # | in the Portfolio object.
          # +------------------------------------------------------------------
          
          addTxn(Portfolio = portf.name,
                 Symbol = symbol,
                 TxnDate = today,
                 TxnQty = as.numeric(to.trade.shares[today, symbol]),
                 TxnPrice = as.numeric(prices[today, symbol]),
                 TxnFees = 0)
        }
      }
    }
    for (k in 1:ncol(dividends))
    {
      symbol <- colnames(dividends)[k]
      div.per.share <- as.numeric(dividends[today, symbol])
      if (!is.na(div.per.share) && length(div.per.share) > 0)
      {
        # +------------------------------------------------------------------
        # | Adding a cash dividend does not affect position quantity, like a 
        # | split would.
        # +------------------------------------------------------------------
        
        addDiv(Portfolio = portf.name,
               Symbol = gsub(pattern = '.div',
                             replacement = '',
                             x = symbol),
               TxnDate = today,
               DivPerShare = div.per.share)
      }
    }
    
    # +------------------------------------------------------------------
    # | The updatePortf function goes through each symbol and calculates 
    # | the PL for each period prices are available.
    # +------------------------------------------------------------------
    
    updatePortf(Portfolio = portf.name)
    
    # +------------------------------------------------------------------
    # | Constructs the equity account calculations from the portfolio 
    # | data and corresponding close prices.
    # +------------------------------------------------------------------
    
    updateAcct(name = acct.name)
    
    # +------------------------------------------------------------------
    # | Calculates End.Eq and Net.Performance.
    # +------------------------------------------------------------------
    
    updateEndEq(Account = acct.name)
    
    message(today)
  }
  
  par(mfrow = c(2, 2))
  plot(x = cumsum(account$summary$Realized.PL[paste0(first(index(prices) + width),'/'), ]),
       main = 'Realized PL')
  plot(x = cumsum(account$summary$Unrealized.PL[paste0(first(index(prices) + width),'/'), ]),
       main = 'Unrealized PL')
  plot(x = (account$summary$End.Eq - portfolio$summary$Gross.Value)[paste0(first(index(prices) + width),'/'), ],
       main = 'Liquidity')
  plot(x = account$summary$End.Eq[paste0(first(index(prices) + width),'/'), ],
       main = 'End Eq')
  par(mfrow = c(1, 1))
  
  # +------------------------------------------------------------------
  # | This function (for now) calculates return on initial equity for 
  # | each instrument in the portfolio or portfolios that make up an 
  # | account. These columns will be additive to return on capital of 
  # | each portfolio, or of the entire account.
  # +------------------------------------------------------------------
  
  R <- PortfReturns(Account = acct.name)[paste0(first(index(prices) + width),'/'), ]
  
  R$Portfolio.DailyEqPL <- rowSums(R)
  
  # +------------------------------------------------------------------
  # | For a set of returns, create a wealth index chart, bars for 
  # | per-period performance, and underwater chart for drawdown.
  # +------------------------------------------------------------------
  
  charts.PerformanceSummary(R = R,
                            geometric = TRUE,
                            main = paste0(portf.name, ' Portfolio'))
  
  table.AnnualizedReturns(R = R)
}

# +------------------------------------------------------------------
# | Level 2
# +------------------------------------------------------------------

PMD.weights <- function(prices)
{
  Returns <- CalculateReturns(prices)
  value <- PMD(Returns = na.omit(Returns),
               use = 'complete.obs')
  return(value@weights)
}

# +------------------------------------------------------------------
# | Main: PMD backtest
# +------------------------------------------------------------------

Symbols <- c('SPY', 'GLD', 'TLT', 'HYG', 'LQD', 'EEM', 'GDX', 'QQQ', 'USO')
prices <- getData(Symbols = Symbols,
                  env = new.env())
dividends <- getDividendsData(Symbols = Symbols)
currency('USD')
for (symbol in Symbols)
{
  stock(primary_id = symbol,
        currency = 'USD',
        multiplier = 1)
}
rm(list = c('account.Ciccio', 'portfolio.PMD'),
   envir = .blotter)
initPortf('PMD',
          symbols = Symbols,
          currency = 'USD',
          initDate = first(index(prices)) - 1)
initAcct(name = 'Ciccio',
         portfolios = 'PMD',
         initEq = 1000000)

generateOrders()
backtest()
