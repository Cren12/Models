packages <- c('blotter',
              'foreach',
              'doFuture')

# +------------------------------------------------------------------
# | library and require load and attach add-on packages. Download and
# | install packages from CRAN-like repositories.
# +------------------------------------------------------------------

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

# +------------------------------------------------------------------
# | Sys.setenv sets environment variables.
# +------------------------------------------------------------------

Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

# source()

# +------------------------------------------------------------------

RebalancePortfolio <- function(
  acct.name, 
  portf.name, 
  prices, 
  width, 
  dividends = NULL, 
  theor.weights, 
  rebalance.on = 'months',
  TxnFees = 0)
{
  # +------------------------------------------------------------------
  # | Extract index values of a given xts object corresponding to the
  # | last observations given a period specified by on. A numeric 
  # | vector of endpoints beginning with 0 and ending with the a value
  # | equal to the length of the x argument is returned.
  # +------------------------------------------------------------------
  
  end.of.periods <- index(prices)[endpoints(x = prices,
                                            on = rebalance.on)]
  if (!is.null(dividends))
  {
    dividend.dates <- index(dividends)
  } else {
    dividend.dates <- NULL
  }
  foreach(i = width:nrow(prices)) %do%
  {
    today <- as.Date(index(prices[i, ]))
    chartSeries(prices[paste0('/', today)],
                name = '')
    
    # +------------------------------------------------------------------
    # | Get a portfolio object consisting of a nested list.
    # +------------------------------------------------------------------
    
    portfolio <- blotter::getPortfolio(portf.name)
    
    # +------------------------------------------------------------------
    # | Retrieves an account object from the .blotter environment.
    # +------------------------------------------------------------------
    
    account <- getAccount(acct.name)
    
    equity <- as.numeric(account$summary$End.Eq[as.character(today), ])
    if (length(equity) == 0)
    {
      equity <- as.numeric(account$summary$End.Eq[1, ])
    }
    avail.liq <- as.numeric(account$summary$End.Eq[as.character(today), ]) - as.numeric(portfolio$summary$Gross.Value[as.character(today), ])
    if (length(avail.liq) == 0)
    {
      avail.liq <- equity
    }
    if (today %in% end.of.periods && !(today %in% dividend.dates))
    {
      theor.value <- ifelse(is.na(lag(theor.weights)[today]), 0, lag(theor.weights)[today] * equity)
      pos.qty <- max(c(0, as.numeric(portfolio$symbols[[symbol]]$posPL$Pos.Qty[as.character(today), ])))
      pos.avg.cost <- max(c(0, as.numeric(portfolio$symbols[[symbol]]$posPL$Pos.Avg.Cost[as.character(today), ])))
      print(paste(today, 'Current position:', pos.qty, '@', pos.avg.cost))
      pos.value <- pos.qty * pos.avg.cost
      to.trade.value <- theor.value - pos.value
      to.trade.value <- ifelse(to.trade.value > 0, min(c(avail.liq, to.trade.value)), to.trade.value)
      to.trade.shares <- ifelse(to.trade.value >= 0, floor(to.trade.value / prices[today, ]), floor(to.trade.value / prices[today, ]))
      if (as.numeric(to.trade.shares) != 0)
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
               TxnQty = as.numeric(to.trade.shares),
               TxnPrice = as.numeric(prices[today, ]),
               TxnFees = TxnFees)
      }
    }
    print(addTA(ta = theor.weights))
    try(expr = print(addLines(h = pos.avg.cost, col = 'gray50')),
        silent = TRUE)
    if (!is.null(dividends))
    {
      div.per.share <- as.numeric(dividends[today])
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
  }
  
  par(mfrow = c(2, 2))
  print(plot(x = cumsum(account$summary$Realized.PL[paste0(first(index(prices) + width),'/'), ]),
             main = 'Realized PL'))
  print(plot(x = cumsum(account$summary$Unrealized.PL[paste0(first(index(prices) + width),'/'), ]),
             main = 'Unrealized PL'))
  print(plot(x = (account$summary$End.Eq - portfolio$summary$Gross.Value)[paste0(first(index(prices) + width),'/'), ],
             main = 'Liquidity'))
  print(plot(x = account$summary$End.Eq[paste0(first(index(prices) + width),'/'), ],
             main = 'End Eq'))
  par(mfrow = c(1, 1))
  
  # +------------------------------------------------------------------
  # | This function (for now) calculates return on initial equity for 
  # | each instrument in the portfolio or portfolios that make up an 
  # | account. These columns will be additive to return on capital of 
  # | each portfolio, or of the entire account.
  # +------------------------------------------------------------------
  
  R <- PortfReturns(Account = acct.name)
  
  R.core <- coredata(x = R)
  R.core <- xts(x = R.core,
                order.by = index(R))
  
  # +------------------------------------------------------------------
  # | For a set of returns, create a wealth index chart, bars for 
  # | per-period performance, and underwater chart for drawdown.
  # +------------------------------------------------------------------
  
  charts.PerformanceSummary(R = R.core,
                            geometric = TRUE,
                            main = paste0(portf.name, ' Portfolio'))
  
  # +------------------------------------------------------------------
  # | Table of Annualized Return, Annualized Std Dev, and Annualized 
  # | Sharpe.
  # +------------------------------------------------------------------
  
  table.AnnualizedReturns(R = R.core)
}
