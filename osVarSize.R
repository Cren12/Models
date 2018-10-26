packages <- c()

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
  theor.value <- ifelse(is.na(mktdata[timestamp, col.name]), 0, mktdata[timestamp, col.name] * equity)
  pos.qty <- max(c(0, as.numeric(portfolio.object$symbols[[symbol]]$posPL$Pos.Qty[as.character(timestamp), ])))
  pos.avg.cost <- max(c(0, as.numeric(portfolio.object$symbols[[symbol]]$posPL$Pos.Avg.Cost[as.character(timestamp), ])))
  pos.value <- pos.qty * pos.avg.cost
  to.trade.value <- theor.value - pos.value
  to.trade.value <- ifelse(to.trade.value > 0, min(c(avail.liq, to.trade.value)), to.trade.value)
  to.trade.shares <- ifelse(to.trade.value >= 0, floor(to.trade.value / Cl(mktdata[timestamp, ])), floor(to.trade.value / Cl(mktdata[timestamp, ])))
  orderqty <- to.trade.shares
  return(orderqty)
}
