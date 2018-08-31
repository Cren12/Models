# +------------------------------------------------------------------
# | User variable inputs
# +------------------------------------------------------------------

target.date <- as.Date( '2018-09-28' )
underlying <- 86.36
quantity <- 0

# +------------------------------------------------------------------
# | User fixed inputs
# +------------------------------------------------------------------

L <- 100000
x <- as.Date( '2018-06-01' ):as.Date( '2020-06-01' )
x0 <- as.Date( '2019-06-01' )
fcc <- 5000

# +------------------------------------------------------------------

package <- c("compiler",
             "plyr",
             "doParallel",
             "DEoptim",
             "dygraphs",
             "xts")
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

logistic.fun <- cmpfun(function(L, k, x, x0)
{
  return( L / (1 + exp(- k * (x - x0))) )
})

# gaussian.fun <- cmpfun(function(a, x, b, c)
# {
#   return( a * exp(- ((x - b) ^ 2) / (2 * c ^ 2)) )
# })

logistic.fun.fcc <- cmpfun(function(L, x, x0, fcc)
{
  opt.fun <- function(L, k, x, x0, fcc)
  {
    return( abs(logistic.fun(L, k, x, x0)[1] - fcc) )
  }
  k <- DEoptim(fn = opt.fun,
               L = L,
               x = as.numeric(x),
               x0 = as.numeric(x0),
               fcc = fcc,
               lower = 0,
               upper = 1,
               control = DEoptim.control(trace = FALSE))$optim$bestmem
  vp <- logistic.fun(L = L, 
                     k = k, 
                     x = as.numeric(x), 
                     x0 = as.numeric(x0))
  vp <- xts(x = vp,
            order.by = as.Date(x))
  colnames(vp) <- 'Value Path'
  return( vp )
})

dygraph(logistic.fun.fcc(L = L, 
                         x = x, 
                         x0 = x0, 
                         fcc = fcc))
(value.path <- logistic.fun.fcc(L = L, 
                                x = x, 
                                x0 = x0, 
                                fcc = fcc)[target.date, ])
cat('Current path:', underlying * quantity, '\n')
to.trade <- round((value.path - underlying * quantity) / underlying)
cat('You should', ifelse(to.trade < 0, 'sell', 'buy'), abs(to.trade), 'shares\n')
opt.to.trade <- max(1, floor(value.path / (underlying * 100)))
cat('Short', opt.to.trade, 'option(s)\n')
