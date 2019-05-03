packages <- c('rugarch',
              'quantmod',
              'foreach',
              'PerformanceAnalytics',
              'mixtools')

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

FHS <- function(
  ohlc,
  n.sim,
  m.sim = 5000
)
{
  dx <- ClCl(ohlc)
  dx[1] <- 0;
  
  # +------------------------------------------------------------------
  # | Method for creating a univariate GARCH specification object prior
  # | to fitting.
  # +------------------------------------------------------------------
  
  spec <- ugarchspec(variance.model = list(model = 'fGARCH',
                                           submodel = 'ALLGARCH'),
                     distribution.model = 'sged')
  
  # +------------------------------------------------------------------
  # | Method for fitting a variety of univariate GARCH models.
  # +------------------------------------------------------------------
  
  fit <- ugarchfit(spec = spec,
                   data = dx)
  z <- fit@fit$z
  z.m <- matrix(data = NA, 
                ncol = m.sim, 
                nrow = n.sim)
  
  for (j in 1:m.sim)
  {
    # +------------------------------------------------------------------
    # | sample takes a sample of the specified size from the elements of
    # | x using either with replacement.
    # +------------------------------------------------------------------
    
    z.m[, j] <- sample(x = tail(z, n.sim),
                       size = n.sim)
  }
  
  # +------------------------------------------------------------------
  # | Method for simulation from a variety of univariate GARCH models.
  # +------------------------------------------------------------------
  
  sim <- ugarchsim(fit = fit,
                   n.sim = n.sim,
                   m.sim = m.sim,
                   custom.dist = list(name = "sample", 
                                      distfit = z.m, 
                                      type = "z"))
  
  price.dist <- foreach(j = 1:m.sim, .combine = c) %do%
  {
    sim.prices <- rep(NA, n.sim)
    sim.prices[1] <- last(Cl(ohlc)) * (1 + sim@simulation$seriesSim[1, j])
    
    for (i in 2:length(sim.prices))
    {
      sim.prices[i] <- sim.prices[i - 1] * (1 + sim@simulation$seriesSim[i, j])
    }
    
    return(last(sim.prices))
  }
  
  # +------------------------------------------------------------------
  # | Return EM algorithm output for mixtures of normal distributions.
  # +------------------------------------------------------------------
  
  n.mix <- normalmixEM(x = log(price.dist),
                       maxit = 10000)
  
  mln.alpha.1 <- n.mix$lambda[1]
  mln.meanlog.1 <- n.mix$mu[1]
  mln.meanlog.2 <- n.mix$mu[2]
  mln.sdlog.1 <- n.mix$sigma[1]
  mln.sdlog.2 <- n.mix$sigma[2]
  k <- seq(96, 113, .01)
  dx <- dmln(x = k, alpha.1 = mln.alpha.1, meanlog.1 = mln.meanlog.1, 
             meanlog.2 = mln.meanlog.2,
             sdlog.1 = mln.sdlog.1, sdlog.2 = mln.sdlog.2)
  plot(dx ~ k, type = "l") ; grid()
  
  return(n.mix)
}
