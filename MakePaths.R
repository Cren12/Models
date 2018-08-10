packages <- c('yuima',
              'foreach',
              'doFuture')

# +------------------------------------------------------------------
# | library() and require() load and attach add-on packages. Possibly,
# | download and install packages from a CRAN-like repositoriy.
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
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

# source()

# +------------------------------------------------------------------

MakePaths <- function(
  S, # The asset price, a numeric value
  Time, # The time to maturity measured in years, a numeric value
  r, # The annualized rate of interest, a numeric value; e.g. 0.25 means 25% pa
  b, # The annualized cost-of-carry rate, a numeric value; e.g. 0.1 means 10% pa
  sigma, # The annualized volatility of the underlying security, a numeric value; e.g. 0.3 means 30% volatility pa
  n.sim
)
{
  mu <- r - b
  drift <- paste(mu, '* S')
  diffusion <-  paste(sigma, '* S')
  
  # +------------------------------------------------------------------
  # | 'setModel' gives a description of stochastic differential 
  # | equation with or without jumps of the following form:
  # |
  # |     dXt = a(t, Xt, alpha)dt +
  # |           + b(t, Xt, beta)dWt +
  # |           + c(t, Xt, gamma)dZt, X0 = x0.
  # +------------------------------------------------------------------
  
  yuima.model <- setModel(drift = drift,
                          diffusion = diffusion,
                          state.variable = 'S',
                          solve.variable = 'S',
                          xinit = S)
  
  # +------------------------------------------------------------------
  # | setSampling is a constructor for yuima.sampling-class.
  # +------------------------------------------------------------------
  
  yuima.sampling <- setSampling(Initial = 0,
                                Terminal = Time,
                                n = 252 * 8 * Time)
  
  # +------------------------------------------------------------------
  # | setYuima constructs an object of yuima-class.
  # +------------------------------------------------------------------
  
  yuima <- setYuima(model = yuima.model,
                    sampling = yuima.sampling)
  
  # +------------------------------------------------------------------
  # | Register the doFuture parallel adaptor to be used by the foreach
  # | package.
  # +------------------------------------------------------------------
  
  registerDoFuture()
  
  # +------------------------------------------------------------------
  # | This function allows the user to plan the future, more 
  # | specifically, it specifies how future():s are resolved, e.g. 
  # | sequentially or in parallel. multiprocess: If multicore 
  # | evaluation is supported, that will be used, otherwise 
  # | multisession evaluation will be used.
  # +------------------------------------------------------------------
  
  plan(multiprocess)
  
  sim <- foreach(i = 1:n.sim) %dopar%
  {
    # +------------------------------------------------------------------
    # | Simulate multi-dimensional stochastic processes. A yuima-class 
    # | object is returned.
    # +------------------------------------------------------------------
    
    scenario <- simulate(object = yuima)
    
    path <- as.vector(scenario@data@zoo.data$`Series 1`)
    names(path) <- seq(from = 0,
                       to = Time,
                       by = yuima@sampling@delta)
    return(path)
  }
  return(sim)
}