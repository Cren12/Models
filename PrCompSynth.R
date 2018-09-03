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

PrCompSynth <- function(
  xts.matrix, # A numeric or complex matrix (or data frame) which provides the data for the principal components analysis
  which.comp # Which rotated variables should be returned
)
{
  # +------------------------------------------------------------------
  # | Performs a principal components analysis on the given data matrix
  # | and returns the results as an object of class prcomp. prcomp 
  # | returns a list with class "prcomp".
  # +------------------------------------------------------------------
  
  pr.comp <- prcomp(x = xts.matrix,
                    center = TRUE,
                    scale. = TRUE)
  
  return(tail(pr.comp$x[, which.comp], 1))
}
