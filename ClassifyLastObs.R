packages <- c('neuralnet')

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

source('Make_mktdata.R')

# +------------------------------------------------------------------

data <- na.omit(Make_mktdata(ohlc = ohlc,
                             change = change))

ClassifyLastObs <- function(
  data # A data frame containing the variables
)
{
  data <- na.omit(data)
  y <- data$Minimum.point
  X <- data[, -1]
  
  out <- tryCatch(
    expr = {
      
      # +------------------------------------------------------------------
      # | Performs a principal components analysis on the given data matrix
      # | and returns the results as an object of class prcomp.
      # +------------------------------------------------------------------
      
      pca <- prcomp(x = X,
                    scale. = TRUE)
      
      formula <- y ~ PC1 + PC2 + PC3 + PC4 + PC5
      
      if (nnet)
      {
        
        # +------------------------------------------------------------------
        # | neuralnet is used to train neural networks. neuralnet returns an
        # | object of class nn.
        # +------------------------------------------------------------------
        
        nnet <- neuralnet(formula = formula,
                          data = cbind.data.frame(y, pca$x[, 1:5]),
                          hidden = 2,
                          linear.output = FALSE)
        
        ## Complete model:
        # nnet1 <- neuralnet(formula = as.formula(substr(paste('y ~', paste(colnames(X), '+ ', collapse = '')), 1, nchar(paste('y ~', paste(colnames(X), '+ ', collapse = ''))) - 3)),
        #                    data = cbind.data.frame(y, X),
        #                    hidden = 2)
        
        # +------------------------------------------------------------------
        # | compute, a method for objects of class nn, typically produced by
        # | neuralnet. Computes the outputs of all neurons for specific 
        # | arbitrary covariate vectors given a trained neural network.
        # +------------------------------------------------------------------
        
        out <- compute(x = nnet,
                       covariate = pca$x[, 1:5])
        
        return(as.numeric(last(out$net.result)))
      } else {
        
        # +------------------------------------------------------------------
        # | glm is used to fit generalized linear models, specified by giving
        # | a symbolic description of the linear predictor and a description
        # | of the error distribution. glm returns an object of class 
        # | inheriting from "glm" which inherits from the class "lm".
        # +------------------------------------------------------------------
        
        model <- glm(formula = formula,
                     family = binomial(link = 'logit'),
                     data = cbind.data.frame(y, pca$x[, 1:5]))
        
        return(last(model$fitted.values))
    },
    error = function(e){
      return(0)
    },
    finally = {}
  )
  return(out)
}
