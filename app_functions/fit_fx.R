# Group by experiment, fit 4-parameter logistic curve
library(dr4pl)
# library(drc)
library(tidyverse)

fit_curve <- function(df, response_var, dose_var){
  
  formula <- as.formula(paste(response_var, dose_var, sep="~"))
  
  out <- 
    tryCatch({
      fit = dr4pl(formula, data = df)
      return(fit)
    }, 
    error = function(condition){
      message(paste("Fit did not converge."))
      # Create a 'dr4pl' object to hold the data
      unfit = list(convergence = FALSE,
                   data = data.frame(Dose = df[dose_var],
                                     Response = df[response_var]),
                   hessian = matrix(as.numeric(NA),4,4),
                   loss.value = as.numeric(NA),
                   method.robust = NA,
                   parameters = setNames(rep(as.numeric(NA),4),c("UpperLimit", "EC50", "Slope", "LowerLimit")),
                   sample.size = nrow(df),
                   call = NA,
                   formula = formula)
      attr(unfit, "class") <- "dr4pl"
      return(unfit)
    }
    )
  
  return(out)
  
}

get_rsq <- function(dr4pl_fit){
  
  params <- dr4pl_fit$parameters
  
  if(any(is.na(params))){
    return(NA)
  }
    
  A <- params["LowerLimit"]
  B <- params["Slope"]
  C <- params["EC50"]
  D <- params["UpperLimit"]
  
  x <- dplyr::pull(dr4pl_fit$data["Dose"])
  y <- dplyr::pull(dr4pl_fit$data["Response"])
  
  y_hat <- D+((A - D)/(1 + (x/C)^B))
  ss_res <- sum((y - y_hat)^2)
  
  y_bar <- mean(y)
  ss_tot <- sum((y - y_bar)^2)

  rsq <- 1 - (ss_res/ss_tot)
  
  return(round(rsq, 3))
  
}

get_params <- function(dr4pl_fit){
  params <- round(dr4pl_fit$parameters, 1)
  params_list <- lapply(split(params, names(params)), unname)
  return(params_list)
}

myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}
