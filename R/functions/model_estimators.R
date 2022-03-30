toFilteredResults <- function(result, activeSims, activeT) {
  if(length(activeT) == 1) {
    lapply(result, function(x) {
      x <- x %>% mutate(MM_SIM = as.character(MM_SIM)) %>% filter(MM_SIM %in% activeSims) %>% filter(t %in% activeT) %>% select(-t)
      colnames(x)[ncol(x)] <- "value"
      return(x)
    })
  } else {
    lapply(result, function(x) {
      x <- x %>% mutate(MM_SIM = as.character(MM_SIM)) %>% filter(MM_SIM %in% activeSims) %>% filter(t %in% activeT)
      colnames(x)[ncol(x)] <- "value"
      return(x)
    })
  }
}

toEstimationData <- function(result, activeSims, sample, activeT = 30) {
  filtered <- toFilteredResults(result, activeSims, activeT)
  lapply(filtered, function(x) {
    merge(x, sample)
  })
}


linearModelFormula <- function(policyVariables) {
  as.formula(paste0("Y ~ (", 
                    paste(policyVariables, collapse = " + "), ")^2", 
                    " + ", 
                    paste(paste0("I(", policyVariables, "^2)"), collapse = " + "))
             )
}

# linearModelFormula <- function(policyVariables) {
#   as.formula(paste0("Y ~ ", paste(policyVariables, collapse = " + ")))
# }

fitModel <- function(data, variables, yCol, modelType) {
  x <- data %>% select(all_of(variables))
  y <- data[, yCol]
  if(modelType %in% c("Linear", "Kriging")) {
    result <- DiceEval::modelFit(
      x,
      y,
      type = modelType,
      formula = linearModelFormula(variables),
      nugget = 1e-3
    )
  } else {
    stop("unsupported model type...")
  }
  return(result)
}

modelEval <- function(model, data, variables, yCol) {
  x <- data %>% select(all_of(variables))
  y <- data[, yCol]
  
  data <- data.frame(y = y, pred = predict.lm(model, data))
  
  return(data.frame(rmse = DiceEval::RMSE(data$y, data$pred), 
                    rma = DiceEval::RMA(data$y, data$pred)$max.value,
                    mae = DiceEval::MAE(data$y, data$pred),
                    mean = mean(data$y),
                    r_squared = DiceEval::R2(data$y, data$pred),
                    aer = DiceEval::RMSE(data$y, data$pred) / mean(data$y)
  )
  )
}
