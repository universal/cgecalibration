variables <- colnames(estimationSample)[-1]

doEstimations <- function(estimationSample, validationSample) {
  lapply(as.list(activeTimepoints), function(t) {
    estimationData <- toEstimationData(result, estimationSample$MM_SIM, estimationSample, activeT = t)
    validationData <- toEstimationData(result, validationSample$MM_SIM, validationSample, activeT = t)
    models <- lapply(estimationData, function(x) {
      if("j" %in% colnames(x)) {
        out <- lapply(sectors, function(s) {
          fitModel(x %>% filter(j == s) %>% select(-j), variables, "value", "Linear")
        })
        names(out) <- sectors
      } else {
        out <- fitModel(x, variables, "value", "Linear")
      }
      return(out)
    })
    
    
    modelEvals <- bind_rows(lapply(names(models), function(z) {
      x <- validationData[[z]]
      if("j" %in% colnames(x)) {
        lapply(sectors, function(s) {
          modelEval(models[[z]][[s]]$model, x %>% filter(j == s) %>% select(-j), variables, "value") %>% mutate(target = paste0(z, "_", s))
        })
      } else {
        modelEval(models[[z]]$model, x, variables, "value")  %>% mutate(target = z)
      }
    }))
    return(list(
      models = models,
      evaluations = modelEvals
    ))
  })
}

estimations <- doEstimations(estimationSample, validationSample)