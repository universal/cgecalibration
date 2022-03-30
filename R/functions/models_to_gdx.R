prepareGDXForModel <- function(data, j = NULL){
  coefficientValues <- coef(data$model)
  coefficientNames <- names(coefficientValues)
  intercepts <- c("(Intercept)")
  quadraticTerms <- grep("I(.+\\^2)", coefficientNames, value = T)
  interactionTerms <- grep(".+:.+", coefficientNames, value = T)
  linearTerms <- setdiff(coefficientNames, c(intercepts, quadraticTerms, interactionTerms))
  interceptGDX <- data.frame(value = coefficientValues[intercepts])
  linearGDX <- data.frame(coefficients = linearTerms, value = coefficientValues[linearTerms])
  quadraticGDX <- data.frame(coefficients = gsub("I\\((.+)\\^2\\)", "\\1", quadraticTerms), coefficients1 = gsub("I\\((.+)\\^2\\)", "\\1", quadraticTerms)) %>% 
    mutate(value = coefficientValues[quadraticTerms])
  interactionGDX <- data.frame(terms = interactionTerms) %>% separate(terms, into = c("coefficients", "coefficients1"), sep = ":") %>%
    mutate(value = coefficientValues[interactionTerms])
  if(!is.null(j)) {
    interceptGDX <- interceptGDX %>% mutate(j = j, .before = 1)
    linearGDX <- linearGDX %>% mutate(j = j, .before = 1)
    quadraticGDX <- quadraticGDX %>% mutate(j = j, .before = 1)
    interactionGDX <- interactionGDX %>% mutate(j = j, .before = 1)
  } 
  return(list(
    intercept = interceptGDX,
    linear = linearGDX,
    interaction = bind_rows(quadraticGDX, interactionGDX)
  ))
}

modelsToGDXFrames <- function(estimations) {
  timepointFrames <- lapply(names(estimations), function(t) {
    lapply(estimations[[t]]$models, function(m) {
      if(all(names(m) %in% sectors)) {
        res <- lapply(sectors, function(s) { prepareGDXForModel(m[[s]], s)})
        list(
          intercepts = bind_rows(lapply(res, function(x)  { x$intercept })) %>% mutate(j = factor(j)) %>% mutate(t = factor(activeTimepoints[t]), .before = 1),
          linears = bind_rows(lapply(res, function(x)  { x$linear })) %>% mutate(j = factor(j), coefficients = factor(coefficients)) %>% mutate(t = factor(activeTimepoints[t]), .before = 1),
          interactions = bind_rows(lapply(res, function(x)  { x$interaction })) %>% mutate(j = factor(j), coefficients = factor(coefficients), coefficients1 = factor(coefficients1)) %>% mutate(t = factor(activeTimepoints[t]), .before = 1)
        )
      } else {
        res <- prepareGDXForModel(m)
        list(
          intercepts = res$intercept %>% mutate(t = factor(activeTimepoints[t]), .before = 1),
          linears = res$linear %>% mutate(coefficients = factor(coefficients)) %>% mutate(t = factor(activeTimepoints[t]), .before = 1),
          interactions = res$interaction %>% mutate(coefficients = factor(coefficients), coefficients1 = factor(coefficients1)) %>% mutate(t = factor(activeTimepoints[t]), .before = 1)
        )
      }
    })
  })
  merged <- lapply(lapply(names(timepointFrames[[1]]), function(target) {
    lapply(timepointFrames, function(x) x[[target]])
  }), function(x) {
    do.call(mapply, c(FUN = bind_rows, x))
  })
  names(merged) <- names(timepointFrames[[1]])
  return(merged)
}

prepareSetForGDX <- function(data, name) {
  outDF <- data[!is.na(data)]
  outLst <- list(
    list(name=name, type='set', uels=c(list(outDF)))
  )
  return(outLst)
}

modelsToGDX <- function(estimations, target, bounds) {
  frames <- modelsToGDXFrames(estimations)
  coefficientsSet <- prepareSetForGDX(levels(frames$Z1$linears$coefficients), "coefficients")
  for(n in names(frames)) {
    for(k in names(frames[[n]])) {
      attr(frames[[n]][[k]], "symName") <- paste0(n, "_", k)
    }
  }
  do.call(wgdx.lst, c(list(gdxName = target), unlist(frames, recursive = FALSE), list(coefficientsSet, bounds)))
}
