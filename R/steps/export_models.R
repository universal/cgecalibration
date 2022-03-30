bounds <- local({
  bounds <- t(bind_rows(paramsList)) %>% as.data.frame() %>% rename(lower = 1, upper = 2) %>% 
    rownames_to_column(var = "coefficients") %>%  mutate(prior = lower + (upper - lower) / 2 ) %>% 
    mutate(variance = (upper - lower) / 1) %>% 
    left_join(trueSample %>% pivot_longer(-MM_SIM, names_to = "coefficients", values_to = "true") %>% select(-MM_SIM)) %>%
    pivot_longer(-coefficients, names_to = "bounds") %>%
    mutate(coefficients = factor(coefficients), bounds = factor(bounds))
  attr(bounds, "symName") <- "bounds_coefficients"
  attr(bounds, "domain") <- c("coefficients", "bounds")
  return(bounds)  
})

modelsToGDX(estimations, "../gams/calibration/models.gdx", bounds)