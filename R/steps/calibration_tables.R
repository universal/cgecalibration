local({
  table_data <- bind_rows(
    calibrationResult$mm_Z1 %>% filter(j == "AGR") %>% rename(value = mm_Z1) %>% mutate(target = paste0("$\\bm{Z1_{", j, ",", t, "}}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z2 %>% filter(t == 30) %>% rename(value = mm_Z2) %>% mutate(target = paste0("$\\bm{Z2_{", t, "}}$")) %>% select(-t),
    calibrationResult$mm_Z3 %>% filter(t == 30, j == "LMN") %>% rename(value = mm_Z3) %>% mutate(target = paste0("$\\bm{Z3_{", j, ",", t, "}}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z4 %>% filter(t == 30) %>% rename(value = mm_Z4) %>% mutate(target = paste0("$\\bm{Z4_{", t, "}}$")) %>% select(-t),
    calibrationResult$mm_Z5 %>% filter(t == 30) %>% rename(value = mm_Z5) %>% mutate(target = paste0("$\\bm{Z5_{", t, "}}$")) %>% select(-t),
    calibrationResult$mm_Z6 %>% filter(t == 30, j == "AGR") %>% rename(value = mm_Z6) %>% mutate(target = paste0("$\\bm{Z6_{", j, ",", t, "}}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z7 %>% filter(t == 30) %>% rename(value = mm_Z7) %>% mutate(target = paste0("$\\bm{Z7_{", t, "}}$")) %>% select(-t),
    calibrationResult$z1_variance %>% rename(value = z1_variance) %>% mutate(target = paste0("$\\bm{Z1_{", j, ",", t, "}}$")) %>% select(-c(j, t)) %>% mutate(RESULTS = "variance", .before = 1),
    calibrationResult$z2_variance %>% rename(value = z2_variance) %>% mutate(target = paste0("$\\bm{Z2_{", t, "}}$")) %>% select(-t) %>% mutate(RESULTS = "variance", .before = 1),
    calibrationResult$z3_variance %>% rename(value = z3_variance) %>% mutate(target = paste0("$\\bm{Z3_{", j, ",", t, "}}$")) %>% select(-c(j, t)) %>% mutate(RESULTS = "variance", .before = 1),
    calibrationResult$z4_variance %>% rename(value = z4_variance) %>% mutate(target = paste0("$\\bm{Z4_{", t, "}}$")) %>% select(-t) %>% mutate(RESULTS = "variance", .before = 1),
    calibrationResult$z5_variance %>% rename(value = z5_variance) %>% mutate(target = paste0("$\\bm{Z5_{", t, "}}$")) %>% select(-t) %>% mutate(RESULTS = "variance", .before = 1),
    calibrationResult$z6_variance %>% rename(value = z6_variance) %>% mutate(target = paste0("$\\bm{Z6_{", j, ",", t, "}}$")) %>% select(-c(j, t)) %>% mutate(RESULTS = "variance", .before = 1),
    calibrationResult$z7_variance %>% rename(value = z7_variance) %>% mutate(target = paste0("$\\bm{Z7_{", t, "}}$")) %>% select(-t) %>% mutate(RESULTS = "variance", .before = 1)
  ) %>% pivot_wider(names_from = RESULTS) %>% mutate(gofM = (metamodel - true)^2 / variance, gofS = (simulation - true)^2 / variance )
  gofs <- sapply(c("0.001" = 0.001, "0.01" = 0.01, "0.05" = 0.05), function(x) sum((table_data$true*x)^2 / table_data$variance))
  
  table_data <- bind_rows(table_data, data.frame(target = c("tot"), true = NA, metamodel = NA, simulation = NA, variance = NA, gofM = sum(table_data$gofM), gofS = sum(table_data$gofS))) %>% mutate(target = factor(target))
  
  
  
  (tabu <- tabular(Format(fmt(digits =4)) * target ~ Heading() * identity * (Heading("$\\sigma^2$") * variance + Heading("true - $F(\\moutputs^{\\text{true}}, \\mparameters^{\\text{true}}) \\equiv 0$") * true + 
                                                                               Heading("metamodel - $\\mtargets^M = M(\\mparameters^*)$") * metamodel + Heading("simulation - $F(\\moutputs^{F,*}, \\mparameters^*) \\equiv 0$") * simulation + 
                                                                               Heading("$\\frac{(\\mtarget_j^M - \\mtarget_j^{\\text{true}})^2}{\\sigma^2}$") * Format(fmt(digits = 2)) * gofM + 
                                                                               Heading("$\\frac{(\\mtarget_j^{F,*} - \\mtarget_j^{\\text{true}})^2}{\\sigma^2}$") * Format(fmt(digits = 2)) * gofS), data = table_data))
  table_file <- file(paste0(outDir, "/", paste0("gof", identifier,".tex")))
  writeLines(c(
    paste0("%gofs: ", paste(names(gofs), gofs, sep = " -- ", collapse = "; ")),
    capture.output(toLatex(tabu))), table_file)
  close(table_file)  
})


local({
  table_data <- bind_rows(
    calibrationResult$mm_Z1 %>% filter(j == "AGR") %>% rename(value = mm_Z1) %>% mutate(target = paste0("$\\bm{Z1_{", j, ",", t, "}}$")) %>% select(-c(j, t)),
    # filter(t == 30, ..
    calibrationResult$mm_Z1 %>% filter(j != "AGR") %>% rename(value = mm_Z1) %>% mutate(target = paste0("$Z1_{", j, ",", t, "}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z2 %>% filter(t == 0) %>% rename(value = mm_Z2) %>% mutate(target = paste0("$Z2_{", t, "}$")) %>% select(-t),
    calibrationResult$mm_Z2 %>% filter(t == 30) %>% rename(value = mm_Z2) %>% mutate(target = paste0("$\\bm{Z2_{", t, "}}$")) %>% select(-t),
    calibrationResult$mm_Z3 %>% filter(t == 30, j == "LMN") %>% rename(value = mm_Z3) %>% mutate(target = paste0("$\\bm{Z3_{", j, ",", t, "}}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z3 %>% filter(t == 30, j != "LMN") %>% rename(value = mm_Z3) %>% mutate(target = paste0("$Z3_{", j, ",", t, "}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z4 %>% filter(t == 0) %>% rename(value = mm_Z4) %>% mutate(target = paste0("$Z4_{", t, "}$")) %>% select(-t),
    calibrationResult$mm_Z4 %>% filter(t == 30) %>% rename(value = mm_Z4) %>% mutate(target = paste0("$\\bm{Z4_{", t, "}}$")) %>% select(-t),
    calibrationResult$mm_Z5 %>% filter(t == 0) %>% rename(value = mm_Z5) %>% mutate(target = paste0("$Z5_{", t, "}$")) %>% select(-t),
    calibrationResult$mm_Z5 %>% filter(t == 30) %>% rename(value = mm_Z5) %>% mutate(target = paste0("$\\bm{Z5_{", t, "}}$")) %>% select(-t),
    calibrationResult$mm_Z6 %>% filter(t == 0) %>% rename(value = mm_Z6) %>% mutate(target = paste0("$Z6_{", j, ",", t, "}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z6 %>% filter(t == 30, j == "AGR") %>% rename(value = mm_Z6) %>% mutate(target = paste0("$\\bm{Z6_{", j, ",", t, "}}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z6 %>% filter(t == 30, j != "AGR") %>% rename(value = mm_Z6) %>% mutate(target = paste0("$Z6_{", j, ",", t, "}$")) %>% select(-c(j, t)),
    calibrationResult$mm_Z7 %>% filter(t == 0) %>% rename(value = mm_Z7) %>% mutate(target = paste0("$Z7_{", t, "}$")) %>% select(-t),
    calibrationResult$mm_Z7 %>% filter(t == 30) %>% rename(value = mm_Z7) %>% mutate(target = paste0("$\\bm{Z7_{", t, "}}$")) %>% select(-t),
  ) %>% pivot_wider(names_from = RESULTS) %>% mutate(target = factor(target)) %>% 
    mutate(diffMS = 100 * (metamodel - simulation) / simulation) %>% mutate(diffTS = 100 * (simulation - true) / true)

  (tabu <- tabular(Format(fmt(digits =4)) * target ~ Heading() * identity *  (Heading("true - $F(\\moutputs^{\\text{true}}, \\mparameters^{\\text{true}}) \\equiv 0$") * true + 
                                                                                Heading("metamodel - $\\mtargets^M = M(\\mparameters^*)$") * metamodel + Heading("simulation - $F(\\moutputs^{F,*}, \\mparameters^*) \\equiv 0$") * simulation + 
                                                                                Heading("$\\% \\frac{\\mtarget_j^M - \\mtarget_j^{\\text{true}}}{\\mtarget_j^{\\text{true}}}$") * Format(fmt(digits = 2)) * diffMS + 
                                                                                Heading("$\\% \\frac{\\mtarget_j^{F,*} - \\mtarget_j^{\\text{true}}}{\\mtarget_j^{\\text{true}}}$") * Format(fmt(digits = 2)) * diffTS), data = table_data))
    
  table_file <- file(paste0(outDir, "/", paste0("calibration", identifier,".tex")))
  writeLines(capture.output(toLatex(tabu)), table_file)
  close(table_file)
})

local({
  theta <- calibrationResult$theta %>% filter(field == "l") %>% select(-field) %>% mutate(bounds = "calibration", .after = coefficients)
  theta$coefficients <- factor(theta$coefficients, levels = levels(bounds$coefficients))
  table_data <- bind_rows(bounds, theta) %>% pivot_wider(names_from = bounds) %>% mutate(diff = 100 * (calibration - true) / true) 
  levels(table_data$coefficients) <- paste0("\\parameter{", sapply(levels(table_data$coefficients), quoteTargetForLatex), "}")
  (tabu <- tabular(Format(fmt(digits =4)) * coefficients ~ Heading() * identity * (Heading(".lo") * Format(fmt(digits = 2)) * lower + 
                                                                                     Heading(".up") * Format(fmt(digits = 2)) * upper + 
                                                                                     Heading("$\\sigma^2$") * Format(fmt(digits = 2)) * variance + 
                                                                                     Heading("prior") * prior + 
                                                                                     Heading("true ($\\mparameters^{\\text{true}}$)") * true + 
                                                                                     Heading("calibration ($\\mparameters^*$)") * calibration + 
                                                                                     Heading("$\\% \\frac{(\\mparameters^* - \\mparameters^{\\text{true}})}{\\mparameters^{\\text{true}}}$") * Format(fmt(digits = 2)) * diff), data = table_data))
  table_file <- file(paste0(outDir, "/", paste0("parameters", identifier,".tex")))
  writeLines(capture.output(toLatex(tabu)), table_file)
  close(table_file)
  
})

