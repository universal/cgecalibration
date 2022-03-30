fmt <- function(x, digits, ...) {
  s <- latexNumeric(format(round(x, digits), scientific = FALSE))  
  s[is.na(x)] <- "-"
  return(s)
}

quoteTargetForLatex <- function(target) {
  if(grepl("_", target)) {
    split <- str_split(target, "_")  
    paste0("$", split[[1]][1], "_{", split[[1]][2], "}$")
  } else {
    paste0("$", target, "$")
  }
}

local({
  table_data <- estimations$t30$evaluations %>% select(target, aer, mean, rmse, r_squared) %>% mutate(target = sapply(target, quoteTargetForLatex)) %>% mutate(target = factor(target))
  (tabu <- tabular(Format(fmt(digits =4)) * target ~ Heading() * identity * (Heading("AER") * aer + Heading("mean") * mean + Heading("RMSE") * rmse + Heading("$R^2$") * r_squared), data = table_data))
  table_file <- file(paste0(outDir, "/", paste0("validation", identifier,".tex")))
  writeLines(capture.output(toLatex(tabu)), table_file)
  close(table_file)
})




