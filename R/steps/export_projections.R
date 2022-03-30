projections <- lapply(toFilteredResults(result, trueSample$MM_SIM, activeT = activeTimepoints), function(x) {
  x <- x %>% select(-MM_SIM)
  if("j" %in% colnames(x)) {
    attr(x, "domains") <- c("t", "j")
    x %>% select(t, j, value)
  } else {
    attr(x, "domains") <- c("t")
    x %>% select(t, value)
  }
})
for(n in names(projections)) {
  attr(projections[[n]], "symName") <- paste0(n, "_projection")
}

do.call(wgdx.lst, c(list(gdxName = "../gams/calibration/projections.gdx"), projections, list(prepareSetForGDX(levels(projections$Z1$j), "j"), prepareSetForGDX(as.character(activeTimepoints), "t"))))
