prepareSetForGDX <- function(data, name) {
  outDF <- data[!is.na(data)]
  outLst <- list(
    list(name=name, type='set', uels=c(list(outDF)))
  )
  return(outLst)
}

sampleToGDX <- function(sample, target) {
  scalars <- lapply(c("ror", "dep", "pop", "zeta", "omega"), function(p) {
    param <- sample %>% select(MM_SIM, all_of(p)) %>% rename(value = !!p) %>% mutate(MM_SIM = factor(MM_SIM))
    attr(param, "symName") <- paste0("mm_", p)
    return(param)
  })
  # parameters <- lapply(c("tp", "sigma", "psi"), function(p) {
  parameters <- lapply(c("sigma", "psi"), function(p) {
      cols <- paste0(p, "_", sectors)
    param <- sample %>% select(MM_SIM, all_of(cols)) %>% pivot_longer(-MM_SIM, names_to = "j", names_prefix = paste0(p, "_")) %>% mutate(j = factor(j), MM_SIM = factor(MM_SIM))
    attr(param, "symName") <- paste0("mm_", p)
    return(param)
  })
  wgdx.lst(target, c(prepareSetForGDX(sample$MM_SIM, "MM_SIM"), prepareSetForGDX(sectors, "j"), scalars, parameters))
}