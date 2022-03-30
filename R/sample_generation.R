source("steps/gams_path.R")
source("steps/dependencies.R")
source("functions/generate_sim_names.R")
source("functions/gdx_helpers.R")
source("steps/base_data.R")

identifier <- "-10"
seed <- 23
baseParamsList <- list(
  ror = c(0.04, 0.06), 
  dep = c(0.03, 0.05),
  pop = c(0.01, 0.03),
  zeta = c(0.98, 1.02),
  omega = c(0 , 1),
  sigma = c(1, 18),
  psi  = c(1, 18)
)

paramsList <- local({
  sigmaParams <- lapply(sectors, function(s) { baseParamsList$sigma })
  names(sigmaParams) <- paste0("sigma_", sectors)
  psiParams <- lapply(sectors, function(s) { baseParamsList$sigma })
  names(psiParams) <- paste0("psi_", sectors)
  
  return(c(baseParamsList[1:5], sigmaParams, psiParams))
})

estimationLHSSample <- lhs.design(length(paramsList)^2*10, length(paramsList), type="random", factor.names = paramsList, seed = seed, digits = 4)
estimationSample <-  estimationLHSSample %>% as_tibble() %>% mutate(MM_SIM = generateSimNames("MM_", n()), .before = 1)
validationLHSSample <- lhs.design(length(paramsList)^2*2, length(paramsList), type="random", factor.names = paramsList, seed = seed, digits = 4)
validationSample <- validationLHSSample %>% as_tibble() %>% mutate(MM_SIM = generateSimNames("VAL_", n()), .before = 1)
trueLHSSample <- lhs.design(length(paramsList)*2, length(paramsList), type="optimum", factor.names = paramsList, seed = seed, digits = 4)
trueSample <- trueLHSSample %>% as_tibble() %>% slice(length(paramsList)) %>% mutate(MM_SIM = generateSimNames("TRUE_", n()), .before = 1)

outDir <- "out"
if(!dir.exists(outDir)) {
  dir.create(outDir)
}
save(estimationLHSSample, estimationSample, validationLHSSample, validationSample, trueSample, paramsList, file = file.path(outDir, paste0("sample", identifier,".RData")))

sample <- bind_rows(estimationSample, validationSample, trueSample)
computeSample <- function(sample) {
  currentWD <- getwd()
  setwd("../gams/")
  system("gams sam.gms")
  sampleToGDX(sample, "sample.gdx")
  system("gams mm_simulations.gms lo=0")
  result <- list(
    Z1 = rgdx.param("result.gdx", "mm_Z1", compress = T),
    Z2 = rgdx.param("result.gdx", "mm_Z2", compress = T),
    Z3 = rgdx.param("result.gdx", "mm_Z3", compress = T),
    Z4 = rgdx.param("result.gdx", "mm_Z4", compress = T),
    Z5 = rgdx.param("result.gdx", "mm_Z5", compress = T),
    Z6 = rgdx.param("result.gdx", "mm_Z6", compress = T),
    Z7 = rgdx.param("result.gdx", "mm_Z7", compress = T)
  )
  setwd(currentWD)
  file.copy("../gams/result.gdx", file.path(outDir, paste0("result", identifier, ".gdx")), overwrite = T)
  return(result)
}

result <- computeSample(sample)
save(result, file = file.path(outDir, paste0("result", identifier,".RData")))
