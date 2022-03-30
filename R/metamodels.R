source("steps/gams_path.R")
source("steps/dependencies.R")
source("steps/base_data.R")
source("functions/model_estimators.R")
source("functions/models_to_gdx.R")

identifier <- "-10"
outDir <- "out"

source("steps/load_results.R")
activeTimepoints <- c(t0 = 0, t30 = 30)
source("steps/estimate_models.R")
source("steps/validation_table.R")

source("steps/export_models.R")
source("steps/export_projections.R")

runCalibration <- function() {
  currentWD <- getwd()
  setwd("../gams/")
  system("gams calibration.gms")
  rawTheta <- rgdx("calibration.gdx", list(name = "theta", field = "all"))
  theta <- rawTheta$val %>% as.data.frame()
  theta[, 1] <- rawTheta$uels[[1]][theta[, 1]]
  theta[, 2] <- rawTheta$uels[[2]][theta[, 2]]
  colnames(theta) <- c("coefficients", "field", "value")
  parameters <- c(unlist(lapply(1:7, function(z) { c(paste0("mm_Z", z), paste0("z", z, "_error"), paste0("z", z, "_variance")) })), "delta_to_true")
  result <- lapply(parameters,
         function(par) {
           rgdx.param("calibration.gdx", par, compress = T)
         }
       )
  names(result) <- parameters
  result$theta <- theta
  setwd(currentWD)
  file.copy("../gams/calibration.gdx", file.path(outDir, paste0("calibration", identifier, ".gdx")), overwrite = T)
  return(result)
}

calibrationResult <- runCalibration()


source("steps/calibration_tables.R")
