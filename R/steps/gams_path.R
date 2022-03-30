if(Sys.info()["sysname"] == "Linux") {
    gamsPath <- "~/development/environments/gams/current"
} else {
  potentialDirs <- sort(list.dirs("C:/GAMS", recursive = F), decreasing = T)
  if(length(potentialDirs) > 0) {
    gamsPath <- potentialDirs[1]
  } else {
    stop("Could not find GAMS in default location, please set gamsPath variable to your installation location!")
  }
}
