generateSimNames <- function(prefix, n) {
  sapply((1:n), function(x) {
    paste0(prefix, sprintf(paste0("%0", str_length(n), "d"), x))
  })
}