# measures of uncertainty
se <- function(x) sqrt(var(x)/length(x))
LCL <- function(x) quantile(x, probs=0.05)
UCL <- function(x) quantile(x, probs=0.95)
