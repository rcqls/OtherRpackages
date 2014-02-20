sigma2.test <- function (x, alternative = "two.sided", var0 = 1, conf.level = 0.95) {
  choices <- c("two.sided", "greater", "less")
  alt <- pmatch(alternative, choices)
  alternative <- choices[alt]
  if (length(conf.level) != 1 || is.na(conf.level) || conf.level <
      0 || conf.level > 1)
    stop("conf.level must be a number between 0 and 1")
  dname <- deparse(substitute(x))
  nx <- length(x)
  if (nx <= 2)
    stop("not enough x observations")
  gradiliberta <- nx - 1
  sx <- sd(x)
  estimate <- sx^2
  s2obs <- (nx - 1) * sx^2/var0
  method <- c("One-sample Chi-squared test for given variance")
  names(estimate) <- c("var of x")
  if (alternative == "less") {
    pval <- pchisq(s2obs, df = nx - 1)
    cint <- c(NA, (nx - 1) * sx^2/qchisq(p = conf.level,
                                         df = nx - 1))
  }
  else if (alternative == "greater") {
    pval <- 1 - pchisq(s2obs, df = nx - 1)
    cint <- c((nx - 1) * sx^2/qchisq(p = 1 - conf.level,
                                     df = nx - 1), NA)
  }
  else {
    pval <- 2 * min(pchisq(s2obs, df = nx - 1), 1 - pchisq(s2obs,
                                    df = nx - 1))
    alpha <- 1 - conf.level
    cint <- c((nx - 1) * sx^2/qchisq(p = 1 - (1 - conf.level)/2,
                                     df = nx - 1), (nx - 1) * sx^2/qchisq(p = (1 - conf.level)/2,
                                       df = nx - 1))
  }
  names(s2obs) <- "X-squared"
  names(gradiliberta) <- "df"
  names(var0) <- "variance"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = s2obs, parameter = gradiliberta,
               p.value = pval, conf.int = cint, estimate = estimate,
               null.value = var0, alternative = alternative, method = method,
               data.name = dname)
  attr(rval, "class") <- "htest"
  return(rval)
}
