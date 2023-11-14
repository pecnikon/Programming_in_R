t.test.jv = function(data,
                     mu = 0,
                     conf.level = 0.95,
                     alternative = c("two.sided", "less", "greater")) {
  data_mean = mean(data)
  data_sd = sd(data)
  n = length(data)
  TT = ((data_mean - mu) / data_sd) * sqrt(n)
  pt = pt(q = TT, df = n - 1)
  if (alternative == "two.sided") {
    leva_strana = data_mean - qt(p = 1 - ((1 - conf.level) / 2), df = n - 1) *
      (data_sd / sqrt(n))
    prava_strana = data_mean + qt(p = 1 - ((1 - conf.level) / 2), df = n -
                                    1) * (data_sd / sqrt(n))
    vystup = cat(
      paste(
        "testovaci statistika:", round(TT, digits = 5), "\n",
        "df:", n - 1, "\n",
        "p-value:", round(2 * (1 - pt(q = abs(TT), df = n-1)), digits = 5), "\n",
        "alternative hypothesis: true mean is not equal to", mu, "\n",
        conf.level * 100, "percent confidence interval:", round(leva_strana, digits = 2), round(prava_strana, digits = 2))
    )
  }
  if (alternative == "less") {
    prava_strana = data_mean + qt(p = conf.level, df = n - 1) * (data_sd) /
      sqrt(n)
    vystup = cat(
      paste(
        "testovaci statistika:", round(TT, digits = 5), "\n",
        "df:", n - 1, "\n",
        "p-value:", round(pt, digits = 5), "\n",
        "alternative hypothesis: true mean is less than", mu, "\n",
        conf.level * 100, "percent confidence interval:", -Inf, round(prava_strana, digits = 2))
    )
  }
  if (alternative == "greater") {
    leva_strana = data_mean - qt(p = conf.level, df = n - 1) * (data_sd) / sqrt(n)
    vystup = cat(
      paste(
        "testovaci statistika:", round(TT, digits = 5), "\n",
        "df:", n - 1, "\n",
        "p-value:", round((1-pt), digits = 5), "\n",
        "alternative hypothesis: true mean is greater than", mu, "\n",
        conf.level * 100, "percent confidence interval:", round(leva_strana, digits = 2), Inf)
    )
  }
  return(invisible(vystup))
}

