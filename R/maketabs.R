library(broom)
anovatab <- function(x, prepend = ""){
  
  tab <- car::Anova(x)

  #format
  out <- tidy(tab) %>%
    mutate(statistic = round(statistic, 4),
           p.value = round(p.value, 3)) %>%
    rename(Term = term,
           `LR Chisq` = statistic,
           DF = df,
           `P-Value` = p.value)
  
  bind_cols(tibble(` ` = c(prepend, rep("", nrow(out)-1))),
            out)
  
}
  
