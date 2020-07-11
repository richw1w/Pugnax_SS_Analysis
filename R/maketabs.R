library(broom)
library(kable)
library(kableExtra)


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
  

coeftab <- function(x, prepend = ""){
  
  #format
  out <- tidy(x) %>%
    mutate(statistic = round(statistic, 4),
           p.value = round(p.value, 3),
           std.error = round(std.error, 3),
           estimate = round(estimate, 3),
    ) %>%
    rename(Term = term,
           Coefficient = estimate,
           t = statistic,
           `Standard Error` = std.error,
           `P-Value` = p.value)
  
  bind_cols(tibble(` ` = c(prepend, rep("", nrow(out)-1))),
            out)
  
}

