# Definir entorno y cargar datos
setwd("C:/Users/YOGA/Desktop/repositories/research/orthogonal_portfolios")

# libraries
library(tidyverse)
library(ggplot2)
library(mvtnorm)
library(gridExtra)
library(copula)

# functions
source("code/00_aer_paper_general.r")
source("code/00_aer_paper_rules_2.r")

# load data
eem  <- read.csv("data_clean/EEM.US_risk_ports_clean.csv")[, -1]
eemo <- read.csv("data_clean/EEMO.US_risk_ports_clean.csv")[, -1]
eemv <- read.csv("data_clean/EEMV.US_risk_ports_clean.csv")[, -1]
fem  <- read.csv("data_clean/FEM.US_risk_ports_clean.csv")[, -1]
ieem <- read.csv("data_clean/IEEM.LN_risk_ports_clean.csv")[, -1]
psrm <- read.csv("data_clean/PSRM.LN_risk_ports_clean.csv")[, -1]

# etfs list
etfs <- list(eem=eem, eemo=eemo, eemv=eemv, fem=fem, ieem=ieem, psrm=psrm)
n_etfs <- length(etfs)

# Función para ajustar y comparar varias copulas
fit_copulas <- function(data_returns){
  
  U <- pobs(data_returns) 
  d <- ncol(U)

  # Ajuste a diferentes cópulas
  copulas_list <- list(
    normal = normalCopula(dim = d),
    t = tCopula(dim = d, df = 4, df.fixed = TRUE), # df fijados en 4
    clayton = claytonCopula(dim = d),
    gumbel = gumbelCopula(dim = d),
    frank = frankCopula(dim = d)
  )

  fits <- map(copulas_list, ~ fitCopula(.x, U, method = "ml"))

  # Aplicar tests de bondad solo para copulas diferentes de "t"
  gof_tests <- map(names(fits), function(name){
    if(name != "t"){
      gofCopula(fits[[name]]@copula, U)
    } else {
      NA
    }
  })

  # Crear resumen con resultados
  comparisons <- tibble(
    Copula = names(fits),
    AIC = map_dbl(fits, AIC),
    BIC = map_dbl(fits, BIC),
    p_value_CvM = map_dbl(gof_tests, ~ ifelse(is.na(.x), NA, .x$p.value["CvM"])),
    p_value_AD = map_dbl(gof_tests, ~ ifelse(is.na(.x), NA, .x$p.value["AD"])),
    Test_result = case_when(
      is.na(p_value_CvM) ~ "Test no aplicado para copula t",
      p_value_CvM < 0.05 | p_value_AD < 0.05 ~ "Se rechaza que la copula es adecuada para los datos",
      TRUE ~ "No se rechaza que la copula es adecuada para los datos"
    )
  ) %>% arrange(AIC)
  
  return(comparisons)
}

# Aplicar función a cada ETF y guardar resultados
copula_results <- map_df(names(etfs), function(etf_name){
  result <- fit_copulas(etfs[[etf_name]])
  result %>% mutate(etf = etf_name)
})

# Guardar resultados en un CSV
write.csv(copula_results, file = "data_clean/copula_results.csv", row.names = FALSE)

# Mostrar resultados
print(glimpse(copula_results))