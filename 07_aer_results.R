library(tidyverse)
setwd("C:/Users/geral/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios/")
vector_etfs <- c("eem", "eemo", "eemv", "fem", "ieem", "psrm", "vwo")
vector_distributions <- c("gauss","t","copula")  # Puedes incluir más distribuciones si es necesario
vector_h <- seq(100, 500, 50)
combinations <- expand.grid(distribution = vector_distributions,
                            etf = vector_etfs,
                            h = vector_h,
                            stringsAsFactors = FALSE)
names_vector <- apply(combinations, 1, function(row) {
  paste("data", row["distribution"], row["etf"], row["h"], sep = "_")
})
file_names <- apply(combinations, 1, function(row) {
  paste("orthogonal_temp/resultadosorthogonal/retornos",
        row["distribution"],
        row["etf"],
        row["h"],
        "portfolio_profits.csv",
        sep = "_")
})
list_data <- setNames(lapply(file_names, function(file) {
  if(file.exists(file)) {
    read.csv(file)
  } else {
    warning(paste("El archivo no existe:", file))
    NULL
  }
}), names_vector)

test.wilcox.function <- function(data_for_test, paired_data = FALSE, h_alternative = 'greater', significance_level = 0.05){
  colnames(data_for_test) <- c("none","cbr", "qs", "kz", "m", "q", "cbr_wcor", "qs_wcor", "kz_wcor", "m_wcor", "q_wcor")
  kz <- data_for_test[,"kz"]
  cbr <- data_for_test[,"cbr"]
  m <- data_for_test[,"m"]
  q <- data_for_test[,"q"]
  qs <- data_for_test[,"qs"]
  wilcoxtest_list <- list()
  wilcoxtest_vector <- c(
    wilcox.test(m,
                q,
                data = data_for_test,
                paired = paired_data,
                alternative=h_alternative)$p.value,
    wilcox.test(kz,
                q,
                data = data_for_test,
                paired = paired_data,
                alternative=h_alternative)$p.value,
    wilcox.test(qs,
                q, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value,
    wilcox.test(cbr,
                q, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value,
    wilcox.test(kz,
                m, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value,
    wilcox.test(qs,
                m, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value,
    wilcox.test(cbr,
                m, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value,
    wilcox.test(qs,
                kz, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value,
    wilcox.test(cbr,
                kz, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value,
    wilcox.test(cbr,
                qs, 
                data = data_for_test, 
                paired = paired_data, 
                alternative=h_alternative)$p.value
  )
  names(wilcoxtest_vector) <- c("m-q","kz-q","qs-q","cbr-q","kz-m","qs-m","cbr-m","qs-kz","cbr-kz","cbr-qs")
  wilcoxtest_list[[1]] = wilcoxtest_vector
  wilcoxtest_list[[2]] = wilcoxtest_vector<=significance_level
  return(wilcoxtest_list)

}

# Función para agrupar los resultados y añadir columnas adicionales
aggregate_results <- function(result_list, combinations, result_data_number = TRUE) {
  all_results <- do.call(rbind, lapply(seq_along(result_list), function(i) {
    if (!is.null(result_list[[i]])) {
      data.frame(
        h = combinations$h[i],
        distribution = combinations$distribution[i],
        etf = combinations$etf[i],
        if(result_data_number){t(result_list[[i]][[1]])}else{t(result_list[[i]][[2]])},  # extrae los p-values
        stringsAsFactors = FALSE
      )
    }
  }))
  return(all_results)
}

# Ejecuta la función Wilcoxon y agrupa los resultados
results_list <- lapply(list_data, test.wilcox.function, paired_data = FALSE, h_alternative = 'greater', significance_level = 0.05)
final_dataframe <- aggregate_results(results_list, combinations, result_data_number = FALSE)

final_dataframe <- final_dataframe %>%
  arrange(etf, desc(distribution))

write.csv(final_dataframe, 
          file = "C:/Users/geral/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios/orthogonal_temp/resultadosorthogonal/p.values_tabla.csv", row.names = FALSE)
