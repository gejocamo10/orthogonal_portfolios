library(tidyverse)
setwd("C:/Users/geral/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios")

vector_etfs <- c("eem", "eemo", "eemv", "fem", "ieem", "psrm", "vwo")
vector_distributions <- c("gauss", "t", "copula")  # Puedes incluir más distribuciones si es necesario
vector_h <- seq(100, 500, 50)
combinations <- expand.grid(distribution = vector_distributions,
                            etf = vector_etfs,
                            h = vector_h,
                            stringsAsFactors = FALSE)
names_vector <- apply(combinations, 1, function(row) {
  paste("data", row["distribution"], row["etf"], row["h"], sep = "_")
})
file_names <- apply(combinations, 1, function(row) {
  paste("data_simulation/retornos",
        row["distribution"],
        row["etf"],
        row["h"],
        "portfolio_profits.csv",
        sep = "_")
})
list_data <- setNames(lapply(file_names, function(file) {
  if (file.exists(file)) {
    read.csv(file)
  } else {
    warning(paste("El archivo no existe:", file))
    NULL
  }
}), names_vector)

# Función modificada para aplicar t.test o wilcox.test según la distribución
test_function <- function(data_for_test, distribution, wcor = FALSE, paired_data = FALSE, h_alternative = 'greater', significance_level = 0.05) {
  colnames(data_for_test) <- c("none", "cbr", "qs", "kz", "m", "q", "cbr_wcor", "qs_wcor", "kz_wcor", "m_wcor", "q_wcor")
  
  if (wcor) {
    kz <- data_for_test[,"kz_wcor"]
    cbr <- data_for_test[,"cbr_wcor"]
    m <- data_for_test[,"m_wcor"]
    q <- data_for_test[,"q_wcor"]
    qs <- data_for_test[,"qs_wcor"]
  } else {
    kz <- data_for_test[,"kz"]
    cbr <- data_for_test[,"cbr"]
    m <- data_for_test[,"m"]
    q <- data_for_test[,"q"]
    qs <- data_for_test[,"qs"]
  }
  
  test_list <- list()
  
  # Función interna para seleccionar la prueba adecuada
  perform_test <- function(x, y) {
    if (distribution == 'copula') {
      # Usar prueba de Wilcoxon
      p_value <- wilcox.test(x, y, paired = paired_data, alternative = h_alternative)$p.value
    } else {
      # Usar t.test
      p_value <- t.test(x, y, paired = paired_data, alternative = h_alternative, var.equal = TRUE)$p.value
    }
    return(p_value)
  }
  
  test_vector <- c(
    perform_test(m, q),
    perform_test(kz, q),
    perform_test(qs, q),
    perform_test(cbr, q),
    perform_test(kz, m),
    perform_test(qs, m),
    perform_test(cbr, m),
    perform_test(qs, kz),
    perform_test(cbr, kz),
    perform_test(cbr, qs)
  )
  names(test_vector) <- c("m-q", "kz-q", "qs-q", "cbr-q", "kz-m", "qs-m", "cbr-m", "qs-kz", "cbr-kz", "cbr-qs")
  
  test_list[[1]] <- test_vector
  test_list[[2]] <- test_vector <= significance_level
  return(test_list)
}

# Función para agrupar los resultados y añadir columnas adicionales
aggregate_results <- function(result_list, combinations, result_data_number = TRUE) {
  all_results <- do.call(rbind, lapply(seq_along(result_list), function(i) {
    if (!is.null(result_list[[i]])) {
      if (result_data_number) {
        data_row <- t(result_list[[i]][[1]])
      } else {
        # Convertir valores lógicos a enteros y mantener los nombres
        logical_vector <- result_list[[i]][[2]]
        integer_vector <- as.integer(logical_vector)
        names(integer_vector) <- names(logical_vector)
        data_row <- t(integer_vector)
      }
      data.frame(
        h = combinations$h[i],
        distribution = combinations$distribution[i],
        etf = combinations$etf[i],
        data_row,
        stringsAsFactors = FALSE
      )
    }
  }))
  return(all_results)
}

# Ejecuta la función de prueba y agrupa los resultados
results_list <- lapply(seq_along(list_data), function(i) {
  data_for_test <- list_data[[i]]
  if (!is.null(data_for_test)) {
    distribution <- combinations$distribution[i]
    test_result <- test_function(data_for_test, distribution = distribution, wcor = FALSE, paired_data = FALSE, h_alternative = 'greater', significance_level = 0.05)
    return(test_result)
  } else {
    return(NULL)
  }
})

final_dataframe <- aggregate_results(results_list, combinations, result_data_number = FALSE)

final_dataframe <- final_dataframe %>%
  arrange(etf, desc(distribution))

write.csv(final_dataframe,
          file = "C:/Users/geral/OneDrive - Universidad del Pacífico/Research/Academic/orthogonal_portfolios/p.values_tabla.csv", row.names = FALSE)
