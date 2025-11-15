# models/analysis_model.R

# Função para formatar o output de um teste como HTML
format_test_as_html <- function(test_name, test_object, conclusion) {
  # Captura o output do teste que seria impresso no console
  test_output <- capture.output(print(test_object))
  
  # Formata como uma string HTML usando <pre> para manter a formatação
  html_string <- paste(
    "<h4>", test_name, "</h4>",
    "<pre>", paste(test_output, collapse = "\n"), "</pre>",
    "<p><b>Conclusão:</b> ", conclusion, "</p>",
    collapse = "\n"
  )
  
  return(html_string)
}

# Função para realizar o teste de estacionariedade e retornar o resultado em HTML
perform_stationarity_test <- function(time_series) {
  adf_test <- tseries::adf.test(time_series)
  
  # Define a conclusão com base no p-valor
  if (adf_test$p.value < 0.05) {
    conclusion <- "A série é provavelmente estacionária (p < 0.05)."
  } else {
    conclusion <- "A série é provavelmente não-estacionária (p >= 0.05)."
  }
  
  # Formata e retorna o resultado
  format_test_as_html(
    "Teste de Estacionariedade (Augmented Dickey-Fuller)",
    adf_test,
    conclusion
  )
}

# Função para analisar os resíduos de um modelo e retornar os resultados em HTML
analyze_model_residuals <- function(model_name, model_object) {
  res <- residuals(model_object)
  results_list <- list()
  
  # 1. Teste de Normalidade (Shapiro-Wilk)
  if (length(res) < 5000) {
    shapiro_test <- shapiro.test(res)
    if (shapiro_test$p.value > 0.05) {
      conclusion <- "Os resíduos parecem ser normalmente distribuídos (p > 0.05)."
    } else {
      conclusion <- "Os resíduos não parecem ser normalmente distribuídos (p <= 0.05)."
    }
    results_list$shapiro <- format_test_as_html(
      "Teste de Normalidade (Shapiro-Wilk)",
      shapiro_test,
      conclusion
    )
  } else {
    results_list$shapiro <- "<p><b>Teste de Normalidade (Shapiro-Wilk):</b> Amostra muito grande (>5000), a análise visual do histograma é mais indicada.</p>"
  }
  
  # 2. Teste de Autocorrelação (Ljung-Box)
  # fitdf é o número de parâmetros do modelo (p + q)
  fitdf <- sum(model_object$arma[c(1, 2)])
  ljung_box_test <- Box.test(res, lag = 10, type = "Ljung-Box", fitdf = fitdf)
  
  if (ljung_box_test$p.value > 0.05) {
    conclusion <- "Não há evidência de autocorrelação nos resíduos (p > 0.05). Os resíduos são considerados ruído branco."
  } else {
    conclusion <- "Há evidência de autocorrelação nos resíduos (p <= 0.05). O modelo pode ser inadequado."
  }
  results_list$ljung_box <- format_test_as_html(
    "Teste de Autocorrelação (Ljung-Box)",
    ljung_box_test,
    conclusion
  )
  
  return(results_list)
}