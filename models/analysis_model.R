# Função para analisar os resíduos de um modelo
analyze_model_residuals <- function(model_name, model_object) {
  
  # Função para realizar o teste de estacionariedade
  perform_stationarity_test <- function(time_series) {
    # Use Augmented Dickey-Fuller test from tseries package
    adf_test <- tseries::adf.test(time_series)
    print(adf_test)
    # Return the p-value
    return(adf_test$p.value)
  }



  message(paste("\n--- Análise de Resíduos para o Modelo:", model_name, "---"))
  
  res <- residuals(model_object)
  
  # 1. Teste de Normalidade (Shapiro-Wilk)
  # Nota: Para amostras grandes (>5000), o teste pode ser muito sensível.
  if (length(res) < 5000) {
    shapiro_test <- shapiro.test(res)
    message("\n1. Teste de Normalidade (Shapiro-Wilk):")
    print(shapiro_test)
    if (shapiro_test$p.value > 0.05) {
      message("Conclusão: p-valor > 0.05. Os resíduos parecem ser normalmente distribuídos.")
    } else {
      message("Conclusão: p-valor <= 0.05. Os resíduos não parecem ser normalmente distribuídos.")
    }
  } else {
    message("\n1. Teste de Normalidade (Shapiro-Wilk):")
    message("Amostra muito grande para o teste de Shapiro-Wilk. A análise visual é mais indicada.")
  }
  
  # 2. Teste de Autocorrelação (Ljung-Box)
  ljung_box_test <- Box.test(res, lag = 10, type = "Ljung-Box", fitdf = sum(model_object$arma[1:2]))
  message("\n2. Teste de Autocorrelação (Ljung-Box):")
  print(ljung_box_test)
  if (ljung_box_test$p.value > 0.05) {
    message("Conclusão: p-valor > 0.05. Não há evidência de autocorrelação nos resíduos (resíduos são ruído branco).")
  } else {
    message("Conclusão: p-valor <= 0.05. Há evidência de autocorrelação nos resíduos (modelo pode ser inadequado).")
  }
}
