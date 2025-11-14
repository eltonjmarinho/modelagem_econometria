# main.R - Controller
# -----------------------------------------------------------------------------
# Este script orquestra todo o fluxo de trabalho do projeto:
# 1. Carrega as bibliotecas necessárias.
# 2. Carrega os módulos de modelo (dados e análise).
# 3. Carrega os módulos de visão (gráficos).
# 4. Executa o fluxo de análise de ponta a ponta.
# -----------------------------------------------------------------------------

# --- 1. Carga de Pacotes ---
# Lista de pacotes necessários
packages <- c("tidyverse", "rb3", "forecast", "tseries", "remotes")

# Função para instalar pacotes se não estiverem presentes
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    if (pkg == "rb3") {
      message("Instalando o pacote 'rb3' do GitHub...")
      # Solução para erro de codificação no Windows
      # Muda temporariamente o locale para 'C' para a instalação
      current_locale <- Sys.getlocale("LC_ALL")
      Sys.setlocale("LC_ALL", "C")
      remotes::install_github("ropensci/rb3")
      Sys.setlocale("LC_ALL", current_locale) # Restaura o locale original
    } else {
      message(paste("Instalando o pacote:", pkg))
      install.packages(pkg)
    }
  }
  # Carrega o pacote após a instalação
  library(pkg, character.only = TRUE)
}

# Itera sobre a lista e instala/carrega os pacotes
sapply(packages, install_if_missing)

message("Todos os pacotes foram carregados com sucesso.")

# --- 2. Carregar Módulos ---
source("models/data_model.R")
source("models/analysis_model.R")
source("views/plot_view.R")

# --- 3. Execução do Fluxo Principal ---
message("Iniciando o fluxo principal de análise...")

# 3.1. Obter os dados
coffee_data <- fetch_coffee_data()
message("Dados de café carregados com sucesso. Amostra:")
print(head(coffee_data))

# 3.2. Visualizar a série temporal
message("Gerando gráfico da série temporal...")
plot_time_series(coffee_data, title = "Preços Futuros do Café (CFE)")

# 3.3. Teste de Estacionariedade
# Converter para objeto de série temporal
coffee_ts <- ts(coffee_data$price)
# Realizar o teste
stationarity_test_result <- perform_stationarity_test(coffee_ts)

# 3.4. Análise de Autocorrelação
message("--- Gerando gráficos ACF e PACF ---")
message("Analisando a série original (provavelmente não-estacionária):")
plot_acf_pacf(coffee_ts, title_suffix = " - Série Original")

message("
Analisando a série diferenciada (para identificar p, q):")
plot_acf_pacf(diff(coffee_ts), title_suffix = " - Série Diferenciada")

# 3.6. Análise de Resíduos e Seleção de Modelo
message("--- Iniciando Análise de Resíduos para Seleção de Modelo ---")
for (model_name in names(fitted_models)) {
  model <- fitted_models[[model_name]]
  analyze_model_residuals(model_name, model)
  plot_residual_diagnostics(model_name, model)
}

# 3.7. Conclusão Final
message("--- Conclusão da Análise ---")
message("A seleção do melhor modelo é baseada em dois critérios principais:")
message("1. Resíduos 'bem comportados': Ausência de autocorrelação (Teste Ljung-Box com p > 0.05) e normalidade.")
message("2. Menor valor de AIC (Akaike Information Criterion), que equilibra ajuste e complexidade.")
message("Com base nos resultados:")
message("- Os modelos AR(1) e ARMA(1,1) provavelmente falharão no teste de Ljung-Box, pois não tratam a não-estacionariedade da série, resultando em resíduos correlacionados.")
message("- O modelo ARIMA(1,1,1) é o candidato mais forte. A diferenciação (o 'I' do meio) torna a série estacionária, e os componentes AR e MA modelam a estrutura de correlação restante. Espera-se que seus resíduos não apresentem autocorrelação significativa (p-valor do Ljung-Box > 0.05).")
message("RECOMENDAÇÃO: O modelo ARIMA(1,1,1) é o mais apropriado. Verifique os resultados dos testes de resíduos para confirmar, mas teoricamente ele é o mais robusto para esta série temporal.")

message("Projeto concluído!")
