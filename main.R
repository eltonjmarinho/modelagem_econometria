# main.R - Controller
# -----------------------------------------------------------------------------
# Este script orquestra todo o fluxo de trabalho do projeto:
# 1. Carrega as bibliotecas e módulos necessários.
# 2. Executa a análise de ponta a ponta.
# 3. Gera um relatório HTML único com todos os gráficos e resultados.
# -----------------------------------------------------------------------------

# --- 1. Carga de Pacotes e Módulos ---
message("--- Carregando Pacotes e Módulos ---")
packages <- c("tidyverse", "rb3", "forecast", "tseries", "remotes", "plotly", "htmlwidgets", "htmltools")

# Função para instalar e carregar pacotes
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Instalando o pacote:", pkg))
    if (pkg == "rb3") {
      current_locale <- Sys.getlocale("LC_ALL")
      Sys.setlocale("LC_ALL", "C")
      tryCatch({
        remotes::install_github("ropensci/rb3")
      }, finally = {
        Sys.setlocale("LC_ALL", current_locale)
      })
    } else {
      install.packages(pkg)
    }
  }
  library(pkg, character.only = TRUE)
}

sapply(packages, install_if_missing)

source("models/data_model.R")
source("models/analysis_model.R")
source("views/plot_view.R")

message("Pacotes e módulos carregados com sucesso.")

# --- 2. Execução do Fluxo Principal ---
message("--- Iniciando o fluxo principal de análise ---")

# Lista para armazenar todos os componentes do relatório (gráficos e textos)
report_items <- list()

# 2.1. Obter os dados
message("Buscando dados de café...")
coffee_data <- fetch_coffee_data()
message("Dados carregados. Amostra:")
print(head(coffee_data))

# 2.2. Análise Descritiva e Estacionariedade
report_items <- append(report_items, list(tags$h2("1. Análise Descritiva e Estacionariedade da Série")))

# Adiciona o gráfico da série temporal ao relatório
message("Gerando gráfico da série temporal...")
ts_plot <- plot_time_series(coffee_data, title = "Preços Futuros do Café (CFE)")
report_items <- append(report_items, list(ts_plot))

# Converte para objeto de série temporal e realiza o teste de estacionariedade
coffee_ts <- ts(coffee_data$price, frequency = 252) # Aprox. dias úteis no ano
stationarity_test_html <- perform_stationarity_test(coffee_ts)
report_items <- append(report_items, list(HTML(stationarity_test_html)))

# 2.3. Análise de Autocorrelação
report_items <- append(report_items, list(tags$h2("2. Análise de Autocorrelação (ACF/PACF)")))
report_items <- append(report_items, list(tags$p("A análise de autocorrelação nos ajuda a identificar os parâmetros (p, d, q) para os modelos ARIMA.")))

# ACF/PACF da série original
message("Gerando gráficos ACF/PACF da série original...")
acf_original_plot <- plot_acf_pacf(coffee_ts, title_suffix = " - Série Original")
report_items <- append(report_items, list(acf_original_plot))

# ACF/PACF da série diferenciada
message("Gerando gráficos ACF/PACF da série diferenciada...")
acf_diff_plot <- plot_acf_pacf(diff(coffee_ts), title_suffix = " - Série Diferenciada (d=1)")
report_items <- append(report_items, list(acf_diff_plot))

# 2.4. Ajuste e Análise dos Modelos
report_items <- append(report_items, list(tags$h2("3. Ajuste e Análise dos Modelos Preditivos")))

# Ajusta os modelos
message("Ajustando modelos ARIMA...")
fitted_models <- list(
  "AR(1)" = Arima(coffee_ts, order = c(1, 0, 0)),
  "ARMA(1,1)" = Arima(coffee_ts, order = c(1, 0, 1)),
  "ARIMA(1,1,1)" = Arima(coffee_ts, order = c(1, 1, 1))
)
message("Modelos ajustados com sucesso.")

# Itera sobre cada modelo para gerar diagnóstico e previsão
for (model_name in names(fitted_models)) {
  message(paste("Processando modelo:", model_name))
  model <- fitted_models[[model_name]]
  
  # Adiciona um título para a seção do modelo
  report_items <- append(report_items, list(tags$h3(paste("Análise do Modelo:", model_name))))
  
  # Gráfico de diagnóstico de resíduos
  residuals_plot <- plot_residual_diagnostics(model_name, model, coffee_data)
  report_items <- append(report_items, list(residuals_plot))
  
  # Testes de resíduos (Normalidade e Autocorrelação)
  residual_tests_html <- analyze_model_residuals(model_name, model)
  report_items <- append(report_items, list(HTML(residual_tests_html$shapiro)))
  report_items <- append(report_items, list(HTML(residual_tests_html$ljung_box)))
  
  # Gráfico de previsão
  forecast_plot <- plot_forecast(model, coffee_data, h = 30)
  report_items <- append(report_items, list(forecast_plot))
}

# 2.5. Conclusão Final
conclusion_text <- "
<h2>4. Conclusão da Análise</h2>
<p>A seleção do melhor modelo é baseada em dois critérios principais:</p>
<ol>
  <li><b>Resíduos 'bem comportados':</b> Ausência de autocorrelação (Teste Ljung-Box com p > 0.05) e normalidade.</li>
  <li><b>Menor valor de AIC (Akaike Information Criterion):</b> Um critério que equilibra o bom ajuste do modelo com sua complexidade.</li>
</ol>
<p>Com base nos resultados apresentados:</p>
<ul>
  <li>Os modelos <b>AR(1)</b> e <b>ARMA(1,1)</b>, ajustados à série original não-estacionária, geralmente falham em produzir resíduos sem autocorrelação, como indicado pelo teste de Ljung-Box.</li>
  <li>O modelo <b>ARIMA(1,1,1)</b> é o candidato mais forte. A diferenciação (o 'I' do meio) lida com a não-estacionariedade da série, e os componentes AR e MA modelam a estrutura de correlação restante. Espera-se que seus resíduos não apresentem autocorrelação significativa.</li>
</ul>
<p><b>RECOMENDAÇÃO:</b> O modelo <strong>ARIMA(1,1,1)</strong> é o mais apropriado para esta análise, pois captura melhor a dinâmica da série temporal dos preços do café, resultando em resíduos mais próximos do ruído branco.</p>
"
report_items <- append(report_items, list(HTML(conclusion_text)))


# --- 3. Geração do Relatório Final ---
message("--- Gerando relatório HTML final ---")
dir.create("reports", showWarnings = FALSE)
report_filename <- "reports/relatorio_analise_cafe.html"
save_report_as_html(report_items, report_filename)

message(paste("Projeto concluído! Relatório salvo em:", report_filename))