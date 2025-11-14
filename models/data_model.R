# models/data_model.R
# -----------------------------------------------------------------------------
# Este script contém a função para buscar, processar e salvar os dados
# de futuros de café (ICF) da B3.
# -----------------------------------------------------------------------------

fetch_coffee_data <- function(start_date = Sys.Date() - 365*5, end_date = Sys.Date()) {
  
  # Define o caminho do arquivo para cache
  cache_file <- "data/coffee_data.csv"
  
  # Verifica se o arquivo de cache já existe
  if (file.exists(cache_file)) {
    
    message("Os dados de café já foram baixados.")
    coffee_data <- readr::read_csv(cache_file, col_types = readr::cols())
    message(paste("Dados de", min(coffee_data$date), "a", max(coffee_data$date)))
    
    resposta <- readline(prompt="Deseja baixar os dados novamente? (s/n): ")
    if (tolower(resposta) == "s") {
      message("Baixando dados novamente...")
    } else {
      message("Pulando para a próxima etapa...")
      return(coffee_data)
    }
    
  } else {
    
    message("Buscando dados de futuros de café (ICF) na B3...")
    
    tryCatch({
      # Garante que os dados locais da B3 estão atualizados
      message("Atualizando o banco de dados local de futuros da B3...")
      rb3::fetch_marketdata(
        "b3-futures-settlement-prices",
        refdate = seq(from = start_date, to = end_date, by = "day")
      )

      # futures_get() retorna todos os contratos.
      # Filtramos pelo ticker (commodity) e datas desejadas.
      icf_curve <- rb3::futures_get() %>%
        dplyr::filter(
          commodity == "ICF",
          refdate >= start_date,
          refdate <= end_date
        ) %>%
        dplyr::collect() # Coleta os dados para um data frame local

      if (is.null(icf_curve) || nrow(icf_curve) == 0) {
        stop("A busca de dados com rb3 não retornou resultados para ICF.")
      }
      
      # Adiciona a data de vencimento e calcula os dias até o vencimento
      icf_curve_processed <- icf_curve %>%
        dplyr::mutate(maturity_date = rb3::maturitycode2date(maturity_code)) %>%
        dplyr::mutate(days_to_settlement = as.integer(maturity_date - refdate))

      # Filtra para obter o primeiro vencimento de cada dia
      coffee_data <- icf_curve_processed %>%
        dplyr::filter(days_to_settlement > 0) %>%
        dplyr::group_by(refdate) %>%
        dplyr::filter(days_to_settlement == min(days_to_settlement)) %>%
        dplyr::ungroup() %>%
        dplyr::select(date = refdate, price) %>%
        dplyr::arrange(date)
      
      # Salva os dados em cache
      readr::write_csv(coffee_data, cache_file)
      message(paste("Dados salvos em", cache_file))
      
      return(coffee_data)
      
    }, error = function(e) {
      message("Ocorreu um erro ao buscar os dados da B3.")
      message("Erro original: ", e$message)
      
      # Tenta ler do cache como fallback
      if (file.exists(cache_file)) {
        message("Tentando ler dados do cache como fallback.")
        coffee_data <- readr::read_csv(cache_file, col_types = readr::cols())
        return(coffee_data)
      } else {
        stop("Falha na busca de dados e nenhum arquivo de cache disponível.")
      }
    })
  }
}
