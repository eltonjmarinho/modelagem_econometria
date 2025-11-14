# Modelagem Econométrica com R

## Descrição

Este projeto realiza uma análise econométrica utilizando dados de café. O objetivo é modelar e visualizar os dados para extrair insights.

## Estrutura do Projeto

- `main.R`: Script principal que orquestra a execução da análise.
- `data/`: Contém os conjuntos de dados brutos.
  - `coffee_data.csv`: Dados sobre café.
- `models/`: Contém os scripts para modelagem dos dados.
  - `data_model.R`: Scripts para preparação e limpeza dos dados.
  - `analysis_model.R`: Scripts para a análise econométrica.
- `views/`: Contém os scripts para visualização dos dados.
  - `plot_view.R`: Scripts para gerar gráficos e visualizações.
- `requirements.txt`: Lista de pacotes R necessários para o projeto.

## Como Executar

1.  Instale as dependências listadas no arquivo `requirements.txt`. Você pode usar o seguinte comando no R para instalar um pacote:
    ```R
    install.packages("nome_do_pacote")
    ```
2.  Execute o script principal para iniciar a análise:
    ```R
    source("main.R")
    ```

## Dependências

Veja o arquivo `requirements.txt` para a lista de pacotes R necessários.
