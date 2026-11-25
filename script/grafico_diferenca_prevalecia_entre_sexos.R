#-------------------------------------------------------------------------------
# DIFERENÇA DAS PREVALÊNCIAS ANTROPOMÉTRICAS POR SEXO
# TCC Ciências Sociais
# Felipe Casula, 2026
#-------------------------------------------------------------------------------

# Este script parte da tabela consolidada de prevalências por sexo e grupo
# etário. A diferença é sempre calculada como:
#
#   prevalência entre mulheres - prevalência entre homens
#
# Portanto:
#   valor positivo = prevalência maior entre mulheres;
#   valor negativo = prevalência maior entre homens.

#--- Bibliotecas ---------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  janitor,
  scales
)


#--- Arquivo de entrada e diretório de saída ----------------------------------

# O caminho é relativo à raiz do Projeto R. Não é necessário declarar o
# diretório-base no script.
arquivo_csv <- file.path(
  "dados",
  "output",
  "prevalencias",
  "prevalencias_sexo_idade_2003_2019.csv"
)

# Este segundo caminho permite executar o script se o CSV tiver sido salvo
# temporariamente na raiz do projeto.
if (!file.exists(arquivo_csv)) {
  arquivo_csv <- "prevalencias_sexo_idade_2003_2019.csv"
}

if (!file.exists(arquivo_csv)) {
  stop(
    "O CSV original das prevalências não foi encontrado. Execute primeiro ",
    "o script de estimação ou salve o arquivo ",
    "'prevalencias_sexo_idade_2003_2019.csv' em ",
    "'dados/output/prevalencias/' ou na raiz do projeto."
  )
}

diretorio_graficos <- file.path(
  "output",
  "graficos_prevalencia"
)

dir.create(
  diretorio_graficos,
  recursive = TRUE,
  showWarnings = FALSE
)


source("script/plotagens_diferencas.R")

message("Ler e verificar as prevalências por sexo e grupo etário")
diferencas_sexo <- calcular_diferencas_prevalencia(arquivo_csv)
print(diferencas_sexo, n = Inf)

grafico_diferenca_sexo <- plotar_diferencas_prevalencia(diferencas_sexo)

if (interactive()) print(grafico_diferenca_sexo)


#--- 5. Salvar em formato vetorial e em alta resolução ------------------------

message("[5/5] Salvar o gráfico em PDF e PNG")

# Título, nota e fonte são inseridos separadamente no TCC.
figura_tcc <- grafico_diferenca_sexo +
  labs(title = NULL, subtitle = NULL, caption = NULL)

ggsave(
  filename = file.path(
    diretorio_graficos,
    "diferenca_prevalencias_antropometricas_por_sexo.pdf"
  ),
  plot = figura_tcc,
  width = 16,
  height = 12,
  units = "in",
  device = grDevices::cairo_pdf,
  bg = "white"
)

ggsave(
  filename = file.path(
    diretorio_graficos,
    "diferenca_prevalencias_antropometricas_por_sexo.png"
  ),
  plot = figura_tcc,
  width = 16,
  height = 12,
  units = "in",
  dpi = 400,
  bg = "white"
)

message(
  "Gráfico concluído. Arquivos salvos em: ",
  normalizePath(
    diretorio_graficos,
    winslash = "/",
    mustWork = FALSE
  )
)
