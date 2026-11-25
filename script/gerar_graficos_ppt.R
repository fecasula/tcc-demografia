# Execute na raiz do projeto. As entradas são as tabelas de resultados em CSV.
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
})

source("script/figuras_abnt.R")
source("script/plotagens_diferencas.R")
source("script/plotagens_indicadores.R")
source("script/plotagens_prevalencia.R")

ler_resultados_ppt <- function(pasta, arquivos) {
  lapply(arquivos, function(arquivo) {
    caminho <- file.path(pasta, arquivo)
    tabela <- read_csv(caminho, show_col_types = FALSE)
    stop_for_problems(tabela)
    tabela
  })
}

tema_figura_ppt <- function() {
  theme(
    text = element_text(family = "sans", colour = "#17212B"),
    plot.title.position = "plot",
    plot.title = element_text(size = 23, face = "bold", margin = margin(b = 6)),
    plot.subtitle = element_text(size = 13, colour = "#334155", margin = margin(b = 12)),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13, colour = "#26323D"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 13),
    legend.key.width = grid::unit(1, "cm"),
    panel.spacing = grid::unit(0.55, "cm"),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 10, hjust = 0, colour = "#334155"),
    plot.margin = margin(14, 22, 10, 16)
  )
}

salvar_conjunto_ppt <- function(graficos, grupo) {
  pasta <- file.path("graficos_ppt", grupo)
  dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

  for (nome in names(graficos)) {
    grafico <- graficos[[nome]]
    rotulos <- grafico$labels
    fonte <- strsplit(rotulos$caption, "\n", fixed = TRUE)[[1]][1]
    subtitulo <- gsub(">=", "≥", rotulos$subtitle, fixed = TRUE)
    grafico <- grafico +
      labs(
        title = paste(strwrap(rotulos$title, width = 70), collapse = "\n"),
        subtitle = paste(strwrap(subtitulo, width = 105), collapse = "\n"),
        caption = paste(strwrap(fonte, width = 125), collapse = "\n")
      )
    ggsave(
      file.path(pasta, paste0(nome, ".png")), grafico,
      width = 12, height = 6.75, units = "in", dpi = 200, bg = "white"
    )
  }
}

preparar_prevalencia_ppt <- function(dados) {
  mutate(
    dados,
    pesquisa = factor(pesquisa, levels = names(cores_pesquisas_abnt)),
    indicador = factor(indicador, levels = c(
      "Sobrepeso (25–<30)", "Excesso de peso (≥25)", "Obesidade (≥30)"
    )),
    sexo_rotulo = factor(sexo_rotulo, levels = c("Homens", "Mulheres"))
  )
}

pasta_prevalencias <- file.path("dados", "output", "prevalencias")
prevalencias <- ler_resultados_ppt(pasta_prevalencias, c(
  observadas = "prevalencias_2003_2019.csv",
  padronizadas = "prevalencias_padronizadas_etaria_2019.csv"
))
observadas <- preparar_prevalencia_ppt(prevalencias$observadas)
padronizadas <- preparar_prevalencia_ppt(prevalencias$padronizadas)
diferencas <- calcular_diferencas_prevalencia(
  file.path(pasta_prevalencias, "prevalencias_sexo_idade_2003_2019.csv")
)

graficos_prevalencia <- list(
  prevalencia_por_idade_sexo = plotar_prevalencia_idade(
    filter(observadas, tipo_resultado == "Por sexo e grupo etário")
  ) +
    facet_grid(rows = vars(sexo_rotulo), cols = vars(indicador)) +
    guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
    tema_figura_ppt() +
    theme(axis.text.x = element_text(size = 11)),
  prevalencia_agregada_por_sexo = plotar_prevalencia_agregada(
    filter(observadas, tipo_resultado == "Agregada por sexo")
  ) +
    facet_wrap(vars(indicador), nrow = 1, scales = "free_y") +
    tema_figura_ppt(),
  prevalencia_padronizada_estrutura_2019 = plotar_prevalencia_agregada(
    padronizadas, padronizada = TRUE
  ) +
    facet_wrap(vars(indicador), nrow = 1, scales = "free_y") +
    labs(subtitle = "População de 20 anos ou mais · padrão etário de 2019 · IC 95%") +
    tema_figura_ppt(),
  diferenca_prevalencias_antropometricas_por_sexo = plotar_diferencas_prevalencia(
    diferencas, tamanho_rotulos = 3.1, distancia_rotulos = 1.35,
    quebras = pretty_breaks(n = 4), manter_legenda_completa = FALSE
  ) +
    facet_grid(
      rows = vars(indicador), cols = vars(pesquisa),
      labeller = labeller(indicador = c(
        "Sobrepeso (25–<30)" = "Sobrepeso",
        "Excesso de peso (≥25)" = "Excesso de peso",
        "Obesidade (≥30)" = "Obesidade"
      ))
    ) +
    labs(
      y = "Diferença (p.p.)",
      subtitle = "Mulheres menos homens por idade e pesquisa · diferenças descritivas"
    ) +
    tema_figura_ppt() +
    theme(
      axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
      strip.text.y.right = element_text(size = 10.5, margin = margin(4))
    )
)

pasta_resultados <- file.path("dados", "resultados")
comparacao <- ler_resultados_ppt(file.path(pasta_resultados, "comparacao_4_ondas"), c(
  paf_idade = "01_paf_por_sexo_e_idade_4_ondas.csv",
  paf_sintetica = "02_paf_sintetica_por_sexo_4_ondas.csv",
  mortes_evitaveis_idade = "03_mortes_evitaveis_por_sexo_e_idade_4_ondas.csv",
  mortes_evitaveis_total = "04_mortes_evitaveis_por_sexo_4_ondas.csv",
  pgev20 = "05_pgev20_por_sexo_4_ondas.csv"
))
comparacao <- lapply(comparacao, function(dados) {
  mutate(
    dados,
    pesquisa = factor(pesquisa, levels = names(cores_pesquisas_abnt)),
    sexo_rotulo = factor(sexo_rotulo, levels = c("Homens", "Mulheres"))
  )
})
graficos_comparacao <- lapply(montar_graficos_comparacao(comparacao), function(grafico) {
  grafico + tema_figura_ppt()
})
graficos_comparacao$"01_paf_por_idade_e_sexo_4_ondas" <-
  graficos_comparacao$"01_paf_por_idade_e_sexo_4_ondas" +
  facet_wrap(vars(sexo_rotulo), nrow = 1)
graficos_comparacao$"03_mortes_por_idade_e_sexo_4_ondas" <-
  graficos_comparacao$"03_mortes_por_idade_e_sexo_4_ondas" +
  facet_wrap(vars(sexo_rotulo), nrow = 1, scales = "free_y")

arquivos_onda <- c(
  paf_idade = "05_paf_por_sexo_e_idade.csv",
  paf_sintetica = "06_paf_sintetica_por_sexo.csv",
  mortes_evitaveis_idade = "07_mortes_evitaveis_por_sexo_e_idade.csv",
  mortes_evitaveis_total = "08_mortes_evitaveis_por_sexo.csv",
  pgev20 = "09_pgev20_por_sexo.csv"
)
ondas <- c("pof_2002_2003", "pof_2008_2009", "pns_2013", "pns_2019")
graficos_ondas <- setNames(lapply(ondas, function(onda) {
  tabela <- ler_resultados_ppt(file.path(pasta_resultados, onda), arquivos_onda)
  graficos <- montar_graficos_onda(
    tabela, tabela$paf_idade$pesquisa[1], tabela$paf_idade$ano_mortalidade[1],
    largura_barras = 0.38
  )
  lapply(graficos, function(grafico) grafico + tema_figura_ppt())
}), ondas)

salvar_conjunto_ppt(graficos_prevalencia, "prevalencia")
salvar_conjunto_ppt(graficos_comparacao, "comparacao_4_ondas")
for (onda in ondas) {
  salvar_conjunto_ppt(graficos_ondas[[onda]], onda)
}

message("Figuras para slides salvas em ", normalizePath("graficos_ppt"))
