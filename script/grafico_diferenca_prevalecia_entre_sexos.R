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


#--- 1. Ler o CSV original das prevalências -----------------------------------

message("[1/5] Ler os resultados de prevalência por sexo e grupo etário")

# O arquivo é produzido diretamente pelo script de estimação das prevalências.
# Cada linha representa uma pesquisa, sexo, grupo etário e indicador.
prevalencias <- read_csv(
  file = arquivo_csv,
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  progress = FALSE
) |>
  clean_names() |>
  transmute(
    pesquisa = as.character(pesquisa),
    ano = as.integer(ano),
    indicador = as.character(indicador),
    sexo = as.character(sexo),
    faixa_idade = as.character(faixa_idade),
    estimativa_pct = as.numeric(estimativa_pct)
  ) |>
  filter(
    !is.na(pesquisa),
    !is.na(indicador),
    sexo %in% c("F", "M"),
    !is.na(faixa_idade),
    is.finite(estimativa_pct)
  )


#--- 2. Validar as combinações necessárias -----------------------------------

message("[2/5] Verificar se cada célula contém uma estimativa por sexo")

duplicidades <- prevalencias |>
  count(
    pesquisa,
    ano,
    indicador,
    faixa_idade,
    sexo,
    name = "numero_estimativas"
  ) |>
  filter(numero_estimativas != 1L)

if (nrow(duplicidades) > 0L) {
  stop(
    "Há combinações de pesquisa, indicador, faixa etária e sexo com ",
    "mais de uma estimativa. Revise a tabela de entrada."
  )
}

celulas_incompletas <- prevalencias |>
  count(
    pesquisa,
    ano,
    indicador,
    faixa_idade,
    name = "numero_sexos"
  ) |>
  filter(numero_sexos != 2L)

if (nrow(celulas_incompletas) > 0L) {
  stop(
    "Há células sem as duas estimativas necessárias: mulheres e homens."
  )
}


#--- 3. Colocar o sexo nas colunas e calcular a diferença ---------------------

message("[3/5] Calcular mulheres menos homens, em pontos percentuais")

faixas_idade <- c(
  "20-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80+"
)

ordem_pesquisas <- c(
  "POF 2002-2003",
  "POF 2008-2009",
  "PNS 2013",
  "PNS 2019"
)

ordem_indicadores <- c(
  "Sobrepeso (25–<30)",
  "Excesso de peso (≥25)",
  "Obesidade (≥30)"
)

diferencas_sexo <- prevalencias |>
  select(
    pesquisa,
    ano,
    indicador,
    faixa_idade,
    sexo,
    estimativa_pct
  ) |>
  pivot_wider(
    names_from = sexo,
    values_from = estimativa_pct
  ) |>
  rename(
    prevalencia_mulheres_pct = F,
    prevalencia_homens_pct = M
  ) |>
  mutate(
    diferenca_pp = prevalencia_mulheres_pct - prevalencia_homens_pct,
    
    direcao_diferenca = case_when(
      diferenca_pp > 0 ~ "Maior entre mulheres",
      diferenca_pp < 0 ~ "Maior entre homens",
      TRUE ~ "Sem diferença"
    ),
    
    rotulo_diferenca = number(
      diferenca_pp,
      accuracy = 0.1,
      decimal.mark = ","
    ),
    
    pesquisa = factor(
      pesquisa,
      levels = ordem_pesquisas,
      ordered = TRUE
    ),
    
    indicador = factor(
      indicador,
      levels = ordem_indicadores,
      ordered = TRUE
    ),
    
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    ),
    
    direcao_diferenca = factor(
      direcao_diferenca,
      levels = c(
        "Maior entre mulheres",
        "Maior entre homens",
        "Sem diferença"
      )
    )
  ) |>
  arrange(
    indicador,
    pesquisa,
    faixa_idade
  )

if (
  nrow(diferencas_sexo) != 84L ||
  any(!is.finite(diferencas_sexo$diferenca_pp))
) {
  stop(
    "O cálculo não produziu as 84 diferenças esperadas ",
    "(3 indicadores × 4 pesquisas × 7 grupos etários)."
  )
}

print(diferencas_sexo, n = Inf)


#--- 4. Construir o gráfico ---------------------------------------------------

message("[4/5] Construir o gráfico comparativo")

# Um limite simétrico mantém a mesma escala acima e abaixo de zero. Isso evita
# ampliar visualmente as diferenças em apenas uma das direções.
limite_eixo <- ceiling(
  max(abs(diferencas_sexo$diferenca_pp), na.rm = TRUE) / 5
) * 5 + 3

diferencas_sexo <- diferencas_sexo |>
  mutate(
    posicao_rotulo = diferenca_pp + if_else(
      diferenca_pp >= 0,
      0.65,
      -0.65
    )
  )

cores_diferenca <- c(
  "Maior entre mulheres" = "#B2182B",
  "Maior entre homens" = "#2166AC",
  "Sem diferença" = "#666666"
)

texto_fonte <- str_wrap(
  paste(
    "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
    "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019."
  ),
  width = 150
)

texto_nota <- str_wrap(
  paste(
    "Nota: valores positivos, representados por barras vermelhas, indicam",
    "maior prevalência entre mulheres; valores negativos, em azul, indicam",
    "maior prevalência entre homens. As diferenças são descritivas e foram",
    "calculadas a partir das estimativas não padronizadas de cada grupo",
    "etário; não representam testes de hipótese."
  ),
  width = 150
)

texto_definicoes <- str_wrap(
  paste(
    "Sobrepeso = 25 ≤ IMC < 30 kg/m²; excesso de peso = IMC ≥ 25 kg/m²,",
    "incluindo obesidade; obesidade = IMC ≥ 30 kg/m². Elaboração própria."
  ),
  width = 150
)

legenda_figura <- paste(
  texto_fonte,
  texto_nota,
  texto_definicoes,
  sep = "\n"
)

grafico_diferenca_sexo <- ggplot(
  diferencas_sexo,
  aes(
    x = faixa_idade,
    y = diferenca_pp,
    fill = direcao_diferenca
  )
) +
  geom_hline(
    yintercept = 0,
    color = "#333333",
    linewidth = 0.45
  ) +
  geom_col(
    width = 0.72,
    color = "white",
    linewidth = 0.20
  ) +
  geom_text(
    aes(
      y = posicao_rotulo,
      label = rotulo_diferenca
    ),
    size = 2.35,
    family = "serif",
    color = "#222222"
  ) +
  facet_grid(
    rows = vars(indicador),
    cols = vars(pesquisa)
  ) +
  scale_fill_manual(
    values = cores_diferenca,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(-limite_eixo, limite_eixo),
    breaks = pretty_breaks(n = 7),
    labels = label_number(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Diferença das prevalências antropométricas por sexo",
    subtitle = paste(
      "Mulheres menos homens, em pontos percentuais,",
      "por grupo etário e pesquisa"
    ),
    x = "Grupo etário (anos)",
    y = "Diferença da prevalência (pontos percentuais)",
    fill = NULL,
    caption = legenda_figura
  ) +
  theme_minimal(
    base_size = 10.5,
    base_family = "serif"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      color = "#222222",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "#444444",
      margin = margin(b = 10),
      hjust = 0
    ),
    plot.caption = element_text(
      size = 8.2,
      color = "#4D4D4D",
      hjust = 0,
      lineheight = 1.05,
      margin = margin(t = 12)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    strip.text = element_text(
      face = "bold",
      size = 9,
      color = "#222222"
    ),
    strip.background = element_rect(
      fill = "#F2F2F2",
      color = "#D0D0D0",
      linewidth = 0.35
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "#E1E1E1",
      linewidth = 0.30
    ),
    panel.spacing = grid::unit(0.8, "lines"),
    axis.title = element_text(
      face = "bold",
      color = "#222222"
    ),
    axis.text = element_text(
      color = "#333333"
    ),
    axis.title.y = element_text(size = 15, margin = margin(r = 12)),
    axis.text.y = element_text(size = 14, color = "#222222"),
    strip.text.y = element_text(size = 13, margin = margin(6, 6, 6, 6)),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.margin = margin(12, 16, 12, 12)
  )

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
