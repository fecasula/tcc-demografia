#-------------------------------------------------------------------------------
# PADRONIZAÇÃO ETÁRIA DAS PREVALÊNCIAS ANTROPOMÉTRICAS
# Projeções da População do IBGE, revisão 2024
# TCC Ciências Sociais
# Felipe Casula, 2026
#-------------------------------------------------------------------------------

# A padronização será calculada dentro dos objetos survey, e não a partir dos
# erros-padrão já exportados para CSV. Por isso, este script precisa dos quatro
# desenhos declarados em estimacao_prevalencia.R.
objetos_survey_necessarios <- c(
  "des_pof02",
  "des_pof08",
  "des_pns13",
  "des_pns19"
)

objetos_survey_disponiveis <- vapply(
  objetos_survey_necessarios,
  function(nome_objeto) {
    if (!exists(nome_objeto, inherits = TRUE)) {
      return(FALSE)
    }

    desenho <- get(nome_objeto, inherits = TRUE)

    all(
      c(
        "sexo", "faixa_idade", "sobrepeso_25a29",
        "excesso_peso", "obesidade"
      ) %in%
        names(desenho$variables)
    )
  },
  logical(1)
)

if (!all(objetos_survey_disponiveis)) {
  message(
    "Os objetos survey ainda não estão na sessão. Executando primeiro ",
    "script/estimacao_prevalencia.R..."
  )

  source("script/estimacao_prevalencia.R")
}

#--- Bibliotecas ---------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  survey,
  tidyverse,
  janitor,
  readxl
)

if (!all(vapply(
  c("des_pof02", "des_pof08", "des_pns13", "des_pns19"),
  function(nome_objeto) {
    exists(nome_objeto, inherits = TRUE) &&
      all(
        c(
          "sexo", "faixa_idade", "sobrepeso_25a29",
          "excesso_peso", "obesidade"
        ) %in%
          names(get(nome_objeto, inherits = TRUE)$variables)
      )
  },
  logical(1)
))) {
  stop(
    "Não foi possível criar os quatro objetos survey necessários à ",
    "padronização. Revise a execução de estimacao_prevalencia.R."
  )
}

#--- Arquivos utilizados -------------------------------------------------------

# Este arquivo é produzido pelo script de estimação das prevalências.
# Ele precisa conter uma linha para cada combinação de:
# pesquisa x sexo x indicador x grupo etário.
arquivo_prevalencias <- paste0(
  "dados/output/prevalencias/",
  "prevalencias_sexo_idade_2003_2019.csv"
)

# Projeções da População do IBGE, revisão 2024.
# Tabela 1: população por sexo e idade simples, 2000 a 2070.
# No produto, 2000-2022 são estimativas e 2023-2070 são projeções. As
# populações têm como data de referência 1º de julho de cada ano.
url_projecoes_populacao <- paste0(
  "https://ftp.ibge.gov.br/Projecao_da_Populacao/",
  "Projecao_da_Populacao_2024/",
  "projecoes_2024_tab1_idade_simples.xlsx"
)

arquivo_projecoes_populacao <- paste0(
  "dados/raw/populacao/",
  "projecoes_2024_tab1_idade_simples.xlsx"
)

diretorio_tabelas <- "dados/output/prevalencias"
diretorio_figuras <- "output/graficos_prevalencia"

if (!file.exists(arquivo_prevalencias)) {
  stop(
    "O arquivo com as prevalências por idade não foi encontrado: ",
    arquivo_prevalencias,
    "\nExecute primeiro o script estimacao_prevalencia.R."
  )
}

if (!file.exists(arquivo_projecoes_populacao)) {
  dir.create(
    dirname(arquivo_projecoes_populacao),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  message(
    "Baixando as Projeções da População do IBGE, revisão 2024..."
  )
  
  download.file(
    url = url_projecoes_populacao,
    destfile = arquivo_projecoes_populacao,
    mode = "wb",
    quiet = FALSE
  )
}


#--- Definições da análise -----------------------------------------------------

# A POF 2017-2018 não integra esta análise.
pesquisas_analisadas <- c(
  "POF 2002-2003",
  "POF 2008-2009",
  "PNS 2013",
  "PNS 2019"
)

anos_analisados <- c(
  2003L,
  2009L,
  2013L,
  2019L
)

# Os mesmos sete grupos devem ter sido utilizados no script de estimação.
faixas_idade <- c(
  "20-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80+"
)

indicadores_analisados <- c(
  "Sobrepeso (25–<30)",
  "Excesso de peso (≥25)",
  "Obesidade (≥30)"
)


#--- Resultados estimados por grupo etário ------------------------------------

prevalencias_idade <- read_csv(
  arquivo_prevalencias,
  show_col_types = FALSE
) |>
  filter(
    pesquisa %in% pesquisas_analisadas
  ) |>
  transmute(
    pesquisa = as.character(pesquisa),
    ano = as.integer(ano),
    sexo = as.character(sexo),
    faixa_idade = as.character(faixa_idade),
    indicador = as.character(indicador),
    estimativa = as.numeric(estimativa),
    erro_padrao = as.numeric(erro_padrao),
    ic_inferior = as.numeric(ic_inferior),
    ic_superior = as.numeric(ic_superior)
  )

# A padronização só pode ser executada depois que o script principal for
# atualizado para os grupos decenais. Esta verificação evita misturar grupos
# quinquenais e decenais inadvertidamente.
faixas_encontradas <- sort(unique(prevalencias_idade$faixa_idade))

if (!setequal(faixas_encontradas, faixas_idade)) {
  stop(
    "Os grupos etários do arquivo de prevalências não são os esperados.",
    "\nEsperados: ", paste(faixas_idade, collapse = ", "),
    "\nEncontrados: ", paste(faixas_encontradas, collapse = ", "),
    "\nAtualize e execute novamente estimacao_prevalencia.R."
  )
}

pesquisas_ausentes <- setdiff(
  pesquisas_analisadas,
  unique(prevalencias_idade$pesquisa)
)

if (length(pesquisas_ausentes) > 0) {
  stop(
    "Pesquisas ausentes no arquivo de prevalências: ",
    paste(pesquisas_ausentes, collapse = ", ")
  )
}

if (!setequal(unique(prevalencias_idade$ano), anos_analisados)) {
  stop(
    "Os anos do arquivo de prevalências não correspondem a 2003, 2009, ",
    "2013 e 2019."
  )
}

if (!setequal(unique(prevalencias_idade$indicador), indicadores_analisados)) {
  stop(
    "O arquivo de prevalências não contém simultaneamente as três ",
    "classificações esperadas: ",
    paste(indicadores_analisados, collapse = ", "),
    ". Execute novamente estimacao_prevalencia.R."
  )
}

# Cada célula analítica deve aparecer somente uma vez.
celulas_duplicadas <- prevalencias_idade |>
  count(
    pesquisa,
    sexo,
    indicador,
    faixa_idade,
    name = "numero_linhas"
  ) |>
  filter(numero_linhas != 1)

if (nrow(celulas_duplicadas) > 0) {
  stop(
    "Existem células duplicadas no arquivo de prevalências."
  )
}

celulas_incompletas <- prevalencias_idade |>
  count(
    pesquisa,
    sexo,
    indicador,
    name = "numero_faixas"
  ) |>
  filter(numero_faixas != length(faixas_idade))

if (nrow(celulas_incompletas) > 0) {
  stop(
    "Existem combinações de pesquisa, sexo e indicador sem as sete faixas ",
    "etárias esperadas."
  )
}

if (
  any(!is.finite(prevalencias_idade$estimativa)) ||
  any(!is.finite(prevalencias_idade$erro_padrao))
) {
  stop(
    "Existem estimativas ou erros-padrão ausentes/não finitos."
  )
}


#--- Projeções populacionais do IBGE ------------------------------------------

# O arquivo oficial possui cinco linhas de título. A sexta linha contém os
# nomes das variáveis: idade, sexo, código, sigla, local e anos de 2000 a 2070.
projecoes_populacao <- read_excel(
  path = arquivo_projecoes_populacao,
  sheet = "1) POP_IDADE SIMPLES",
  skip = 5
) |>
  clean_names()

# Após clean_names(), as colunas anuais são nomeadas x2000, x2001, ..., x2070.
colunas_anos <- names(projecoes_populacao)[
  str_detect(names(projecoes_populacao), "^x?[0-9]{4}$")
]

if (length(colunas_anos) == 0) {
  stop(
    "As colunas anuais não foram encontradas no arquivo de projeções do IBGE."
  )
}

populacao_idade_simples <- projecoes_populacao |>
  filter(
    sigla == "BR",
    sexo %in% c("Ambos", "Homens", "Mulheres")
  ) |>
  select(
    idade,
    sexo,
    all_of(colunas_anos)
  ) |>
  pivot_longer(
    cols = all_of(colunas_anos),
    names_to = "ano",
    values_to = "populacao"
  ) |>
  mutate(
    # clean_names() normalmente transforma 2000 em x2000. A extração dos
    # quatro algarismos funciona tanto para x2000 quanto para 2000.
    ano = as.integer(str_extract(ano, "[0-9]{4}")),
    idade = as.integer(idade),
    populacao = as.numeric(populacao),
    
    # A idade 90 reúne a população de 90 anos ou mais no arquivo oficial.
    faixa_idade = case_when(
      idade >= 20 & idade < 30 ~ "20-29",
      idade >= 30 & idade < 40 ~ "30-39",
      idade >= 40 & idade < 50 ~ "40-49",
      idade >= 50 & idade < 60 ~ "50-59",
      idade >= 60 & idade < 70 ~ "60-69",
      idade >= 70 & idade < 80 ~ "70-79",
      idade >= 80 ~ "80+",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    idade >= 20,
    !is.na(faixa_idade),
    is.finite(populacao),
    populacao >= 0
  )


#--- População por sexo nos anos das pesquisas -------------------------------

# Estas tabelas representam as estruturas efetivas de cada ano e permitem
# calcular uma prevalência agregada que respeita o volume populacional estimado
# de homens e mulheres em 2003, 2009, 2013 e 2019.
populacao_anos_pesquisas <- populacao_idade_simples |>
  filter(
    ano %in% anos_analisados,
    sexo %in% c("Homens", "Mulheres")
  ) |>
  mutate(
    sexo = recode(
      sexo,
      Homens = "M",
      Mulheres = "F"
    )
  ) |>
  group_by(
    ano,
    sexo,
    faixa_idade
  ) |>
  summarise(
    populacao = sum(populacao),
    .groups = "drop"
  ) |>
  group_by(
    ano,
    sexo
  ) |>
  mutate(
    peso_etario_ano = populacao / sum(populacao)
  ) |>
  ungroup() |>
  mutate(
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    ),
    fonte = paste(
      "IBGE, Projeções da População, revisão 2024,",
      "população por sexo e idade simples"
    ),
    url_fonte = url_projecoes_populacao
  ) |>
  arrange(
    ano,
    sexo,
    faixa_idade
  )

if (
  nrow(populacao_anos_pesquisas) !=
  length(unique(prevalencias_idade$ano)) * 2 * length(faixas_idade)
) {
  stop(
    "A projeção do IBGE não produziu todas as combinações esperadas de ",
    "ano, sexo e faixa etária."
  )
}

controle_pesos_anuais <- populacao_anos_pesquisas |>
  group_by(
    ano,
    sexo
  ) |>
  summarise(
    soma_pesos = sum(peso_etario_ano),
    .groups = "drop"
  )

if (any(abs(controle_pesos_anuais$soma_pesos - 1) > 1e-10)) {
  stop("Há pesos populacionais anuais que não somam 1.")
}


#--- Estrutura etária comum de 2019 -------------------------------------------

# Para a padronização histórica, utilizamos a população brasileira de ambos os
# sexos em 2019 como padrão único. O mesmo vetor de pesos é aplicado a homens e
# mulheres em todas as pesquisas. Assim, diferenças temporais ou entre os sexos
# não são produzidas por estruturas-padrão distintas.
pesos_etarios_2019 <- populacao_idade_simples |>
  filter(
    ano == 2019,
    sexo == "Ambos"
  ) |>
  group_by(faixa_idade) |>
  summarise(
    populacao_padrao = sum(populacao),
    .groups = "drop"
  ) |>
  mutate(
    peso_etario_2019 = populacao_padrao / sum(populacao_padrao),
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    ),
    fonte = paste(
      "IBGE, Projeções da População, revisão 2024,",
      "população por sexo e idade simples"
    ),
    url_fonte = url_projecoes_populacao
  ) |>
  arrange(faixa_idade)

if (nrow(pesos_etarios_2019) != length(faixas_idade)) {
  stop(
    "Não foi possível construir os sete grupos etários da população de 2019."
  )
}

if (abs(sum(pesos_etarios_2019$peso_etario_2019) - 1) > 1e-10) {
  stop("Os pesos etários da população de 2019 não somam 1.")
}

print(populacao_anos_pesquisas, n = Inf)
print(pesos_etarios_2019, n = Inf)


#--- Agregação com a estrutura populacional de cada ano -----------------------

# Este primeiro resultado responde à pergunta descritiva de cada pesquisa:
# qual é a prevalência agregada quando cada faixa recebe o peso correspondente
# à população masculina ou feminina estimada no respectivo ano?
contribuicoes_estrutura_ano <- prevalencias_idade |>
  mutate(
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    )
  ) |>
  left_join(
    populacao_anos_pesquisas,
    by = c(
      "ano",
      "sexo",
      "faixa_idade"
    )
  ) |>
  mutate(
    contribuicao_estimativa = peso_etario_ano * estimativa,
    contribuicao_variancia = (peso_etario_ano * erro_padrao)^2
  )

if (any(is.na(contribuicoes_estrutura_ano$peso_etario_ano))) {
  stop(
    "Há resultados por idade sem correspondência na população projetada ",
    "para o respectivo ano e sexo."
  )
}

# O CSV de prevalências não contém as covariâncias entre os domínios etários.
# Por esse motivo, o erro-padrão agregado é aproximado e trata os domínios como
# independentes. A estimativa pontual ponderada não depende dessa aproximação.
prevalencias_agregadas_estrutura_ano <- contribuicoes_estrutura_ano |>
  group_by(
    pesquisa,
    ano,
    sexo,
    indicador
  ) |>
  summarise(
    estimativa = sum(contribuicao_estimativa),
    variancia_aproximada = sum(contribuicao_variancia),
    erro_padrao = sqrt(variancia_aproximada),
    .groups = "drop"
  ) |>
  mutate(
    estimativa_logit = pmin(
      pmax(estimativa, 1e-8),
      1 - 1e-8
    ),
    erro_padrao_logit = erro_padrao /
      (estimativa_logit * (1 - estimativa_logit)),
    ic_inferior = plogis(
      qlogis(estimativa_logit) -
        qnorm(0.975) * erro_padrao_logit
    ),
    ic_superior = plogis(
      qlogis(estimativa_logit) +
        qnorm(0.975) * erro_padrao_logit
    ),
    coeficiente_variacao_pct = if_else(
      estimativa > 0,
      100 * erro_padrao / estimativa,
      NA_real_
    ),
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    sexo_rotulo = recode(
      sexo,
      M = "Homens",
      F = "Mulheres"
    ),
    ponderacao = paste(
      "Estrutura populacional projetada para o ano",
      "e o sexo da pesquisa"
    )
  ) |>
  select(
    pesquisa,
    ano,
    sexo,
    sexo_rotulo,
    indicador,
    ponderacao,
    estimativa,
    erro_padrao,
    coeficiente_variacao_pct,
    ic_inferior,
    ic_superior,
    estimativa_pct,
    ic_inferior_pct,
    ic_superior_pct
  ) |>
  arrange(
    indicador,
    sexo,
    ano
  )

print(prevalencias_agregadas_estrutura_ano, n = Inf)


#--- Padronização direta -------------------------------------------------------

# survey::svystandardize() faz a padronização dentro dos domínios de sexo.
# A função modifica os pesos do desenho para que homens e mulheres tenham a
# mesma distribuição etária de 2019, mantendo os totais estimados de cada
# domínio. Desse modo, a variância é calculada por linearização no próprio
# desenho complexo e preserva as covariâncias induzidas por estratos e UPA.

populacao_padrao_2019 <- pesos_etarios_2019 |>
  arrange(faixa_idade) |>
  pull(peso_etario_2019)

names(populacao_padrao_2019) <- faixas_idade

if (
  length(populacao_padrao_2019) != length(faixas_idade) ||
  any(!is.finite(populacao_padrao_2019)) ||
  any(populacao_padrao_2019 <= 0) ||
  abs(sum(populacao_padrao_2019) - 1) > 1e-10
) {
  stop("O vetor da população-padrão de 2019 é inválido.")
}

# ----- POF 2002-2003 -----

des_pof02_padronizado_2019 <- survey::svystandardize(
  design = des_pof02,
  by = ~faixa_idade,
  over = ~sexo,
  population = populacao_padrao_2019
)

prev_sobrepeso_25a29_pad_pof02 <- svyby(
  formula = ~sobrepeso_25a29,
  by = ~sexo,
  design = des_pof02_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_25a29_pad_pof02) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_sobrepeso_25a29_pad_pof02 <- prev_sobrepeso_25a29_pad_pof02 |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano = 2003L,
    indicador = "Sobrepeso (25–<30)",
    .before = 1
  )

prev_excesso_peso_pad_pof02 <- svyby(
  formula = ~excesso_peso,
  by = ~sexo,
  design = des_pof02_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_excesso_peso_pad_pof02) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_excesso_peso_pad_pof02 <- prev_excesso_peso_pad_pof02 |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano = 2003L,
    indicador = "Excesso de peso (≥25)",
    .before = 1
  )

prev_obesidade_pad_pof02 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pof02_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_pad_pof02) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_obesidade_pad_pof02 <- prev_obesidade_pad_pof02 |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano = 2003L,
    indicador = "Obesidade (≥30)",
    .before = 1
  )

# ----- POF 2008-2009 -----

des_pof08_padronizado_2019 <- survey::svystandardize(
  design = des_pof08,
  by = ~faixa_idade,
  over = ~sexo,
  population = populacao_padrao_2019
)

prev_sobrepeso_25a29_pad_pof08 <- svyby(
  formula = ~sobrepeso_25a29,
  by = ~sexo,
  design = des_pof08_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_25a29_pad_pof08) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_sobrepeso_25a29_pad_pof08 <- prev_sobrepeso_25a29_pad_pof08 |>
  mutate(
    pesquisa = "POF 2008-2009",
    ano = 2009L,
    indicador = "Sobrepeso (25–<30)",
    .before = 1
  )

prev_excesso_peso_pad_pof08 <- svyby(
  formula = ~excesso_peso,
  by = ~sexo,
  design = des_pof08_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_excesso_peso_pad_pof08) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_excesso_peso_pad_pof08 <- prev_excesso_peso_pad_pof08 |>
  mutate(
    pesquisa = "POF 2008-2009",
    ano = 2009L,
    indicador = "Excesso de peso (≥25)",
    .before = 1
  )

prev_obesidade_pad_pof08 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pof08_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_pad_pof08) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_obesidade_pad_pof08 <- prev_obesidade_pad_pof08 |>
  mutate(
    pesquisa = "POF 2008-2009",
    ano = 2009L,
    indicador = "Obesidade (≥30)",
    .before = 1
  )

# ----- PNS 2013 -----

des_pns13_padronizado_2019 <- survey::svystandardize(
  design = des_pns13,
  by = ~faixa_idade,
  over = ~sexo,
  population = populacao_padrao_2019
)

prev_sobrepeso_25a29_pad_pns13 <- svyby(
  formula = ~sobrepeso_25a29,
  by = ~sexo,
  design = des_pns13_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_25a29_pad_pns13) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_sobrepeso_25a29_pad_pns13 <- prev_sobrepeso_25a29_pad_pns13 |>
  mutate(
    pesquisa = "PNS 2013",
    ano = 2013L,
    indicador = "Sobrepeso (25–<30)",
    .before = 1
  )

prev_excesso_peso_pad_pns13 <- svyby(
  formula = ~excesso_peso,
  by = ~sexo,
  design = des_pns13_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_excesso_peso_pad_pns13) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_excesso_peso_pad_pns13 <- prev_excesso_peso_pad_pns13 |>
  mutate(
    pesquisa = "PNS 2013",
    ano = 2013L,
    indicador = "Excesso de peso (≥25)",
    .before = 1
  )

prev_obesidade_pad_pns13 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pns13_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_pad_pns13) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_obesidade_pad_pns13 <- prev_obesidade_pad_pns13 |>
  mutate(
    pesquisa = "PNS 2013",
    ano = 2013L,
    indicador = "Obesidade (≥30)",
    .before = 1
  )

# ----- PNS 2019 -----

des_pns19_padronizado_2019 <- survey::svystandardize(
  design = des_pns19,
  by = ~faixa_idade,
  over = ~sexo,
  population = populacao_padrao_2019
)

prev_sobrepeso_25a29_pad_pns19 <- svyby(
  formula = ~sobrepeso_25a29,
  by = ~sexo,
  design = des_pns19_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_25a29_pad_pns19) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_sobrepeso_25a29_pad_pns19 <- prev_sobrepeso_25a29_pad_pns19 |>
  mutate(
    pesquisa = "PNS 2019",
    ano = 2019L,
    indicador = "Sobrepeso (25–<30)",
    .before = 1
  )

prev_excesso_peso_pad_pns19 <- svyby(
  formula = ~excesso_peso,
  by = ~sexo,
  design = des_pns19_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_excesso_peso_pad_pns19) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_excesso_peso_pad_pns19 <- prev_excesso_peso_pad_pns19 |>
  mutate(
    pesquisa = "PNS 2019",
    ano = 2019L,
    indicador = "Excesso de peso (≥25)",
    .before = 1
  )

prev_obesidade_pad_pns19 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pns19_padronizado_2019,
  FUN = survey::svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_pad_pns19) <- c(
  "sexo", "estimativa", "erro_padrao", "ic_inferior", "ic_superior"
)

prev_obesidade_pad_pns19 <- prev_obesidade_pad_pns19 |>
  mutate(
    pesquisa = "PNS 2019",
    ano = 2019L,
    indicador = "Obesidade (≥30)",
    .before = 1
  )

# Consolidação dos resultados produzidos diretamente pelo pacote survey.
prevalencias_padronizadas <- bind_rows(
  prev_sobrepeso_25a29_pad_pof02,
  prev_excesso_peso_pad_pof02,
  prev_obesidade_pad_pof02,
  prev_sobrepeso_25a29_pad_pof08,
  prev_excesso_peso_pad_pof08,
  prev_obesidade_pad_pof08,
  prev_sobrepeso_25a29_pad_pns13,
  prev_excesso_peso_pad_pns13,
  prev_obesidade_pad_pns13,
  prev_sobrepeso_25a29_pad_pns19,
  prev_excesso_peso_pad_pns19,
  prev_obesidade_pad_pns19
) |>
  mutate(
    sexo = as.character(sexo),
    sexo_rotulo = recode(
      sexo,
      M = "Homens",
      F = "Mulheres"
    ),
    coeficiente_variacao_pct = if_else(
      estimativa > 0,
      100 * erro_padrao / estimativa,
      NA_real_
    ),
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    padronizacao = paste(
      "Padronização direta com survey::svystandardize();",
      "estrutura etária da população brasileira de 2019, ambos os sexos,",
      "IBGE, Projeções da População, revisão 2024"
    ),
    metodo_inferencia = paste(
      "Linearização de Taylor no desenho padronizado;",
      "IC 95% logit com survey::svyciprop()"
    )
  ) |>
  select(
    pesquisa,
    ano,
    sexo,
    sexo_rotulo,
    indicador,
    padronizacao,
    metodo_inferencia,
    estimativa,
    erro_padrao,
    coeficiente_variacao_pct,
    ic_inferior,
    ic_superior,
    estimativa_pct,
    ic_inferior_pct,
    ic_superior_pct
  ) |>
  arrange(
    indicador,
    sexo,
    ano
  )

# A decomposição abaixo é mantida somente para auditar a estimativa pontual.
# Ela não é usada para calcular o erro-padrão ou o intervalo de confiança.
contribuicoes_padronizacao <- prevalencias_idade |>
  mutate(
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    )
  ) |>
  left_join(
    pesos_etarios_2019,
    by = "faixa_idade"
  ) |>
  mutate(
    contribuicao_estimativa = peso_etario_2019 * estimativa
  )

if (any(is.na(contribuicoes_padronizacao$peso_etario_2019))) {
  stop(
    "Há grupos etários das prevalências sem correspondência nos pesos de 2019."
  )
}

auditoria_estimativa_padronizada <- contribuicoes_padronizacao |>
  group_by(
    pesquisa,
    ano,
    sexo,
    indicador
  ) |>
  summarise(
    estimativa_por_componentes = sum(contribuicao_estimativa),
    .groups = "drop"
  ) |>
  left_join(
    prevalencias_padronizadas |>
      select(
        pesquisa,
        ano,
        sexo,
        indicador,
        estimativa_survey = estimativa
      ),
    by = c("pesquisa", "ano", "sexo", "indicador")
  ) |>
  mutate(
    diferenca_absoluta = abs(
      estimativa_por_componentes - estimativa_survey
    )
  )

if (
  any(!is.finite(auditoria_estimativa_padronizada$diferenca_absoluta)) ||
  max(auditoria_estimativa_padronizada$diferenca_absoluta) > 1e-8
) {
  stop(
    "A estimativa de survey::svystandardize() não coincidiu com a média ponderada ",
    "das prevalências etárias. Revise a ordem da população-padrão."
  )
}

print(prevalencias_padronizadas, n = Inf)
print(auditoria_estimativa_padronizada, n = Inf)


#--- Exportação das tabelas ----------------------------------------------------

dir.create(
  diretorio_tabelas,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  populacao_anos_pesquisas,
  file.path(
    diretorio_tabelas,
    "populacao_projetada_sexo_faixa_anos_pesquisas.csv"
  ),
  na = ""
)

write_csv(
  pesos_etarios_2019,
  file.path(
    diretorio_tabelas,
    "pesos_etarios_populacao_2019.csv"
  ),
  na = ""
)

write_csv(
  contribuicoes_estrutura_ano,
  file.path(
    diretorio_tabelas,
    "contribuicoes_agregacao_estrutura_ano.csv"
  ),
  na = ""
)

write_csv(
  prevalencias_agregadas_estrutura_ano,
  file.path(
    diretorio_tabelas,
    "prevalencias_agregadas_estrutura_ano.csv"
  ),
  na = ""
)

write_csv(
  contribuicoes_padronizacao,
  file.path(
    diretorio_tabelas,
    "contribuicoes_padronizacao_etaria_2019.csv"
  ),
  na = ""
)

write_csv(
  prevalencias_padronizadas,
  file.path(
    diretorio_tabelas,
    "prevalencias_padronizadas_etaria_2019.csv"
  ),
  na = ""
)

write_csv(
  auditoria_estimativa_padronizada,
  file.path(
    diretorio_tabelas,
    "auditoria_padronizacao_survey_2019.csv"
  ),
  na = ""
)


#--- Configuração visual -------------------------------------------------------

source("script/figuras_abnt.R")

cores_sexo <- c(
  "Homens" = "#28587B",
  "Mulheres" = "#A23B72"
)

tema_academico <- tema_figura_abnt()


#--- Gráfico 1: evolução das prevalências por faixa etária --------------------

dados_grafico_idade <- prevalencias_idade |>
  mutate(
    pesquisa = factor(
      pesquisa,
      levels = pesquisas_analisadas,
      ordered = TRUE
    ),
    faixa_idade = factor(
      faixa_idade,
      levels = faixas_idade,
      ordered = TRUE
    ),
    indicador = factor(
      indicador,
      levels = c(
        "Sobrepeso (25–<30)",
        "Excesso de peso (≥25)",
        "Obesidade (≥30)"
      ),
      ordered = TRUE
    ),
    sexo_rotulo = recode(
      sexo,
      M = "Homens",
      F = "Mulheres"
    )
  )

grafico_prevalencia_idade <- ggplot(
  dados_grafico_idade,
  aes(
    x = faixa_idade,
    y = estimativa,
    color = pesquisa,
    shape = pesquisa,
    group = pesquisa
  )
) +
  geom_linerange(
    aes(
      ymin = ic_inferior,
      ymax = ic_superior
    ),
    linewidth = 0.44,
    alpha = 0.72
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.15) +
  facet_grid(
    rows = vars(indicador),
    cols = vars(sexo_rotulo),
    switch = "y"
  ) +
  scale_color_manual(values = cores_pesquisas_abnt) +
  scale_shape_manual(values = formas_pesquisas_abnt) +
  scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Prevalências antropométricas por idade e sexo",
    subtitle = paste(
      "Brasil, POF 2002-2003 e 2008-2009;",
      "PNS 2013 e 2019 — intervalos de confiança de 95%"
    ),
    x = "Grupo etário",
    y = "Prevalência estimada",
    color = "Pesquisa",
    shape = "Pesquisa",
    caption = paste(
      "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
      "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019.",
      "\nNota: sobrepeso = 25 ≤ IMC < 30 kg/m²; excesso de peso = IMC ≥ 25 kg/m²",
      "e inclui as pessoas com obesidade; obesidade = IMC ≥ 30 kg/m².",
      "\nElaboração própria."
    )
  ) +
  tema_academico +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.y.left = element_text(angle = 90, size = 9.5)
  )

if (interactive()) print(grafico_prevalencia_idade)


#--- Gráfico 2: evolução das prevalências padronizadas ------------------------

dados_grafico_padronizado <- prevalencias_padronizadas |>
  mutate(
    pesquisa = factor(
      pesquisa,
      levels = pesquisas_analisadas,
      ordered = TRUE
    ),
    indicador = factor(
      indicador,
      levels = c(
        "Sobrepeso (25–<30)",
        "Excesso de peso (≥25)",
        "Obesidade (≥30)"
      ),
      ordered = TRUE
    )
  )

grafico_prevalencia_padronizada <- ggplot(
  dados_grafico_padronizado,
  aes(
    x = pesquisa,
    y = estimativa,
    color = sexo_rotulo,
    shape = sexo_rotulo,
    group = sexo_rotulo
  )
) +
  geom_errorbar(
    aes(
      ymin = ic_inferior,
      ymax = ic_superior
    ),
    width = 0.08,
    linewidth = 0.55,
    position = position_dodge(width = 0.25)
  ) +
  geom_line(
    linewidth = 0.75,
    position = position_dodge(width = 0.25)
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(width = 0.25)
  ) +
  facet_wrap(
    vars(indicador),
    ncol = 1,
    scales = "free_y"
  ) +
  scale_color_manual(values = cores_sexo) +
  scale_shape_manual(
    values = c(
      "Homens" = 16,
      "Mulheres" = 17
    )
  ) +
  scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0.03, 0.10))
  ) +
  scale_x_discrete(labels = c(
    "POF 2002-2003" = "2003", "POF 2008-2009" = "2009",
    "PNS 2013" = "2013", "PNS 2019" = "2019"
  )) +
  labs(
    title = paste(
      "Evolução das prevalências antropométricas",
      "padronizadas por idade"
    ),
    subtitle = paste(
      "População de 20 anos ou mais; estrutura etária fixa da",
      "população brasileira projetada para 2019"
    ),
    x = "Ano de referência",
    y = "Prevalência padronizada",
    color = "Sexo",
    shape = "Sexo",
    caption = paste(
      "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
      "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019;",
      "Projeções da População, revisão 2024.",
      "\nNota: sobrepeso = 25 ≤ IMC < 30 kg/m²; excesso de peso = IMC ≥ 25 kg/m²",
      "e inclui as pessoas com obesidade; obesidade = IMC ≥ 30 kg/m².",
      "Padrão de 2019 formado pela população de ambos os sexos;",
      "padronização direta com survey::svystandardize();",
      "IC 95% logit calculado no desenho padronizado.",
      "\nElaboração própria."
    )
  ) +
  tema_academico

if (interactive()) print(grafico_prevalencia_padronizada)


#--- Exportação das figuras ----------------------------------------------------

dir.create(
  diretorio_figuras,
  recursive = TRUE,
  showWarnings = FALSE
)

salvar_figura_abnt(
  grafico_prevalencia_padronizada,
  file.path(diretorio_figuras, "prevalencia_padronizada_estrutura_2019"),
  altura_cm = 18
)

message(
  "Padronização direta concluída com survey::svystandardize().\n",
  "Tabelas: ", diretorio_tabelas, "\n",
  "Figura: ", diretorio_figuras
)
