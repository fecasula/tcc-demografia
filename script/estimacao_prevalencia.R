#------------------------------------------------------------------------------- 
# ESTIMAÇÃO PREVALÊNCIA DE SOBREPESO E OBESIDADE NO BRASIL (2002 - 2019)
# TCC Ciências Sociais 
# Felipe Casula, 2026
#-------------------------------------------------------------------------------
rm(list = ls(all = TRUE))

#--- Bibliotecas ---------------------------------------------------------------
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  survey,
  tidyverse,
  janitor
)

# Quando um domínio contém uma UPA solitária, o pacote aplica o ajuste
# conservador em torno da média geral. A segunda opção estende o ajuste aos
# domínios analisados (sexo e grupo etário).
options(
  survey.lonely.psu = "adjust",
  survey.adjust.domain.lonely = TRUE
)

#--- Dados --------------------------------------------------------------------

# ----- POF 2002-2003 -----

# Arquivo utilizado: T_MORADOR.TXT
#
# Posições no arquivo:
#  3-4   = Unidade da Federação
#  5-7   = Número sequencial do setor
#  8     = Dígito verificador do setor
# 12-13  = Estrato geográfico
# 14-24  = Fator de expansão inicial
# 25-35  = Fator de expansão final, utilizado nas estimativas
# 40     = Sexo
# 49-51  = Idade
# 116-120 = Peso imputado
# 121-125 = Altura imputada

pof2002 <- read_fwf(
  file = "dados/raw/pof/T_MORADOR_2002-2003.TXT",
  col_positions = fwf_positions(
    start = c(3, 5, 8, 12, 14, 25, 40, 49, 116, 121),
    end   = c(4, 7, 8, 13, 24, 35, 40, 51, 120, 125),
    col_names = c(
      "uf",
      "sequencial_setor",
      "dv_setor",
      "estrato_pof",
      "peso_inicial",
      "peso_final",
      "sexo_codigo",
      "idade",
      "peso_kg",
      "altura_cm"
    )
  ),
  col_types = cols(.default = col_character()),
  trim_ws = TRUE
)

# ----- POF 2008-2009 -----

# Arquivo utilizado: T_MORADOR_S.txt
#
# Posições no arquivo:
#   3-4   = Unidade da Federação
#  14-15  = Estrato POF
#  16-29  = Peso inicial
#  30-43  = Peso final
#  60-62  = Idade
#  76-77  = Sexo
# 160-161 = Gestação
# 187-191 = Altura imputada
# 192-196 = Peso imputado
# 247-252 = Código da UPA

pof2008 <- read_fwf(
  file = "dados/raw/pof/T_MORADOR_2008-2009.TXT",
  col_positions = fwf_positions(
    start = c(3, 14, 16, 30, 60, 76, 160, 187, 192, 247),
    end   = c(4, 15, 29, 43, 62, 77, 161, 191, 196, 252),
    col_names = c(
      "uf",
      "estrato_pof",
      "peso_inicial",
      "peso_final",
      "idade",
      "sexo_codigo",
      "gestacao_codigo",
      "altura_cm",
      "peso_kg",
      "cod_upa"
    )
  ),
  col_types = cols(.default = col_character()),
  trim_ws = TRUE
)

# ----- PNS -----

objetos_2013 <- load("dados/raw/pns/pns2013.rdata")
objetos_2019 <- load("dados/raw/pns/pns2019.rdata")

# ----- TABUAS DE VIDA -----

# Tabuas de vida IBGE utilizadas para padronização de estrutura etária
lt <- read.csv2(
  "dados/raw/tabuas_de_vida/tabuas_de_vida.csv",
  stringsAsFactors = FALSE
) |>
  clean_names()


#--- Definições comuns ---------------------------------------------------------

# Para a PNS de 2013, a entrevista do 'adulto selecionado' começava aos 18 anos; 
# já em 2019, começa aos 15 anos. Por esse motivo, optei por adotar 20+ como 
# domínio analisado

idade_minima <- 20L

# Grupos etários decenais, com intervalo aberto em 80 anos ou mais
faixas_idade <- c(
  "20-29",
  "30-39",
  "40-49",
  "50-59",
  "60-69",
  "70-79",
  "80+"
)

criar_faixa_idade <- function(idade) {
  cut(
    idade,
    breaks = c(20, 30, 40, 50, 60, 70, 80, Inf),
    labels = faixas_idade,
    right = FALSE,
    ordered_result = TRUE
  )
}

# Categorização binária para as prevalências estimadas:
#   sobrepeso = 25,0 a <30,0 kg/m²
#   obesidade = 30,0 kg/m² ou mais, sem separar seus graus
classificar_antropometria <- function(dados) {
  dados |>
    mutate(
      altura_m = altura_cm / 100,
      imc = peso_kg / (altura_m^2),
      
      # Limites usados para evitar que erros de aferição ou digitação sejam
      # interpretados como estados antropométricos reais.
      antropometria_valida =
        !gestante &
        is.finite(peso_kg) & peso_kg > 0 &
        is.finite(altura_cm) & altura_cm > 0 &
        is.finite(imc) & imc >= 15 & imc < 60,
      
      sobrepeso = case_when(
        antropometria_valida ~ as.integer(imc >= 25 & imc < 30),
        TRUE ~ NA_integer_
      ),
      
      obesidade = case_when(
        antropometria_valida ~ as.integer(imc >= 30),
        TRUE ~ NA_integer_
      )
    )
}

#--- Preparação da POF 2002-2003 ----------------------------------------------

pof2002 <- pof2002 |>
  transmute(
    # O código da UPA precisa ser reconstruído nessa edição
    upa = paste0(
      uf,
      sequencial_setor,
      dv_setor
    ),
    
    # O código do estrato não é nacionalmente único. Acrescentei a UF.
    estrato = paste0(
      uf,
      "_",
      estrato_pof
    ),
    
    # O segundo fator de expansão é o peso final destinado às estimativas
    peso_amostral = parse_double(peso_final),
    
    idade = parse_integer(idade),
    
    sexo = case_when(
      sexo_codigo == "1" ~ "M",
      sexo_codigo == "2" ~ "F",
      TRUE ~ NA_character_
    ),
    
    peso_kg = parse_double(peso_kg),
    altura_cm = parse_double(altura_cm),
    
    # A POF 2002-2003 não apresenta, nesse arquivo, um indicador que permita
    # identificar diretamente todas as gestantes.
    gestante = FALSE
  ) |>
  filter(
    is.finite(peso_amostral),
    peso_amostral > 0
  ) |>
  mutate(
    sexo = factor(sexo, levels = c("M", "F")),
    faixa_idade = criar_faixa_idade(idade)
  ) |>
  classificar_antropometria()

# --- Declração desenho amostral ---

# Declaração amostral da POF 2002-2003
des_pof02 <- svydesign(
  ids = ~upa,
  strata = ~estrato,
  weights = ~peso_amostral,
  data = pof2002,
  nest = TRUE
)

# Domínio efetivamente analisado
des_pof02 <- subset(
  des_pof02,
  idade >= idade_minima &
    !is.na(sexo) &
    !is.na(faixa_idade) &
    antropometria_valida
)

#--- Preparação da POF 2008-2009 ----------------------------------------------

pof2008 <- pof2008 |>
  transmute(
    upa = cod_upa,
    
    # Acrescentar UF impede que estratos de UFs diferentes recebam o mesmo ID.
    estrato = paste0(
      uf,
      "_",
      estrato_pof
    ),
    
    # Fator de expansão ajustado às estimativas populacionais de 15/01/2009.
    peso_amostral = parse_double(peso_final),
    
    idade = parse_integer(idade),
    
    sexo = case_when(
      sexo_codigo == "1" ~ "M",
      sexo_codigo == "2" ~ "F",
      TRUE ~ NA_character_
    ),
    
    # Código 1 = gestante. Código 2 = não gestante.
    # Código 0 ou vazio corresponde a pessoas fora do universo da pergunta.
    gestante = coalesce(gestacao_codigo == "1", FALSE),
    
    # Essas duas variáveis já são as medidas imputadas pelo IBGE.
    peso_kg = parse_double(peso_kg),
    altura_cm = parse_double(altura_cm)
  ) |>
  filter(
    is.finite(peso_amostral),
    peso_amostral > 0
  ) |>
  mutate(
    sexo = factor(sexo, levels = c("M", "F")),
    faixa_idade = criar_faixa_idade(idade)
  ) |>
  classificar_antropometria()

# --- Declração desenho amostral ---

# Declaração amostral da POF 2008-2009
des_pof08 <- svydesign(
  ids = ~upa,
  strata = ~estrato,
  weights = ~peso_amostral,
  data = pof2008,
  nest = TRUE
)

des_pof08 <- subset(
  des_pof08,
  idade >= idade_minima &
    !is.na(sexo) &
    !is.na(faixa_idade) &
    antropometria_valida
)

#--- Preparação da PNS 2013 ---------------------------------------------------

# W00203 = "Altura - Final (em cm)" 
# W00103 = "Peso - Final (em kg)" 
# C006 = "Sexo" (1 = Masculino; 2 = Feminino) 
# C008 = "Idade do morador na data de referência"
# M001 = "Entrevista do adulto selecionado" (1 = Realizada; 2 = Recusa; 3 = Morador não encontrado)
# P005 = "A Sra está grávida no momento?" (1 = Sim; 2 = Não; 3 = Não sabe)
# V00291 = "Peso do morador selecionado com calibração"

pns2013 <- pns2013 |>
  filter(as.character(M001) == "1") |>
  transmute(
    estrato = V0024,
    upa = UPA_PNS,
    peso_amostral = as.numeric(V00291),
    idade = as.numeric(C008),
    sexo = case_when(
      as.character(C006) == "1" ~ "M",
      as.character(C006) == "2" ~ "F",
      TRUE ~ NA_character_
    ),
    gestante = coalesce(as.character(P005) == "1", FALSE),
    peso_kg = as.numeric(W00103),
    altura_cm = as.numeric(W00203)
  ) |>
  mutate(
    sexo = factor(sexo, levels = c("M", "F")),
    faixa_idade = criar_faixa_idade(idade)
  ) |>
  classificar_antropometria()

# Declaração amostral
des_pns13 <- svydesign(
  ids = ~upa,                # Conglomerados 
  strata = ~estrato,         # Estratos
  weights = ~peso_amostral,  # Pesos calibrados para subamostra
  data = pns2013,            # Dados tratados
  nest = TRUE
)

# O recorte é feito DEPOIS da declaração do desenho, como análise de domínio.
des_pns13 <- subset(
  des_pns13,
  idade >= idade_minima &
    !is.na(sexo) &
    !is.na(faixa_idade) &
    antropometria_valida
)

#--- Preparação da PNS 2019 ---------------------------------------------------

# W00203 = "Altura - Final (em cm)" 
# W00103 = "Peso - Final (em kg)" 
# C006 = "Sexo" (1 = Masculino; 2 = Feminino) 
# C008 = "Idade do morador na data de referência"
# W001 = "Antropometria aferida do morador selecionado" (1 = Realizada; 2 = Recusa; 3 = Morador não encontrado)
# P005 = "A Sra está grávida no momento?" (1 = Sim; 2 = Não; 3 = Não sabe)
# V00301 = "Peso do morador selecionado para antropometria com calibração"

pns2019 <- pns2019 |>
  filter(as.character(W001) == "1") |>
  transmute(
    estrato = V0024,
    upa = UPA_PNS,
    peso_amostral = as.numeric(V00301),
    idade = as.numeric(C008),
    sexo = case_when(
      as.character(C006) == "1" ~ "M",
      as.character(C006) == "2" ~ "F",
      TRUE ~ NA_character_
    ),
    gestante = coalesce(as.character(P005) == "1", FALSE),
    peso_kg = as.numeric(W00103),
    altura_cm = as.numeric(W00203)
  ) |>
  mutate(
    sexo = factor(sexo, levels = c("M", "F")),
    faixa_idade = criar_faixa_idade(idade)
  ) |>
  classificar_antropometria()

# Declaração amostral
des_pns19 <- svydesign(
  ids = ~upa,                # Conglomerados 
  strata = ~estrato,         # Estratos
  weights = ~peso_amostral,  # Pesos calibrados para subamostra
  data = pns2019,            # Dados tratados
  nest = TRUE
)

des_pns19 <- subset(
  des_pns19,
  idade >= idade_minima &
    !is.na(sexo) &
    !is.na(faixa_idade) &
    antropometria_valida
)

#--- Perfil das fontes de dado ------------------------------------------------

perfil_dfs <- function(desenho, pesquisa) {
  tibble(
    pesquisa = pesquisa,
    pessoas_analisadas = nrow(desenho$variables),
    estratos = n_distinct(desenho$variables$estrato),
    upas = n_distinct(desenho$variables$upa),
    peso_minimo = min(weights(desenho)),
    peso_maximo = max(weights(desenho))
  )
}

tb_perfil <- bind_rows(
  perfil_dfs(des_pof02, "POF 2002-2003"),
  perfil_dfs(des_pof08, "POF 2008-2009"),
  perfil_dfs(des_pns13, "PNS 2013"),
  perfil_dfs(des_pns19, "PNS 2019")
)

print(tb_perfil)

# Antes da padronização, todas as combinações sexo x idade precisam existir.
verificar_celulas <- function(desenho, pesquisa) {
  tabela <- with(desenho$variables, table(sexo, faixa_idade))
  
  if (any(tabela == 0)) {
    ausentes <- which(tabela == 0, arr.ind = TRUE)
    rotulos <- paste(
      rownames(tabela)[ausentes[, "row"]],
      colnames(tabela)[ausentes[, "col"]],
      sep = "__"
    )
    stop(
      pesquisa,
      " possui células sexo x idade vazias: ",
      paste(rotulos, collapse = ", ")
    )
  }
}

verificar_celulas(des_pof02, "POF 2002-2003")
verificar_celulas(des_pof08, "POF 2008-2009")
verificar_celulas(des_pns13, "PNS 2013")
verificar_celulas(des_pns19, "PNS 2019")

#--- Prevalências POF 2002-2003 -----------------------------------------------

# 1. Sobrepeso por sexo e grupo etário
prev_sobrepeso_idade_pof02 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo + faixa_idade,
  design = des_pof02,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_idade_pof02) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_idade_pof02 <- prev_sobrepeso_idade_pof02 |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano = 2003L,
    indicador = "Sobrepeso",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 2. Obesidade por sexo e grupo etário
prev_obesidade_idade_pof02 <- svyby(
  formula = ~obesidade,
  by = ~sexo + faixa_idade,
  design = des_pof02,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_obesidade_idade_pof02) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_idade_pof02 <- prev_obesidade_idade_pof02 |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano = 2003L,
    indicador = "Obesidade",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 3. Resultados específicos por sexo e idade
prev_idade_pof02 <- bind_rows(
  prev_sobrepeso_idade_pof02,
  prev_obesidade_idade_pof02
) |>
  arrange(indicador, sexo, faixa_idade)

print(prev_idade_pof02, n = Inf)


# 4. Sobrepeso agregado por sexo
prev_sobrepeso_sexo_pof02 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo,
  design = des_pof02,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_sexo_pof02) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_sexo_pof02 <- prev_sobrepeso_sexo_pof02 |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano = 2003L,
    indicador = "Sobrepeso",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 5. Obesidade agregada por sexo
prev_obesidade_sexo_pof02 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pof02,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_sexo_pof02) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_sexo_pof02 <- prev_obesidade_sexo_pof02 |>
  mutate(
    pesquisa = "POF 2002-2003",
    ano = 2003L,
    indicador = "Obesidade",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 6. Resultado agregado da POF 2002-2003
prev_sexo_pof02 <- bind_rows(
  prev_sobrepeso_sexo_pof02,
  prev_obesidade_sexo_pof02
) |>
  arrange(indicador, sexo)

print(prev_sexo_pof02, n = Inf)


#--- Prevalências POF 2008-2009 -----------------------------------------------

# 1. Sobrepeso por sexo e grupo etário
prev_sobrepeso_idade_pof08 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo + faixa_idade,
  design = des_pof08,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_idade_pof08) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_idade_pof08 <- prev_sobrepeso_idade_pof08 |>
  mutate(
    pesquisa = "POF 2008-2009",
    ano = 2009L,
    indicador = "Sobrepeso",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 2. Obesidade por sexo e grupo etário
prev_obesidade_idade_pof08 <- svyby(
  formula = ~obesidade,
  by = ~sexo + faixa_idade,
  design = des_pof08,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_obesidade_idade_pof08) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_idade_pof08 <- prev_obesidade_idade_pof08 |>
  mutate(
    pesquisa = "POF 2008-2009",
    ano = 2009L,
    indicador = "Obesidade",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 3. Resultados específicos por sexo e idade
prev_idade_pof08 <- bind_rows(
  prev_sobrepeso_idade_pof08,
  prev_obesidade_idade_pof08
) |>
  arrange(indicador, sexo, faixa_idade)

#print(prev_idade_pof08, n = Inf)


# 4. Sobrepeso agregado por sexo
prev_sobrepeso_sexo_pof08 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo,
  design = des_pof08,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_sexo_pof08) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_sexo_pof08 <- prev_sobrepeso_sexo_pof08 |>
  mutate(
    pesquisa = "POF 2008-2009",
    ano = 2009L,
    indicador = "Sobrepeso",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 5. Obesidade agregada por sexo
prev_obesidade_sexo_pof08 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pof08,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_sexo_pof08) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_sexo_pof08 <- prev_obesidade_sexo_pof08 |>
  mutate(
    pesquisa = "POF 2008-2009",
    ano = 2009L,
    indicador = "Obesidade",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 6. Resultado agregado da POF 2008-2009
prev_sexo_pof08 <- bind_rows(
  prev_sobrepeso_sexo_pof08,
  prev_obesidade_sexo_pof08
) |>
  arrange(indicador, sexo)

#print(prev_sexo_pof08, n = Inf)

#--- Prevalências PNS 2013 ----------------------------------------------------

# 1. Sobrepeso por sexo e grupo etário
prev_sobrepeso_idade_pns13 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo + faixa_idade,
  design = des_pns13,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_idade_pns13) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_idade_pns13 <- prev_sobrepeso_idade_pns13 |>
  mutate(
    pesquisa = "PNS 2013",
    ano = 2013L,
    indicador = "Sobrepeso",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 2. Obesidade por sexo e grupo etário
prev_obesidade_idade_pns13 <- svyby(
  formula = ~obesidade,
  by = ~sexo + faixa_idade,
  design = des_pns13,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_obesidade_idade_pns13) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_idade_pns13 <- prev_obesidade_idade_pns13 |>
  mutate(
    pesquisa = "PNS 2013",
    ano = 2013L,
    indicador = "Obesidade",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 3. Reunir os resultados específicos por idade
prev_idade_pns13 <- bind_rows(
  prev_sobrepeso_idade_pns13,
  prev_obesidade_idade_pns13
) |>
  arrange(indicador, sexo, faixa_idade)

print(prev_idade_pns13, n = Inf)


# 4. Sobrepeso agregado por sexo, sem padronização etária
prev_sobrepeso_sexo_pns13 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo,
  design = des_pns13,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_sexo_pns13) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_sexo_pns13 <- prev_sobrepeso_sexo_pns13 |>
  mutate(
    pesquisa = "PNS 2013",
    ano = 2013L,
    indicador = "Sobrepeso",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 5. Obesidade agregada por sexo, sem padronização etária
prev_obesidade_sexo_pns13 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pns13,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_sexo_pns13) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_sexo_pns13 <- prev_obesidade_sexo_pns13 |>
  mutate(
    pesquisa = "PNS 2013",
    ano = 2013L,
    indicador = "Obesidade",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 6. Resultado agregado da PNS 2013
prev_sexo_pns13 <- bind_rows(
  prev_sobrepeso_sexo_pns13,
  prev_obesidade_sexo_pns13
) |>
  arrange(indicador, sexo)

print(prev_sexo_pns13, n = Inf)


#--- Prevalências PNS 2019 ----------------------------------------------------

# 1. Sobrepeso por sexo e grupo etário
prev_sobrepeso_idade_pns19 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo + faixa_idade,
  design = des_pns19,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_idade_pns19) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_idade_pns19 <- prev_sobrepeso_idade_pns19 |>
  mutate(
    pesquisa = "PNS 2019",
    ano = 2019L,
    indicador = "Sobrepeso",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 2. Obesidade por sexo e grupo etário
prev_obesidade_idade_pns19 <- svyby(
  formula = ~obesidade,
  by = ~sexo + faixa_idade,
  design = des_pns19,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE,
  drop.empty.groups = FALSE
) |>
  as_tibble()

names(prev_obesidade_idade_pns19) <- c(
  "sexo",
  "faixa_idade",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_idade_pns19 <- prev_obesidade_idade_pns19 |>
  mutate(
    pesquisa = "PNS 2019",
    ano = 2019L,
    indicador = "Obesidade",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 3. Reunir os resultados específicos por idade
prev_idade_pns19 <- bind_rows(
  prev_sobrepeso_idade_pns19,
  prev_obesidade_idade_pns19
) |>
  arrange(indicador, sexo, faixa_idade)

print(prev_idade_pns19, n = Inf)


# 4. Sobrepeso agregado por sexo, sem padronização etária
prev_sobrepeso_sexo_pns19 <- svyby(
  formula = ~sobrepeso,
  by = ~sexo,
  design = des_pns19,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_sobrepeso_sexo_pns19) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_sobrepeso_sexo_pns19 <- prev_sobrepeso_sexo_pns19 |>
  mutate(
    pesquisa = "PNS 2019",
    ano = 2019L,
    indicador = "Sobrepeso",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )


# 5. Obesidade agregada por sexo, sem padronização etária
prev_obesidade_sexo_pns19 <- svyby(
  formula = ~obesidade,
  by = ~sexo,
  design = des_pns19,
  FUN = svyciprop,
  method = "logit",
  vartype = c("se", "ci"),
  level = 0.95,
  na.rm = TRUE,
  keep.names = FALSE
) |>
  as_tibble()

names(prev_obesidade_sexo_pns19) <- c(
  "sexo",
  "estimativa",
  "erro_padrao",
  "ic_inferior",
  "ic_superior"
)

prev_obesidade_sexo_pns19 <- prev_obesidade_sexo_pns19 |>
  mutate(
    pesquisa = "PNS 2019",
    ano = 2019L,
    indicador = "Obesidade",
    padronizacao = "Não padronizada",
    estimativa_pct = 100 * estimativa,
    ic_inferior_pct = 100 * ic_inferior,
    ic_superior_pct = 100 * ic_superior,
    .before = 1
  )

# 6. Resultado agregado da PNS 2019
prev_sexo_pns19 <- bind_rows(
  prev_sobrepeso_sexo_pns19,
  prev_obesidade_sexo_pns19
) |>
  arrange(indicador, sexo)

print(prev_sexo_pns19, n = Inf)

#--- Consolidação dos resultados ----------------------------------------------

# Resultados específicos por sexo e grupo etário
df_prevalencia_idade <- bind_rows(
  prev_idade_pof02,
  prev_idade_pof08,
  prev_idade_pns13,
  prev_idade_pns19
) |>
  mutate(
    tipo_resultado = "Por sexo e grupo etário",
    padronizacao = "Não padronizada"
  )


# Resultados agregados por sexo
df_prevalencia_sexo <- bind_rows(
  prev_sexo_pof02,
  prev_sexo_pof08,
  prev_sexo_pns13,
  prev_sexo_pns19
) |>
  mutate(
    faixa_idade = "20+",
    tipo_resultado = "Agregada por sexo"
  )


# Base única contendo os dois níveis de resultado
df_prevalencias <- bind_rows(
  df_prevalencia_idade,
  df_prevalencia_sexo
) |>
  mutate(
    pesquisa = factor(
      pesquisa,
      levels = c(
        "POF 2002-2003",
        "POF 2008-2009",
        "PNS 2013",
        "PNS 2019"
      ),
      ordered = TRUE
    ),
    
    ano = as.integer(ano),
    
    sexo = factor(
      as.character(sexo),
      levels = c("M", "F")
    ),
    
    sexo_rotulo = recode(
      as.character(sexo),
      M = "Homens",
      F = "Mulheres"
    ),
    
    indicador = factor(
      indicador,
      levels = c("Sobrepeso", "Obesidade"),
      ordered = TRUE
    ),
    
    faixa_idade = factor(
      as.character(faixa_idade),
      levels = c(faixas_idade, "20+"),
      ordered = TRUE
    )
  ) |>
  select(
    pesquisa,
    ano,
    tipo_resultado,
    padronizacao,
    sexo,
    sexo_rotulo,
    faixa_idade,
    indicador,
    estimativa,
    erro_padrao,
    ic_inferior,
    ic_superior,
    estimativa_pct,
    ic_inferior_pct,
    ic_superior_pct
  ) |>
  arrange(
    tipo_resultado,
    indicador,
    sexo,
    pesquisa,
    faixa_idade
  )

#print(df_prevalencias, n = Inf)

#--- Exportação das tabelas ----------------------------------------------------

dir.create(
  "dados/output/prevalencias",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  df_prevalencias,
  "dados/output/prevalencias/prevalencias_2003_2019.csv",
  na = ""
)

write_csv(
  df_prevalencia_idade,
  "dados/output/prevalencias/prevalencias_sexo_idade_2003_2019.csv",
  na = ""
)

write_csv(
  df_prevalencia_sexo,
  "dados/output/prevalencias/prevalencias_agregadas_sexo_2003_2019.csv",
  na = ""
)

#--- Configuração visual -------------------------------------------------------

cores_pesquisas <- c(
  "POF 2002-2003" = "#264653",
  "POF 2008-2009" = "#2A9D8F",
  "PNS 2013" = "#E9A03B",
  "PNS 2019" = "#B23A48"
)

cores_sexo <- c(
  "Homens" = "#28587B",
  "Mulheres" = "#A23B72"
)

tema_academico <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      color = "#222222"
    ),
    plot.subtitle = element_text(
      size = 10.5,
      color = "#4D4D4D",
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 8.5,
      color = "#555555",
      hjust = 0,
      margin = margin(t = 12)
    ),
    strip.text = element_text(
      face = "bold",
      color = "#222222"
    ),
    strip.background = element_rect(
      fill = "#F2F2F2",
      color = "#D9D9D9"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "#333333"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(12, 15, 12, 12)
  )

#--- Gráfico das prevalências específicas por idade ---------------------------

dados_grafico_idade <- df_prevalencias |>
  filter(tipo_resultado == "Por sexo e grupo etário") |>
  mutate(
    faixa_idade = factor(
      as.character(faixa_idade),
      levels = faixas_idade,
      ordered = TRUE
    )
  )

grafico_prevalencia_idade <- ggplot(
  dados_grafico_idade,
  aes(
    x = faixa_idade,
    y = estimativa,
    color = pesquisa,
    group = pesquisa
  )
) +
  geom_linerange(
    aes(
      ymin = ic_inferior,
      ymax = ic_superior
    ),
    linewidth = 0.25,
    alpha = 0.30
  ) +
  geom_line(
    linewidth = 0.75
  ) +
  geom_point(
    size = 1.7
  ) +
  facet_grid(
    rows = vars(indicador),
    cols = vars(sexo_rotulo)
  ) +
  scale_color_manual(
    values = cores_pesquisas
  ) +
  scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  labs(
    title = "Prevalência de sobrepeso e obesidade por idade e sexo",
    subtitle = paste(
      "Brasil, POF 2002-2003 e 2008-2009;",
      "PNS 2013 e 2019 — intervalos de confiança de 95%"
    ),
    x = "Grupo etário",
    y = "Prevalência estimada",
    color = "Pesquisa",
    caption = paste(
      "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
      "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019.",
      "\nElaboração própria."
    )
  ) +
  tema_academico +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(grafico_prevalencia_idade)

#--- Gráfico das prevalências agregadas ---------------------------------------

dados_grafico_sexo <- df_prevalencias |>
  filter(tipo_resultado == "Agregada por sexo")

grafico_prevalencia_sexo <- ggplot(
  dados_grafico_sexo,
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
    ncol = 2,
    scales = "free_y"
  ) +
  scale_color_manual(
    values = cores_sexo
  ) +
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
  labs(
    title = "Evolução da prevalência de sobrepeso e obesidade",
    subtitle = paste(
      "Estimativas agregadas por sexo, população de 20 anos ou mais;",
      "intervalos de confiança de 95%"
    ),
    x = "Pesquisa",
    y = "Prevalência estimada",
    color = "Sexo",
    shape = "Sexo",
    caption = paste(
      "Fonte: IBGE, Pesquisa de Orçamentos Familiares 2002-2003 e",
      "2008-2009; Pesquisa Nacional de Saúde 2013 e 2019.",
      "\nNota: estimativas ainda não padronizadas por idade.",
      "\nElaboração própria."
    )
  ) +
  tema_academico +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

print(grafico_prevalencia_sexo)

#--- Exportação das figuras ----------------------------------------------------

dir.create(
  "output/graficos_prevalencia",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = "output/graficos_prevalencia/prevalencia_por_idade_sexo.png",
  plot = grafico_prevalencia_idade,
  width = 12,
  height = 8,
  units = "in",
  dpi = 320,
  bg = "white"
)

ggsave(
  filename = "output/graficos_prevalencia/prevalencia_agregada_por_sexo.png",
  plot = grafico_prevalencia_sexo,
  width = 11,
  height = 6.5,
  units = "in",
  dpi = 320,
  bg = "white"
)

