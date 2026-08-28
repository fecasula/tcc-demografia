#------------------------------------------------------
# CRIAR GRÁFICOS SOBRE PAF POR SEXO REGIÃO E IDADE
# Formato ABNT para Dissertação de Mestrado
#------------------------------------------------------

rm(list = ls(all = TRUE))

#---------  Bibliotecas ------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)

#---------  Dados Resultados ------------

# Diretório base
base_path <- "D:/TCC/tcc_demografia_2026/2a_versao"

#----2013
e_gain_2013 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/e_gain_2013.csv"))
mortes_evitaveis_2013 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/mortes_evitaveis_2013.csv"))
paf_sint_2013 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/paf_sint_2013.csv"))
paf_2013 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/paf_2013.csv"))

#----2019
e_gain_2019 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/e_gain_2019.csv"))
mortes_evitaveis_2019 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/mortes_evitaveis_2019.csv"))
paf_sint_2019 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/paf_sint_2019.csv"))
paf_2019 <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/paf_2019.csv"))

#---------  Configurações ABNT ------------

# Tema ABNT para gráficos
tema_abnt <- theme_bw() +
  theme(
    # Fonte e tamanho conforme ABNT
    text = element_text(family = "serif", size = 12),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 1),
    
    # Eixos
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    # Legenda
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.box.background = element_rect(color = "black", size = 0.5),
    
    # Bordas
    panel.border = element_rect(color = "black", size = 0.5),
    panel.grid.major = element_line(color = "gray80", size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Margens
    plot.margin = margin(10, 10, 10, 10)
  )

# Cores consistentes para sexo
cor_sexo <- c("Masculino" = "#2B5B84", "Feminino" = "#C0392B")  # Azul e vermelho mais sóbrios

#---------  Preparação dos Dados ------------

# Combinar dados PAF por idade
paf_2013_prep <- paf_2013 %>%
  mutate(ano = "2013",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         faixa_idade = factor(faixa_idade, levels = unique(faixa_idade)))

paf_2019_prep <- paf_2019 %>%
  mutate(ano = "2019",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         faixa_idade = factor(faixa_idade, levels = unique(faixa_idade)))

paf_combinado <- bind_rows(paf_2013_prep, paf_2019_prep)

# Combinar dados de mortes evitáveis
mortes_2013_prep <- mortes_evitaveis_2013 %>%
  mutate(ano = "2013",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         faixa_idade = factor(faixa_idade, levels = unique(faixa_idade)))

mortes_2019_prep <- mortes_evitaveis_2019 %>%
  mutate(ano = "2019",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         faixa_idade = factor(faixa_idade, levels = unique(faixa_idade)))

mortes_combinado <- bind_rows(mortes_2013_prep, mortes_2019_prep)

# Preparar dados PAF sintética
paf_sint_2013_prep <- paf_sint_2013 %>%
  mutate(ano = "2013",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         categoria = paste(sexo_nome, "2013"))

paf_sint_2019_prep <- paf_sint_2019 %>%
  mutate(ano = "2019",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         categoria = paste(sexo_nome, "2019"))

paf_sint_combinado <- bind_rows(paf_sint_2013_prep, paf_sint_2019_prep) %>%
  mutate(categoria = factor(categoria, 
                            levels = c("Masculino 2013", "Feminino 2013", 
                                       "Masculino 2019", "Feminino 2019")))

# Preparar dados de ganhos em expectativa de vida
e_gain_2013_prep <- e_gain_2013 %>%
  mutate(ano = "2013",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         categoria = paste(sexo_nome, "2013"))

e_gain_2019_prep <- e_gain_2019 %>%
  mutate(ano = "2019",
         sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
         categoria = paste(sexo_nome, "2019"))

e_gain_combinado <- bind_rows(e_gain_2013_prep, e_gain_2019_prep) %>%
  mutate(categoria = factor(categoria, 
                            levels = c("Masculino 2013", "Feminino 2013", 
                                       "Masculino 2019", "Feminino 2019")))

#---------  Gráfico 1: PAF por Sexo e Grupo Etário ------------

plot_paf_idade <- ggplot(paf_combinado, 
                         aes(x = faixa_idade, y = paf, 
                             color = sexo_nome, 
                             linetype = ano,
                             group = interaction(sexo_nome, ano))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(name = "Sexo:",
                     values = cor_sexo) +
  scale_linetype_manual(name = "Ano:",
                        values = c("2013" = "dashed", "2019" = "solid")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, big.mark = "."),
                     limits = c(0, max(paf_combinado$paf) * 1.1),
                     expand = c(0, 0)) +
  labs(
    title = "Fração Atribuível à População (FAP) por Sexo e Grupo Etário",
    subtitle = "Brasil, 2013 e 2019",
    x = "Grupo Etário",
    y = "Fração Atribuível à População (FAP)",
    caption = "Fonte: Elaboração própria"
  ) +
  tema_abnt

print(plot_paf_idade)

#---------  Gráfico 2: FAP Sintética ------------

plot_paf_sint <- ggplot(paf_sint_combinado, 
                        aes(x = categoria, y = paf_sint, fill = sexo_nome)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.3) +
  geom_text(aes(label = scales::percent(paf_sint, accuracy = 0.1, big.mark = ".")), 
            vjust = -0.5, size = 3.5, family = "serif") +
  scale_fill_manual(name = "Sexo:",
                    values = cor_sexo) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, big.mark = "."),
                     limits = c(0, max(paf_sint_combinado$paf_sint) * 1.15),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = c("Masculino 2013" = "Masculino\n2013",
                              "Feminino 2013" = "Feminino\n2013",
                              "Masculino 2019" = "Masculino\n2019",
                              "Feminino 2019" = "Feminino\n2019")) +
  labs(
    title = "Fração Atribuível à População (FAP) Sintética",
    subtitle = "Brasil, 2013 e 2019",
    x = "",
    y = "FAP Sintética",
    caption = "Fonte: Elaboração própria"
  ) +
  tema_abnt +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(plot_paf_sint)

#---------  Gráfico 3: Ganhos em Expectativa de Vida ------------

plot_e_gain <- ggplot(e_gain_combinado, 
                      aes(x = categoria, y = ganho, fill = sexo_nome)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", size = 0.3) +
  geom_text(aes(label = sprintf("%.2f", ganho)), 
            vjust = -0.5, size = 3.5, family = "serif") +
  scale_fill_manual(name = "Sexo:",
                    values = cor_sexo) +
  scale_y_continuous(limits = c(0, max(e_gain_combinado$ganho) * 1.15),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = c("Masculino 2013" = "Masculino\n2013",
                              "Feminino 2013" = "Feminino\n2013",
                              "Masculino 2019" = "Masculino\n2019",
                              "Feminino 2019" = "Feminino\n2019")) +
  labs(
    title = "Potenciais Ganhos em Expectativa de Vida",
    subtitle = "Eliminação da obesidade - Brasil, 2013 e 2019",
    x = "",
    y = "Ganhos em Expectativa de Vida (anos)",
    caption = "Fonte: Elaboração própria"
  ) +
  tema_abnt +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(plot_e_gain)

#---------  Gráfico 4: Mortes Evitáveis por Grupo Etário ------------

plot_mortes_evitaveis <- ggplot(mortes_combinado, 
                                aes(x = faixa_idade, y = mortes_evitaveis, 
                                    color = sexo_nome, 
                                    linetype = ano,
                                    group = interaction(sexo_nome, ano))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(name = "Sexo:",
                     values = cor_sexo) +
  scale_linetype_manual(name = "Ano:",
                        values = c("2013" = "dashed", "2019" = "solid")) +
  scale_y_continuous(labels = scales::comma_format(big.mark = "."),
                     expand = c(0, 0)) +
  labs(
    title = "Número Absoluto de Mortes Evitáveis por Grupo Etário",
    subtitle = "Eliminação da obesidade - Brasil, 2013 e 2019",
    x = "Grupo Etário",
    y = "Número de Mortes Evitáveis",
    caption = "Fonte: Elaboração própria"
  ) +
  tema_abnt

print(plot_mortes_evitaveis)

#---------  Gráfico Alternativo: Comparação de Expectativa de Vida ------------

plot_e_comparacao <- ggplot(e_gain_combinado %>% 
                              pivot_longer(cols = c(e_obs, e_ideal),
                                           names_to = "tipo",
                                           values_to = "expectativa") %>%
                              mutate(tipo = ifelse(tipo == "e_obs", "Observada", "Contrafactual")),
                            aes(x = categoria, y = expectativa, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", size = 0.3) +
  geom_text(aes(label = sprintf("%.1f", expectativa)), 
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3, family = "serif") +
  scale_fill_manual(name = "Expectativa:",
                    values = c("Observada" = "gray60", "Contrafactual" = "darkgreen")) +
  scale_y_continuous(limits = c(0, max(e_gain_combinado$e_ideal) * 1.1),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = c("Masculino 2013" = "Masculino\n2013",
                              "Feminino 2013" = "Feminino\n2013",
                              "Masculino 2019" = "Masculino\n2019",
                              "Feminino 2019" = "Feminino\n2019")) +
  labs(
    title = "Expectativa de Vida Observada e Contrafactual",
    subtitle = "Cenário de eliminação da obesidade - Brasil, 2013 e 2019",
    x = "",
    y = "Expectativa de Vida (anos)",
    caption = "Fonte: Elaboração própria"
  ) +
  tema_abnt +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(plot_e_comparacao)

#---------  Salvar Gráficos ------------

# Criar diretório para gráficos
graficos_path <- paste0(base_path, "/analise_contrafactual/output/graficos")
dir.create(graficos_path, showWarnings = FALSE, recursive = TRUE)

# Salvar em alta resolução (300 dpi conforme ABNT)
ggsave(paste0(graficos_path, "/figura1_paf_idade.png"),
       plot = plot_paf_idade, width = 10, height = 6, dpi = 300)

ggsave(paste0(graficos_path, "/figura2_paf_sintetica.png"),
       plot = plot_paf_sint, width = 8, height = 6, dpi = 300)

ggsave(paste0(graficos_path, "/figura3_ganhos_expectativa.png"),
       plot = plot_e_gain, width = 8, height = 6, dpi = 300)

ggsave(paste0(graficos_path, "/figura4_mortes_evitaveis.png"),
       plot = plot_mortes_evitaveis, width = 10, height = 6, dpi = 300)

ggsave(paste0(graficos_path, "/figura5_expectativa_comparacao.png"),
       plot = plot_e_comparacao, width = 8, height = 6, dpi = 300)

#---------  Painel Combinado ------------

# Criar painel com os 4 gráficos principais
painel_principal <- (plot_paf_idade + plot_mortes_evitaveis) / 
  (plot_paf_sint + plot_e_gain) +
  plot_annotation(
    title = "Impacto da Obesidade na Mortalidade da População Brasileira",
    subtitle = "Análise por sexo - 2013 e 2019",
    caption = "Fonte: Elaboração própria",
    theme = theme(
      plot.title = element_text(family = "serif", size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(family = "serif", size = 12, hjust = 0.5),
      plot.caption = element_text(family = "serif", size = 8, hjust = 1)
    )
  )

ggsave(paste0(graficos_path, "/figura_painel_principal.png"),
       plot = painel_principal, width = 14, height = 12, dpi = 300)

# Salvar também em formato PDF para melhor qualidade de impressão
ggsave(paste0(graficos_path, "/figura1_paf_idade.pdf"),
       plot = plot_paf_idade, width = 10, height = 6, device = "pdf")

ggsave(paste0(graficos_path, "/figura2_paf_sintetica.pdf"),
       plot = plot_paf_sint, width = 8, height = 6, device = "pdf")

ggsave(paste0(graficos_path, "/figura3_ganhos_expectativa.pdf"),
       plot = plot_e_gain, width = 8, height = 6, device = "pdf")

ggsave(paste0(graficos_path, "/figura4_mortes_evitaveis.pdf"),
       plot = plot_mortes_evitaveis, width = 10, height = 6, device = "pdf")

cat("Gráficos salvos com sucesso em:", graficos_path, "\n")

