#------------------------------------------------------
# GRÁFICO DECOMPOSIÇÃO DE KITAGAWA
# Efeito Risco x Efeito de Composição por IMC
# Formato ABNT para Dissertação de Mestrado
#------------------------------------------------------

rm(list = ls(all = TRUE))

#---------  Bibliotecas ------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

#---------  Dados ------------

# Carregar dados da decomposição de Kitagawa
base_path <- "D:/TCC/tcc_demografia_2026/2a_versao"
kitagawa_final <- read.csv(paste0(base_path, "/analise_contrafactual/output/resultados/kitagawa.csv"))

#---------  Configurações ABNT ------------

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
cor_sexo <- c("Masculino" = "#2B5B84", "Feminino" = "#C0392B")

#------------------------------------------------------
# GRÁFICO DECOMPOSIÇÃO DE KITAGAWA - VERSÃO REFORMULADA
# Efeito Risco x Efeito de Composição por IMC
# Formato ABNT para Dissertação de Mestrado
#------------------------------------------------------

#---------  Preparação dos Dados ------------

# Preparar dados com os efeitos absolutos
kitagawa_plot <- kitagawa_final %>%
  mutate(
    sexo_nome = ifelse(sexo == "M", "Masculino", "Feminino"),
    faixa_idade = factor(faixa_idade, levels = unique(faixa_idade))
  )

# Transformar para formato longo para plotagem
kitagawa_long <- kitagawa_plot %>%
  select(faixa_idade, sexo_nome, efeito_composicao, efeito_taxa) %>%
  pivot_longer(
    cols = c(efeito_composicao, efeito_taxa),
    names_to = "tipo_efeito",
    values_to = "valor_efeito"
  ) %>%
  mutate(
    tipo_efeito = case_when(
      tipo_efeito == "efeito_composicao" ~ "Efeito Composição (IMC)",
      tipo_efeito == "efeito_taxa" ~ "Efeito Taxa (Mortalidade)"
    ),
    # Definir tipo de linha: tracejada para taxa, contínua para composição
    tipo_linha = case_when(
      tipo_efeito == "Efeito Composição (IMC)" ~ "Composição",
      tipo_efeito == "Efeito Taxa (Mortalidade)" ~ "Taxa"
    )
  )


#---------  Gráfico Alternativo: Efeitos Relativos (Percentuais) ------------

# Calcular efeitos relativos
kitagawa_pct_long <- kitagawa_plot %>%
  mutate(
    # Efeitos como percentual do delta total
    pct_efeito_composicao = (efeito_composicao / delta_tem) * 100,
    pct_efeito_taxa = (efeito_taxa / delta_tem) * 100
  ) %>%
  select(faixa_idade, sexo_nome, pct_efeito_composicao, pct_efeito_taxa) %>%
  pivot_longer(
    cols = c(pct_efeito_composicao, pct_efeito_taxa),
    names_to = "tipo_efeito",
    values_to = "percentual"
  ) %>%
  mutate(
    tipo_efeito = case_when(
      tipo_efeito == "pct_efeito_composicao" ~ "Efeito Composição (IMC)",
      tipo_efeito == "pct_efeito_taxa" ~ "Efeito Taxa (Mortalidade)"
    ),
    tipo_linha = case_when(
      tipo_efeito == "Efeito Composição (IMC)" ~ "Composição",
      tipo_efeito == "Efeito Taxa (Mortalidade)" ~ "Taxa"
    )
  )

plot_kitagawa_pct <- ggplot(kitagawa_pct_long, 
                            aes(x = faixa_idade, y = percentual, 
                                color = sexo_nome,
                                linetype = tipo_linha,
                                group = interaction(sexo_nome, tipo_efeito))) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", 
             size = 0.8, alpha = 0.8) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5, aes(shape = tipo_efeito)) +
  scale_color_manual(name = "Sexo:",
                     values = c("Masculino" = "#2B5B84", "Feminino" = "#C0392B")) +
  scale_linetype_manual(name = "Tipo de Linha:",
                        values = c("Composição" = "solid", "Taxa" = "dashed"),
                        labels = c("Composição" = "Composição (IMC)", 
                                   "Taxa" = "Taxa (Mortalidade)")) +
  scale_shape_manual(name = "Efeito:",
                     values = c("Efeito Composição (IMC)" = 17, 
                                "Efeito Taxa (Mortalidade)" = 16)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, big.mark = ".", scale = 1)) +
  labs(
    title = "Decomposição de Kitagawa: Contribuição Percentual dos Efeitos",
    subtitle = "Contribuição relativa na variação da TEM por grupo etário e sexo - Brasil, 2013-2019",
    x = "Grupo Etário",
    y = "Contribuição Relativa (%)",
    caption = "Fonte: Elaboração própria\nNota: Linhas contínuas = efeito composição (IMC); linhas tracejadas = efeito taxa (mortalidade)"
  ) +
  tema_abnt

print(plot_kitagawa_pct)


#---------  Gráfico Alternativo: Efeitos Percentuais ------------

# Gráfico mostrando os efeitos percentuais empilhados
plot_kitagawa_pct_barra <- ggplot(kitagawa_plot %>% 
                              select(faixa_idade, sexo_nome, pct_composicao, pct_taxa) %>%
                              pivot_longer(cols = c(pct_composicao, pct_taxa),
                                           names_to = "tipo_efeito",
                                           values_to = "percentual") %>%
                              mutate(tipo_efeito = case_when(
                                tipo_efeito == "pct_composicao" ~ "Efeito Composição (IMC)",
                                tipo_efeito == "pct_taxa" ~ "Efeito Taxa (Mortalidade)"
                              )),
                            aes(x = faixa_idade, y = percentual, fill = tipo_efeito)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black", size = 0.3) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  facet_wrap(~ sexo_nome, ncol = 1, scales = "free_y") +
  scale_fill_manual(name = "Tipo de Efeito:",
                    values = c("Efeito Composição (IMC)" = "#E67E22", 
                               "Efeito Taxa (Mortalidade)" = "#27AE60")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, big.mark = ".")) +
  labs(
    title = "Decomposição de Kitagawa: Efeitos Percentuais",
    subtitle = "Contribuição relativa dos efeitos composição e taxa - Brasil (2013-2019)",
    x = "Grupo Etário",
    y = "Contribuição Percentual (%)",
    caption = "Fonte: Elaboração própria"
  ) +
  tema_abnt

print(plot_kitagawa_pct_barra)

#---------  Salvar Gráficos ------------

# Criar diretório para gráficos
graficos_path <- paste0(base_path, "/analise_contrafactual/output/graficos")
dir.create(graficos_path, showWarnings = FALSE, recursive = TRUE)

# Salvar gráficos em PNG e PDF
ggsave(paste0(graficos_path, "/figura_kitagawa_efeitos_percentuais.png"),
       plot = plot_kitagawa_pct, width = 12, height = 7, dpi = 300)

ggsave(paste0(graficos_path, "/figura_kitagawa_efeitos_percentuais.pdf"),
       plot = plot_kitagawa_pct, width = 12, height = 7, device = "pdf")

ggsave(paste0(graficos_path, "/figura_kitagawa_efeitos_percentuais_barra.png"),
       plot = plot_kitagawa_pct_barra, width = 12, height = 7, dpi = 300)

ggsave(paste0(graficos_path, "/figura_kitagawa_efeitos_percentuais_barra.pdf"),
       plot = plot_kitagawa_pct_barra, width = 12, height = 7, device = "pdf")

cat("Gráficos da decomposição de Kitagawa (versão reformulada) salvos com sucesso em:", graficos_path, "\n")
