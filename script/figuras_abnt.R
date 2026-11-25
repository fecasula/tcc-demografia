# A4 com margens laterais de 3 cm e 2 cm: 16 cm de largura útil.
tema_figura_abnt <- function() {
  ggplot2::theme_minimal(base_size = 12, base_family = "sans") +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "#17212B"),
      axis.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.text = ggplot2::element_text(size = 10, colour = "#26323D"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10, colour = "#26323D"),
      legend.key.width = grid::unit(0.8, "cm"),
      legend.spacing.x = grid::unit(0.15, "cm"),
      strip.text = ggplot2::element_text(size = 10.5, face = "bold"),
      strip.background = ggplot2::element_rect(fill = "#EDF1F4", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#DCE2E7", linewidth = 0.3),
      panel.spacing = grid::unit(0.55, "cm"),
      plot.margin = ggplot2::margin(5, 7, 3, 4)
    )
}

salvar_figura_abnt <- function(grafico, caminho, altura_cm) {
  # Título, nota e fonte ficam fora da imagem, no texto do documento.
  miolo <- grafico + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  ggplot2::ggsave(
    paste0(caminho, ".pdf"), miolo, device = grDevices::cairo_pdf,
    width = 16, height = altura_cm, units = "cm", bg = "white"
  )
  ggplot2::ggsave(
    paste0(caminho, ".png"), miolo,
    width = 16, height = altura_cm, units = "cm", dpi = 600, bg = "white"
  )
}

cores_pesquisas_abnt <- c(
  "POF 2002-2003" = "#185775",
  "POF 2008-2009" = "#16734D",
  "PNS 2013" = "#9C5300",
  "PNS 2019" = "#91234F"
)

formas_pesquisas_abnt <- c(
  "POF 2002-2003" = 16,
  "POF 2008-2009" = 17,
  "PNS 2013" = 15,
  "PNS 2019" = 18
)

salvar_conjunto_abnt <- function(graficos, pasta, alturas_cm) {
  for (i in seq_along(graficos)) {
    salvar_figura_abnt(
      graficos[[i]], file.path(pasta, names(graficos)[i]), alturas_cm[i]
    )
  }
}
