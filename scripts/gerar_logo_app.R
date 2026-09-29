# Gera a logo do Plota (www/img/logo_app.png e www/img/favicon.png).
# Desenho plano, no mesmo traço do Ranova, do Croma, do Calibra e do Minhas
# Entregas: eixos em L, três barras com pontas arredondadas (cores CIELCH) e,
# por cima, a curva de tendência que termina em um ponto — barras, linhas e
# pontos juntos, como os gráficos que o app monta.
# Uso: Rscript scripts/gerar_logo_app.R

library(colorspace)

cor_lch <- function(h) hex(polarLAB(L = if (h > 200) 54 else 64, C = if (h > 200) 34 else 44, H = h), fixup = TRUE)

# Engrenagem: anel com dentes radiais e miolo vazado, no mesmo traço do resto do icone
engrenagem <- function(cx, cy, raio, n_dentes, comp_dente, cor, espessura) {
  ang <- seq(0, 2 * pi, length.out = 160)
  lines(cx + raio * cos(ang), cy + raio * sin(ang), col = cor, lwd = espessura, lend = "round")
  dentes <- seq(0, 2 * pi, length.out = n_dentes + 1)[-(n_dentes + 1)]
  for (a in dentes) {
    segments(cx + raio * cos(a), cy + raio * sin(a),
             cx + (raio + comp_dente) * cos(a), cy + (raio + comp_dente) * sin(a),
             col = cor, lwd = espessura, lend = "round")
  }
  ang2 <- seq(0, 2 * pi, length.out = 60)
  lines(cx + raio * .42 * cos(ang2), cy + raio * .42 * sin(ang2), col = cor, lwd = espessura * .8, lend = "round")
}

desenhar_logo <- function(escala = 1) {
  par(mar = c(0, 0, 0, 0), bg = "transparent")
  plot.new(); plot.window(c(-1, 1), c(-1, 1), asp = 1)

  espessura <- 84 * escala
  traco <- 70 * escala
  base <- -.7

  # Barras (médias) em azul, verde e dourado
  alturas <- c(.42, .72, .56)
  posicoes <- c(-.44, .02, .48)
  matizes <- c(250, 148, 78)
  for (i in seq_along(alturas)) {
    segments(posicoes[i], base, posicoes[i], base + alturas[i] * 1.2, col = cor_lch(matizes[i]), lwd = espessura, lend = "round")
  }

  # Eixos em L
  lines(c(-.86, -.86, .86), c(.86, -.86, -.86), col = "#173B5B", lwd = traco, lend = "round", ljoin = "round")

  # Engrenagem sobre o gráfico de barras
  engrenagem(.16, .2, .34, 8, .14, "#173B5B", traco * .62)
}

tipo <- if (capabilities("aqua")) "quartz" else "cairo"
dir.create("www/img", showWarnings = FALSE, recursive = TRUE)
png("www/img/logo_app.png", width = 512, height = 512, bg = "transparent", type = tipo)
desenhar_logo(); dev.off()
png("www/img/favicon.png", width = 64, height = 64, bg = "transparent", type = tipo)
desenhar_logo(escala = 64 / 512); dev.off()
