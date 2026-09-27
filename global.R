# Funções e dependências compartilhadas pela aplicação

library(shiny)
library(ggplot2)
library(rhandsontable)
library(patchwork)
library(showtext)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Versão lida do DESCRIPTION (fonte única; atualize lá e no CHANGELOG.md).
VERSAO_APP <- tryCatch(unname(read.dcf("DESCRIPTION", fields = "Version")[1, 1]), error = function(e) "dev")

CORES_APP <- list(
  navy = "#173B5B", blue = "#2A5C92", ink = "#263B4D", muted = "#627589",
  line = "#D9E3EB", canvas = "#F4F7FA", soft = "#EAF2FA", green = "#4D965D",
  red = "#B94B4B", gold = "#C0924A"
)

TEXTO_PRIVACIDADE <- paste(
  "Os dados ficam apenas na sessão aberta do navegador e são usados somente para desenhar os gráficos.",
  "Nada é gravado em banco de dados, arquivos ou cookies; ao fechar ou atualizar a página, as informações são descartadas."
)

# ---------------------------------------------------------
# Fontes (arquivos livres em fonts/, iguais na tela, no PNG, no PDF e no SVG)
# ---------------------------------------------------------

FONTES <- list(
  arial   = list(nome = "Arial (Liberation Sans)", arquivos = "LiberationSans"),
  times   = list(nome = "Times New Roman (Liberation Serif)", arquivos = "LiberationSerif"),
  ptsans  = list(nome = "PT Sans", arquivos = "PT_Sans-Web"),
  poppins = list(nome = "Poppins", arquivos = "Poppins"),
  crimson = list(nome = "Crimson Text", arquivos = "CrimsonText")
)

registrar_fontes <- function(pasta = "fonts") {
  for (familia in names(FONTES)) {
    base <- file.path(pasta, FONTES[[familia]]$arquivos)
    arquivo <- function(estilo) paste0(base, "-", estilo, ".ttf")
    if (!file.exists(arquivo("Regular"))) next
    sysfonts::font_add(familia, regular = arquivo("Regular"), bold = arquivo("Bold"),
                       italic = arquivo("Italic"), bolditalic = arquivo("BoldItalic"))
  }
}
registrar_fontes()

OPCOES_FONTES <- stats::setNames(names(FONTES), vapply(FONTES, `[[`, character(1), "nome"))

fonte_valida <- function(fonte) if (!is.null(fonte) && fonte %in% sysfonts::font_families()) fonte else "sans"

# ---------------------------------------------------------
# Componentes de interface
# ---------------------------------------------------------

# Cartão numerado (passo a passo), o mesmo do Ranova e do Calibra.
cartao <- function(numero = NULL, titulo, ..., icone = NULL, acoes = NULL, classe = NULL) {
  div(
    class = paste("painel cartao", classe),
    div(
      class = "cabecalho-secao",
      h4(class = "titulo-cartao",
         if (!is.null(numero)) tags$span(class = "numero-passo", numero) else if (!is.null(icone)) icon(icone),
         titulo),
      if (!is.null(acoes)) div(class = "acoes-cartao", acoes)
    ),
    ...
  )
}

# Seção recolhível (mesmo componente dos painéis de aparência do Ranova).
recolhivel <- function(titulo, descricao, icone, ..., aberto = FALSE) {
  tags$details(
    class = "recolhivel", open = if (aberto) NA else NULL,
    tags$summary(
      tags$span(class = "recolhivel-icone", icon(icone)),
      tags$span(class = "recolhivel-texto", tags$b(titulo), tags$span(descricao)),
      tags$span(class = "recolhivel-seta")
    ),
    div(class = "recolhivel-corpo", ...)
  )
}

# ---------------------------------------------------------
# Leitura e conversão de dados
# ---------------------------------------------------------

texto_vazio <- function(x) is.na(x) | trimws(as.character(x)) == ""

# Aceita vírgula ou ponto decimal ("0,125", "0.125", "1.250,5").
converter_numero <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  texto <- gsub("\\s+", "", as.character(x))
  texto[is.na(texto) | texto == ""] <- NA_character_
  com_virgula <- !is.na(texto) & grepl(",", texto, fixed = TRUE)
  texto[com_virgula] <- sub(",", ".", gsub(".", "", texto[com_virgula], fixed = TRUE), fixed = TRUE)
  suppressWarnings(as.numeric(texto))
}

# Coluna numérica: todos os valores preenchidos viram número.
coluna_numerica <- function(x) {
  preenchidos <- !texto_vazio(x)
  any(preenchidos) && all(!is.na(converter_numero(x[preenchidos])))
}

tipos_colunas <- function(dados) {
  vapply(dados, function(coluna) if (coluna_numerica(coluna)) "numérica" else "texto", character(1))
}

# Remove linhas e colunas totalmente vazias e dá nome às colunas sem título.
limpar_tabela <- function(dados) {
  dados <- as.data.frame(dados, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(dados) == 0 || nrow(dados) == 0) return(dados)
  nomes <- trimws(names(dados))
  nomes[is.na(nomes) | nomes == "" | grepl("^\\.\\.\\.[0-9]+$", nomes)] <- paste0("Coluna ", which(is.na(nomes) | nomes == "" | grepl("^\\.\\.\\.[0-9]+$", nomes)))
  names(dados) <- make.unique(nomes, sep = " ")
  dados[] <- lapply(dados, function(coluna) { coluna <- as.character(coluna); coluna[is.na(coluna)] <- ""; trimws(coluna) })
  manter_col <- vapply(dados, function(coluna) any(!texto_vazio(coluna)), logical(1))
  dados <- dados[, manter_col, drop = FALSE]
  if (ncol(dados) == 0) return(dados)
  manter_lin <- apply(dados, 1, function(linha) any(!texto_vazio(linha)))
  dados <- dados[manter_lin, , drop = FALSE]
  rownames(dados) <- NULL
  dados
}

# Texto colado do Excel: uma linha por observação, colunas separadas por tabulação,
# ponto e vírgula ou dois ou mais espaços. A primeira linha traz os nomes.
ler_texto_colado <- function(texto) {
  linhas <- strsplit(gsub("\r", "", texto %||% ""), "\n", fixed = TRUE)[[1]]
  linhas <- linhas[trimws(linhas) != ""]
  if (length(linhas) < 2) stop("Cole a linha de títulos e pelo menos uma linha de dados.")
  separador <- if (any(grepl("\t", linhas))) "\t" else if (any(grepl(";", linhas))) ";" else " {2,}"
  partes <- lapply(linhas, function(l) trimws(strsplit(l, separador, perl = TRUE)[[1]]))
  n_col <- max(lengths(partes))
  matriz <- do.call(rbind, lapply(partes, function(p) c(p, rep("", n_col - length(p)))))
  dados <- as.data.frame(matriz[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(dados) <- matriz[1, ]
  limpar_tabela(dados)
}

ler_arquivo_dados <- function(caminho, nome_arquivo, aba = NULL) {
  extensao <- tolower(tools::file_ext(nome_arquivo))
  dados <- switch(
    extensao,
    xlsx = , xls = as.data.frame(readxl::read_excel(caminho, sheet = aba %||% 1, col_types = "text"), check.names = FALSE),
    csv = , txt = , tsv = {
      primeira <- readLines(caminho, n = 1, warn = FALSE, encoding = "UTF-8")
      contagem <- vapply(c(";", ",", "\t"), function(sep) lengths(regmatches(primeira, gregexpr(sep, primeira, fixed = TRUE))), numeric(1))
      utils::read.table(caminho, header = TRUE, sep = names(which.max(contagem)), quote = "\"", dec = ".",
                        colClasses = "character", check.names = FALSE, fill = TRUE, encoding = "UTF-8", comment.char = "")
    },
    stop("Formato não suportado. Envie um arquivo .xlsx, .xls ou .csv.")
  )
  limpar_tabela(dados)
}

# ---------------------------------------------------------
# Conjuntos de exemplo (experimentos agronômicos típicos)
# ---------------------------------------------------------

formatar_br <- function(x, digitos) formatC(x, format = "f", digits = digitos, decimal.mark = ",")

exemplo_cultivares <- function() {
  set.seed(21)
  cultivares <- c("BRS Anauê", "Gold Mine", "Iracema", "Natal")
  doses <- c(0, 50, 100, 150, 200)
  base <- expand.grid(Bloco = 1:4, Dose = doses, Cultivar = cultivares, stringsAsFactors = FALSE)
  efeito_cult <- c(0, 2.4, 1.1, -1.3)[match(base$Cultivar, cultivares)]
  produtividade <- 18 + efeito_cult + 0.19 * base$Dose - 0.00062 * base$Dose^2 + stats::rnorm(nrow(base), 0, 1.6)
  brix <- 9.2 + c(0, 1.4, 0.8, -0.5)[match(base$Cultivar, cultivares)] + 0.006 * base$Dose + stats::rnorm(nrow(base), 0, 0.45)
  letras <- c("BRS Anauê" = "b", "Gold Mine" = "a", "Iracema" = "ab", "Natal" = "c")
  data.frame(
    Cultivar = base$Cultivar,
    "Dose de N (kg ha^-1)" = as.character(base$Dose),
    Bloco = as.character(base$Bloco),
    "Produtividade (t ha^-1)" = formatar_br(produtividade, 2),
    "Sólidos solúveis (°Brix)" = formatar_br(brix, 1),
    Letras = unname(letras[base$Cultivar]),
    check.names = FALSE, stringsAsFactors = FALSE
  )[order(base$Cultivar, base$Dose, base$Bloco), ]
}

exemplo_crescimento <- function() {
  set.seed(7)
  dias <- seq(15, 90, by = 15)
  base <- expand.grid(Rep = 1:4, DAS = dias, Manejo = c("Irrigado", "Sequeiro"), stringsAsFactors = FALSE)
  assintota <- ifelse(base$Manejo == "Irrigado", 92, 64)
  massa <- assintota / (1 + exp(-0.09 * (base$DAS - 48))) + stats::rnorm(nrow(base), 0, 2.8)
  area <- ifelse(base$Manejo == "Irrigado", 1, 0.74) * (0.9 * base$DAS - 0.0068 * base$DAS^2) + stats::rnorm(nrow(base), 0, 1.5)
  data.frame(
    Manejo = base$Manejo,
    DAS = as.character(base$DAS),
    "Repetição" = as.character(base$Rep),
    "Massa seca (g planta^-1)" = formatar_br(pmax(massa, 0.5), 1),
    "Índice de área foliar" = formatar_br(pmax(area / 10, 0.1), 2),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

EXEMPLOS <- list(
  "Cultivares × doses de N" = exemplo_cultivares,
  "Crescimento ao longo do ciclo" = exemplo_crescimento
)

# ---------------------------------------------------------
# Especificação de um gráfico
# ---------------------------------------------------------

TIPOS_GRAFICO <- list(
  barras       = list(nome = "Barras", icone = "chart-column"),
  barras_h     = list(nome = "Barras horizontais", icone = "chart-bar"),
  empilhadas   = list(nome = "Empilhadas", icone = "layer-group"),
  linhas       = list(nome = "Linhas", icone = "chart-line"),
  pontos       = list(nome = "Médias e erro", icone = "grip-lines-vertical"),
  dispersao    = list(nome = "Dispersão e regressão", icone = "braille"),
  boxplot      = list(nome = "Boxplot", icone = "box"),
  violino      = list(nome = "Violino", icone = "guitar"),
  histograma   = list(nome = "Histograma", icone = "chart-simple"),
  densidade    = list(nome = "Densidade", icone = "mountain")
)

# Tipos que resumem os dados (média, mediana ou soma) antes de desenhar.
TIPOS_RESUMO <- c("barras", "barras_h", "empilhadas", "linhas", "pontos")
TIPOS_BARRA <- c("barras", "barras_h", "empilhadas")
TIPOS_SO_Y <- c("histograma", "densidade")

PADRAO_GRAFICO <- list(
  nome = "Gráfico", dados = "", tipo = "barras", x = "", y = "", grupo = "", faceta = "",
  estatistica = "media", erro = "ep", erro_lado = "ambos", rotulo = "nenhum", col_letras = "",
  empilhar = "valores", regressao = "nenhuma", equacao = "grafico", ponto_otimo = FALSE, observacoes = FALSE,
  bins = 15, paleta = "plota", colorir_x = FALSE, cor_unica = "#2A5C92", cores = list(), cores_paleta = "",
  contorno = "#173B5B", contorno_largura = 0.4, sem_contorno = FALSE, largura_barra = 0.7,
  opacidade = 1, tamanho_ponto = 2.6, espessura_linha = 0.8, formas = TRUE, tipos_linha = FALSE,
  erro_cor = "#263B4D", erro_largura = 0.5, erro_traco = 0.2,
  titulo = "", subtitulo = "", rodape = "", titulo_x = "", titulo_y = "", titulo_legenda = "",
  sem_titulo_legenda = FALSE, fonte = "arial", tamanho = 12, cor_texto = "#263B4D",
  negrito_eixos = TRUE, tamanho_rotulos = 1,
  tema = "publicacao", grade = "nenhuma", marcas_dentro = FALSE, legenda = "direita", girar_x = "0",
  y_min = NA, y_max = NA, y_zero = TRUE, decimais = NA, virgula = TRUE,
  escala_facetas = "fixed", ncol_facetas = NA
)

novo_grafico <- function(nome, nome_dados, dados) {
  spec <- PADRAO_GRAFICO
  spec$nome <- nome
  spec$dados <- nome_dados
  if (!is.null(dados) && ncol(dados) > 0) {
    tipos <- tipos_colunas(dados)
    categoricas <- names(tipos)[tipos == "texto"]
    numericas <- names(tipos)[tipos == "numérica"]
    spec$x <- categoricas[1] %||% names(dados)[1]
    spec$y <- rev(numericas)[1] %||% names(dados)[ncol(dados)]
    if (identical(spec$y, spec$x) && length(numericas) > 1) spec$y <- numericas[1]
  }
  spec
}

completar_spec <- function(spec) utils::modifyList(PADRAO_GRAFICO, spec %||% list())

# ---------------------------------------------------------
# Paletas
# ---------------------------------------------------------

PALETAS <- list(
  plota     = list(nome = "Plota (azul, verde, dourado)", cores = c("#2A5C92", "#4D965D", "#C0924A", "#B94B4B", "#173B5B", "#7FA7CF", "#8BC39A", "#627589")),
  campo     = list(nome = "Campo (verdes e terra)", cores = c("#1F4D2B", "#4D965D", "#9BC27F", "#C9A227", "#8C5A2B", "#5B3A1E", "#B5DCB9", "#E8D3A8")),
  cinza     = list(nome = "Tons de cinza (impressão)", rampa = c("#1F1F1F", "#BDBDBD")),
  azul      = list(nome = "Tons de azul", rampa = c("#173B5B", "#2A5C92", "#9DC0E3")),
  verde     = list(nome = "Tons de verde", rampa = c("#1F4D2B", "#4D965D", "#B5DCB9")),
  terra     = list(nome = "Terra", rampa = c("#5B3A1E", "#C0924A", "#E8D3A8")),
  contraste = list(nome = "Okabe-Ito (daltônicos)", cores = c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#000000")),
  viridis   = list(nome = "Viridis", hcl = "viridis"),
  pastel    = list(nome = "Pastel", cores = c("#8DB3D9", "#A8D5A2", "#F2C98A", "#E8A1A1", "#C3B1E1", "#9FD8D6", "#F5E29B", "#C8C8C8"))
)

OPCOES_PALETAS <- stats::setNames(names(PALETAS), vapply(PALETAS, `[[`, character(1), "nome"))

cores_paleta <- function(paleta, n) {
  p <- PALETAS[[if (paleta %in% names(PALETAS)) paleta else "plota"]]
  if (n < 1) return(character(0))
  if (!is.null(p$cores)) return(rep(p$cores, length.out = n))
  if (!is.null(p$hcl)) return(grDevices::hcl.colors(n, p$hcl))
  if (n == 1) return(p$rampa[1])
  grDevices::colorRampPalette(p$rampa)(n)
}

cor_valida <- function(cor) is.character(cor) && length(cor) == 1 && grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", cor)

# Cores dos níveis: as escolhidas pelo usuário (por nome do nível) ou as da paleta.
cores_niveis <- function(niveis, spec) {
  base <- stats::setNames(cores_paleta(spec$paleta, length(niveis)), niveis)
  for (nivel in intersect(niveis, names(spec$cores))) {
    if (cor_valida(spec$cores[[nivel]])) base[[nivel]] <- spec$cores[[nivel]]
  }
  base
}

FORMAS <- c(16, 17, 15, 18, 1, 2, 0, 5, 8, 4)
TIPOS_LINHA <- c("solid", "22", "42", "13", "1343", "73", "2262", "12223242")

# ---------------------------------------------------------
# Textos: expoentes e índices digitados como ^-1, ^2, _2
# ---------------------------------------------------------

# Um texto com marcas vira expressão plotmath: "t ha^-1" -> t ha⁻¹ com expoente real,
# "CO_2" -> CO₂. Assim o sinal de menos e os números saem em qualquer fonte.
PADRAO_MARCAS <- "\\^(\\{[^}]*\\}|[-+\u2212]?[0-9]+([.,][0-9]+)?)|_(\\{[^}]*\\}|[0-9]+)"

tem_marcas <- function(texto) grepl(PADRAO_MARCAS, texto, perl = TRUE)

aspas <- function(x) paste0('"', gsub('(["\\\\])', "\\\\\\1", x), '"')

texto_para_expressao <- function(texto) {
  achados <- gregexpr(PADRAO_MARCAS, texto, perl = TRUE)[[1]]
  inicios <- as.integer(achados); fins <- inicios + attr(achados, "match.length") - 1
  partes <- character(0); posicao <- 1
  for (i in seq_along(inicios)) {
    antes <- substr(texto, posicao, inicios[i] - 1)
    marca <- substr(texto, inicios[i], inicios[i])
    corpo <- gsub("^\\{|\\}$", "", substr(texto, inicios[i] + 1, fins[i]))
    corpo <- sub("^-", "\u2212", corpo)
    base <- if (nzchar(antes)) aspas(antes) else '""'
    partes <- c(partes, if (marca == "^") paste0(base, "^", aspas(corpo)) else paste0(base, "[", aspas(corpo), "]"))
    posicao <- fins[i] + 1
  }
  resto <- substr(texto, posicao, nchar(texto))
  if (nzchar(resto)) partes <- c(partes, aspas(resto))
  tryCatch(parse(text = paste(partes, collapse = "*"))[[1]], error = function(e) texto)
}

# Para títulos e rótulos de eixos/legendas (aceita expressões).
formatar_texto <- function(texto) {
  if (is.null(texto) || length(texto) == 0) return(texto)
  texto <- as.character(texto)
  if (!any(tem_marcas(texto))) return(texto)
  if (length(texto) == 1) return(texto_para_expressao(texto))
  as.expression(lapply(texto, function(t) if (tem_marcas(t)) texto_para_expressao(t) else t))
}

# Para onde só cabe texto simples (faixas de painéis, equações): remove as marcas.
texto_simples <- function(texto) {
  if (is.null(texto)) return(texto)
  texto <- gsub("\\^\\{?([-+]?[0-9.,]+)\\}?", "\\1", as.character(texto), perl = TRUE)
  texto <- gsub("\\^\\{([^}]*)\\}", "\\1", texto, perl = TRUE)
  texto <- gsub("_\\{([^}]*)\\}", "\\1", texto, perl = TRUE)
  gsub("_([0-9]+)", "\\1", texto, perl = TRUE)
}

num_br <- function(x, digitos = 2) {
  texto <- formatC(x, format = "f", digits = digitos, decimal.mark = ",", big.mark = ".")
  sub("^-(0(,0+)?)$", "\\1", texto)
}

# Coeficiente com 4 algarismos significativos, sem notação científica.
coef_br <- function(x) {
  texto <- formatC(signif(x, 4), format = "fg", digits = 4, decimal.mark = ",", big.mark = "")
  sub("[,]$", "", trimws(texto))
}

# ---------------------------------------------------------
# Preparação e resumo dos dados
# ---------------------------------------------------------

coluna_existe <- function(dados, coluna) !is.null(coluna) && nzchar(coluna) && coluna %in% names(dados)

# Fator com os níveis na ordem em que aparecem (ou numérica, se todos forem números).
como_fator <- function(x) {
  x <- trimws(as.character(x))
  numeros <- converter_numero(x)
  if (all(!is.na(numeros[x != ""]))) {
    niveis <- unique(x[order(numeros)])
  } else {
    niveis <- unique(x)
  }
  factor(x, levels = niveis[niveis != ""])
}

preparar_dados <- function(dados, spec) {
  if (is.null(dados) || nrow(dados) == 0) stop("O conjunto de dados está vazio.")
  if (!coluna_existe(dados, spec$y)) stop("Escolha a variável numérica (eixo Y).")
  y <- converter_numero(dados[[spec$y]])
  if (all(is.na(y))) stop(sprintf("A coluna \"%s\" não tem valores numéricos.", spec$y))
  saida <- data.frame(y = y)
  so_y <- spec$tipo %in% TIPOS_SO_Y
  if (!so_y) {
    if (!coluna_existe(dados, spec$x)) stop("Escolha a variável do eixo X.")
    if (identical(spec$x, spec$y)) stop("Escolha variáveis diferentes para os eixos X e Y.")
    x_bruto <- dados[[spec$x]]
    x_num <- converter_numero(x_bruto)
    saida$x_num <- x_num
    saida$x <- como_fator(x_bruto)
  }
  saida$grupo <- if (coluna_existe(dados, spec$grupo) && !identical(spec$grupo, spec$x)) como_fator(dados[[spec$grupo]]) else factor("Todos")
  saida$faceta <- if (coluna_existe(dados, spec$faceta)) como_fator(dados[[spec$faceta]]) else factor("Todos")
  saida$letra <- if (coluna_existe(dados, spec$col_letras)) trimws(as.character(dados[[spec$col_letras]])) else ""
  saida <- saida[!is.na(saida$y) & !is.na(saida$grupo) & !is.na(saida$faceta), , drop = FALSE]
  if (!so_y) saida <- saida[!is.na(saida$x), , drop = FALSE]
  if (nrow(saida) == 0) stop("Não há linhas completas para as variáveis escolhidas.")
  saida
}

tem_grupo <- function(d) nlevels(droplevels(d$grupo)) > 1 || !identical(levels(d$grupo), "Todos")
tem_faceta <- function(d) !identical(levels(d$faceta), "Todos")

x_numerico <- function(d) !is.null(d$x_num) && all(!is.na(d$x_num))

# Resumo por X × grupo × faceta: média (ou mediana/soma), erro e letra informada.
resumir <- function(d, spec) {
  chaves <- interaction(d$x, d$grupo, d$faceta, drop = TRUE, lex.order = TRUE)
  partes <- split(d, chaves)
  linhas <- lapply(partes, function(p) {
    v <- p$y
    n <- length(v)
    centro <- switch(spec$estatistica, mediana = stats::median(v), soma = sum(v), mean(v))
    desvio <- if (n > 1) stats::sd(v) else 0
    erro <- switch(
      spec$erro,
      ep = desvio / sqrt(n),
      dp = desvio,
      ic95 = if (n > 1) stats::qt(0.975, n - 1) * desvio / sqrt(n) else 0,
      0
    )
    if (spec$estatistica == "soma") erro <- 0
    letras <- p$letra[nzchar(p$letra)]
    data.frame(x = p$x[1], x_num = if (!is.null(p$x_num)) p$x_num[1] else NA_real_, grupo = p$grupo[1],
               faceta = p$faceta[1], valor = centro, erro = erro, n = n,
               letra = if (length(letras)) letras[1] else "", stringsAsFactors = FALSE)
  })
  resumo <- do.call(rbind, linhas)
  resumo$x <- factor(resumo$x, levels = levels(d$x))
  resumo$grupo <- factor(resumo$grupo, levels = levels(d$grupo))
  resumo$faceta <- factor(resumo$faceta, levels = levels(d$faceta))
  resumo$inf <- resumo$valor - resumo$erro
  resumo$sup <- resumo$valor + resumo$erro
  if (identical(spec$erro_lado, "superior")) resumo$inf <- resumo$valor
  rownames(resumo) <- NULL
  resumo
}

# ---------------------------------------------------------
# Regressão (linear, quadrática, cúbica, logarítmica)
# ---------------------------------------------------------

REGRESSOES <- c("Nenhuma" = "nenhuma", "Linear" = "linear", "Quadrática" = "quadratica",
                "Cúbica" = "cubica", "Logarítmica" = "log", "Suavizada (LOESS)" = "loess")

ajustar_regressao <- function(x, y, tipo) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  grau <- switch(tipo, linear = 1, quadratica = 2, cubica = 3, log = 1, loess = 2, 1)
  if (length(unique(x)) <= grau || (tipo == "log" && any(x <= 0))) return(NULL)
  grade <- seq(min(x), max(x), length.out = 120)
  if (tipo == "loess") {
    if (length(unique(x)) < 4) return(NULL)
    modelo <- suppressWarnings(stats::loess(y ~ x, span = 0.9))
    return(list(curva = data.frame(x = grade, y = stats::predict(modelo, data.frame(x = grade))), equacao = NULL))
  }
  if (tipo == "log") {
    modelo <- stats::lm(y ~ log(x))
  } else {
    modelo <- stats::lm(y ~ stats::poly(x, grau, raw = TRUE))
  }
  coefs <- unname(stats::coef(modelo))
  if (any(is.na(coefs))) return(NULL)
  r2 <- summary(modelo)$r.squared
  termo <- function(valor, sufixo, primeiro = FALSE) {
    sinal <- if (valor < 0) "−" else "+"
    corpo <- paste0(coef_br(abs(valor)), sufixo)
    if (primeiro) paste0(if (valor < 0) "−" else "", corpo) else paste(sinal, corpo)
  }
  sufixos <- if (tipo == "log") c("", "ln(x)") else c("", "x", "x²", "x³")[seq_along(coefs)]
  partes <- vapply(seq_along(coefs), function(i) termo(coefs[i], sufixos[i], i == 1), character(1))
  equacao <- paste0("ŷ = ", paste(partes, collapse = " "), "   R² = ", num_br(r2, 2))
  otimo <- NULL
  if (tipo == "quadratica" && coefs[3] != 0) {
    xv <- -coefs[2] / (2 * coefs[3])
    if (xv >= min(x) && xv <= max(x)) {
      otimo <- list(x = xv, y = coefs[1] + coefs[2] * xv + coefs[3] * xv^2, maximo = coefs[3] < 0)
    }
  }
  curva <- data.frame(x = grade, y = stats::predict(modelo, data.frame(x = grade)))
  list(curva = curva, equacao = equacao, r2 = r2, otimo = otimo)
}

# ---------------------------------------------------------
# Tema
# ---------------------------------------------------------

TEMAS <- c("Publicação (eixos em L)" = "publicacao", "Com moldura" = "moldura",
           "Mínimo (sem eixos)" = "minimo", "Moderno (fundo suave)" = "moderno")

LEGENDAS <- c("À direita" = "direita", "Abaixo" = "abaixo", "Acima" = "acima", "À esquerda" = "esquerda",
              "Dentro, canto superior direito" = "dentro_dir", "Dentro, canto superior esquerdo" = "dentro_esq",
              "Sem legenda" = "nenhuma")

GGPLOT_35 <- utils::packageVersion("ggplot2") >= "3.5.0"

tema_plota <- function(spec) {
  base_size <- max(5, min(40, as.numeric(spec$tamanho %||% 12)))
  familia <- fonte_valida(spec$fonte)
  tinta <- if (cor_valida(spec$cor_texto)) spec$cor_texto else CORES_APP$ink
  linha_eixo <- element_line(colour = tinta, linewidth = 0.45)
  base <- switch(
    spec$tema,
    moldura = theme_bw(base_size, base_family = familia) +
      theme(panel.border = element_rect(colour = tinta, linewidth = 0.6, fill = NA)),
    minimo = theme_minimal(base_size, base_family = familia) + theme(axis.ticks = element_blank()),
    moderno = theme_minimal(base_size, base_family = familia) +
      theme(panel.background = element_rect(fill = "#F2F5F8", colour = NA), axis.ticks = element_blank()),
    theme_classic(base_size, base_family = familia) + theme(axis.line = linha_eixo)
  )
  grade_cor <- if (identical(spec$tema, "moderno")) "#FFFFFF" else "#E3E9EF"
  grade <- switch(
    spec$grade,
    y = theme(panel.grid.major.y = element_line(colour = grade_cor, linewidth = 0.45), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()),
    xy = theme(panel.grid.major = element_line(colour = grade_cor, linewidth = 0.45), panel.grid.minor = element_blank()),
    theme(panel.grid = element_blank())
  )
  posicao <- spec$legenda %||% "direita"
  legenda <- switch(
    posicao,
    abaixo = theme(legend.position = "bottom"),
    acima = theme(legend.position = "top"),
    esquerda = theme(legend.position = "left"),
    nenhuma = theme(legend.position = "none"),
    dentro_dir = , dentro_esq = {
      canto <- if (posicao == "dentro_dir") c(0.98, 0.98) else c(0.02, 0.98)
      if (GGPLOT_35) theme(legend.position = "inside", legend.position.inside = canto, legend.justification = canto)
      else theme(legend.position = canto, legend.justification = canto)
    },
    theme(legend.position = "right")
  )
  angulo <- as.numeric(spec$girar_x %||% 0)
  texto_x <- if (angulo == 0) element_text(colour = tinta) else element_text(colour = tinta, angle = angulo, hjust = 1, vjust = if (angulo == 90) 0.5 else 1)
  negrito <- if (isTRUE(spec$negrito_eixos)) "bold" else "plain"
  marcas <- if (isTRUE(spec$marcas_dentro)) {
    theme(axis.ticks.length = unit(-0.16, "cm"),
          axis.text.x = element_text(margin = margin(t = 6)), axis.text.y = element_text(margin = margin(r = 6)))
  } else {
    theme(axis.ticks.length = unit(0.14, "cm"))
  }
  base + grade + legenda + marcas +
    theme(
      text = element_text(colour = tinta),
      axis.title = element_text(face = negrito, colour = tinta),
      axis.text = element_text(colour = tinta, size = rel(0.9)),
      axis.text.x = texto_x,
      axis.ticks = if (identical(spec$tema, "minimo") || identical(spec$tema, "moderno")) element_blank() else element_line(colour = tinta, linewidth = 0.4),
      legend.title = element_text(face = negrito, size = rel(0.92)),
      legend.text = element_text(size = rel(0.88)),
      legend.key = element_rect(fill = NA, colour = NA),
      legend.background = element_rect(fill = NA, colour = NA),
      plot.title = element_text(face = "bold", size = rel(1.25), colour = tinta, margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.95), colour = CORES_APP$muted, margin = margin(b = 8)),
      plot.caption = element_text(size = rel(0.75), colour = CORES_APP$muted, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = rel(0.95), colour = tinta),
      plot.margin = margin(10, 14, 8, 8)
    )
}

rotulos_numero <- function(spec, percentual = FALSE) {
  casas <- suppressWarnings(as.integer(spec$decimais))
  precisao <- if (length(casas) == 1 && !is.na(casas)) 10^-casas else NULL
  decimal <- if (isTRUE(spec$virgula)) "," else "."
  milhar <- if (isTRUE(spec$virgula)) "." else ","
  if (percentual) return(scales::label_percent(accuracy = precisao %||% 1, decimal.mark = decimal, big.mark = milhar))
  scales::label_number(accuracy = precisao, decimal.mark = decimal, big.mark = milhar)
}

# ---------------------------------------------------------
# Construção do gráfico
# ---------------------------------------------------------

# Título (com expoentes, se houver). O plotmath ignora o negrito do tema: é aplicado aqui.
titulo_eixo <- function(digitado, padrao, negrito = FALSE) {
  digitado <- trimws(digitado %||% "")
  texto <- formatar_texto(if (nzchar(digitado)) digitado else padrao)
  if (negrito && is.language(texto)) texto <- bquote(bold(.(texto)))
  texto
}

construir_grafico <- function(spec, dados) {
  s <- completar_spec(spec)
  d <- preparar_dados(dados, s)
  agrupado <- tem_grupo(d)
  facetado <- tem_faceta(d)
  familia <- fonte_valida(s$fonte)
  tinta <- if (cor_valida(s$cor_texto)) s$cor_texto else CORES_APP$ink
  base_size <- max(5, min(40, as.numeric(s$tamanho %||% 12)))
  tamanho_rotulo <- base_size * as.numeric(s$tamanho_rotulos %||% 1) * 0.85 / .pt
  contorno <- if (isTRUE(s$sem_contorno) || !cor_valida(s$contorno)) NA else s$contorno
  opacidade <- max(0.05, min(1, as.numeric(s$opacidade %||% 1)))
  erro_cor <- if (cor_valida(s$erro_cor)) s$erro_cor else tinta
  cor_unica <- if (cor_valida(s$cor_unica)) s$cor_unica else CORES_APP$blue

  # Variável que define as cores: grupo, o próprio X (se pedido) ou nenhuma.
  var_cor <- if (agrupado) "grupo" else if (isTRUE(s$colorir_x) && !(s$tipo %in% TIPOS_SO_Y)) "x" else NULL
  niveis_cor <- if (!is.null(var_cor)) levels(droplevels(d[[var_cor]])) else character(0)
  cores <- cores_niveis(niveis_cor, s)
  legenda_cor <- if (agrupado) {
    if (isTRUE(s$sem_titulo_legenda)) NULL else titulo_eixo(s$titulo_legenda, s$grupo, isTRUE(s$negrito_eixos))
  } else NULL
  rotulos_legenda <- formatar_texto(niveis_cor)

  largura <- max(0.1, min(1, as.numeric(s$largura_barra %||% 0.7)))
  desvio <- position_dodge(width = largura)
  y_numero <- rotulos_numero(s)
  camadas <- list()
  escala_y <- NULL
  titulo_y_padrao <- s$y
  titulo_x_padrao <- s$x
  virar <- FALSE
  limite_zero <- FALSE
  eq_textos <- NULL

  aes_cor <- function(tipo = c("fill", "colour")) {
    tipo <- match.arg(tipo)
    if (is.null(var_cor)) return(aes())
    if (tipo == "fill") aes(fill = .data[[var_cor]]) else aes(colour = .data[[var_cor]])
  }

  if (s$tipo %in% TIPOS_RESUMO) {
    r <- resumir(d, s)
    if (s$tipo == "empilhadas") {
      if (identical(s$empilhar, "percentual")) {
        totais <- stats::ave(r$valor, r$x, r$faceta, FUN = sum)
        r$valor <- ifelse(totais == 0, 0, r$valor / totais)
      }
      r <- r[order(r$x, r$faceta, -as.integer(r$grupo)), ]
      r$sup <- stats::ave(r$valor, r$x, r$faceta, FUN = cumsum)
      r$centro <- r$sup - r$valor / 2
      camadas <- c(camadas, list(
        camada(geom_col, data = r, aes(x = .data$x, y = .data$valor, !!!aes_cor("fill")), width = largura,
                 fill = if (is.null(var_cor)) cor_unica else NULL, colour = contorno,
                 linewidth = s$contorno_largura, alpha = opacidade, position = position_stack(reverse = FALSE))
      ))
      if (identical(s$rotulo, "valor")) {
        camadas <- c(camadas, list(geom_text(
          data = r, aes(x = .data$x, y = .data$centro,
                        label = if (identical(s$empilhar, "percentual")) scales::label_percent(accuracy = 1, decimal.mark = ",")(.data$valor) else y_numero(.data$valor)),
          size = tamanho_rotulo * 0.9, family = familia, colour = "white", fontface = "bold")))
      }
      if (identical(s$empilhar, "percentual")) {
        escala_y <- scale_y_continuous(labels = rotulos_numero(s, percentual = TRUE), expand = expansion(mult = c(0, 0.02)))
      }
      limite_zero <- TRUE
      topo_rotulos <- r$sup
    } else if (s$tipo %in% c("barras", "barras_h")) {
      camadas <- c(camadas, list(
        camada(geom_col, data = r, aes(x = .data$x, y = .data$valor, !!!aes_cor("fill"), group = .data$grupo),
                 position = desvio, width = largura * if (agrupado) 0.94 else 1,
                 fill = if (is.null(var_cor)) cor_unica else NULL,
                 colour = contorno, linewidth = s$contorno_largura, alpha = opacidade)
      ))
      virar <- identical(s$tipo, "barras_h")
      limite_zero <- isTRUE(s$y_zero)
      topo_rotulos <- if (s$erro != "nenhum") r$sup else r$valor
    } else if (s$tipo == "linhas") {
      numerico <- x_numerico(d)
      eixo_x <- if (numerico) "x_num" else "x"
      if (!numerico) r$x_pos <- r$x
      regressao <- numerico && !identical(s$regressao, "nenhuma")
      camadas_linha <- list()
      if (regressao) {
        ajuste <- desenhar_regressao(d, s, cores, var_cor, agrupado)
        camadas_linha <- ajuste$camadas
        eq_textos <- ajuste$equacoes
      } else {
        camadas_linha <- list(camada(geom_line, 
          data = r, aes(x = .data[[eixo_x]], y = .data$valor, group = .data$grupo, !!!aes_cor("colour"),
                        linetype = if (isTRUE(s$tipos_linha) && agrupado) .data$grupo else NULL),
          linewidth = s$espessura_linha, colour = if (is.null(var_cor)) cor_unica else NULL, alpha = opacidade))
      }
      traco <- s$erro_traco * if (numerico) diff(range(r$x_num)) / max(1, length(unique(r$x_num)) - 1) else 1
      camadas <- c(camadas, camadas_linha,
        if (s$erro != "nenhum") list(geom_errorbar(data = r, aes(x = .data[[eixo_x]], ymin = .data$inf, ymax = .data$sup, group = .data$grupo),
                                                   width = traco, colour = erro_cor, linewidth = s$erro_largura)),
        list(camada(geom_point, data = r, aes(x = .data[[eixo_x]], y = .data$valor, !!!aes_cor("colour"),
                                      shape = if (isTRUE(s$formas) && agrupado) .data$grupo else NULL),
                        size = s$tamanho_ponto, colour = if (is.null(var_cor)) cor_unica else NULL))
      )
      if (numerico) {
        escala_x_num <- scale_x_continuous(breaks = sort(unique(r$x_num)), labels = rotulos_numero(s))
        camadas <- c(camadas, list(escala_x_num))
      }
      topo_rotulos <- if (s$erro != "nenhum") r$sup else r$valor
      limite_zero <- FALSE
    } else {
      # Médias e barras de erro (pontos)
      camadas <- c(camadas, list(
        if (s$erro != "nenhum") geom_errorbar(data = r, aes(x = .data$x, ymin = .data$inf, ymax = .data$sup, group = .data$grupo),
                                              position = desvio, width = s$erro_traco, colour = erro_cor, linewidth = s$erro_largura),
        camada(geom_point, data = r, aes(x = .data$x, y = .data$valor, !!!aes_cor("colour"), group = .data$grupo,
                                 shape = if (isTRUE(s$formas) && agrupado) .data$grupo else NULL),
                   position = desvio, size = s$tamanho_ponto * 1.3, colour = if (is.null(var_cor)) cor_unica else NULL)
      ))
      topo_rotulos <- if (s$erro != "nenhum") r$sup else r$valor
    }

    if (s$tipo %in% c("barras", "barras_h") && s$erro != "nenhum") {
      camadas <- c(camadas, list(geom_errorbar(
        data = r, aes(x = .data$x, ymin = .data$inf, ymax = .data$sup, group = .data$grupo),
        position = desvio, width = s$erro_traco * largura / if (agrupado) max(1, nlevels(droplevels(r$grupo))) else 1,
        colour = erro_cor, linewidth = s$erro_largura)))
    }

    # Observações individuais sobre barras e pontos
    if (isTRUE(s$observacoes) && s$tipo %in% c("barras", "barras_h", "pontos")) {
      posicao <- if (agrupado) position_jitterdodge(jitter.width = 0.12, dodge.width = largura, seed = 1) else position_jitter(width = 0.08, height = 0, seed = 1)
      camadas <- c(camadas, list(camada(geom_point, data = d, aes(x = .data$x, y = .data$y, group = .data$grupo, fill = if (agrupado) .data$grupo else NULL),
                                            position = posicao, size = s$tamanho_ponto * 0.6, colour = tinta, alpha = 0.45)))
    }

    # Rótulos sobre as barras/pontos: letras informadas ou o próprio valor
    if (s$tipo != "empilhadas" && !identical(s$rotulo, "nenhum")) {
      r$rotulo <- if (identical(s$rotulo, "valor")) y_numero(r$valor) else r$letra
      r$pos_rotulo <- topo_rotulos
      eixo_rotulo <- if (s$tipo == "linhas" && x_numerico(d)) "x_num" else "x"
      camadas <- c(camadas, list(geom_text(
        data = r[nzchar(r$rotulo), , drop = FALSE],
        aes(x = .data[[eixo_rotulo]], y = .data$pos_rotulo, label = .data$rotulo, group = .data$grupo),
        position = if (s$tipo %in% c("barras", "barras_h", "pontos")) desvio else "identity",
        vjust = if (identical(s$tipo, "barras_h")) 0.5 else -0.55,
        hjust = if (identical(s$tipo, "barras_h")) -0.25 else 0.5,
        size = tamanho_rotulo, family = familia, colour = tinta, fontface = "bold")))
    }
    titulo_y_padrao <- s$y
    valores_y <- if (s$tipo == "empilhadas") c(0, r$sup) else c(r$inf, r$sup, r$valor)
    if (isTRUE(s$observacoes)) valores_y <- c(valores_y, d$y)
  } else if (s$tipo == "dispersao") {
    numerico <- x_numerico(d)
    eixo_x <- if (numerico) "x_num" else "x"
    camadas <- c(camadas, list(camada(geom_point, 
      data = d, aes(x = .data[[eixo_x]], y = .data$y, !!!aes_cor("colour"),
                    shape = if (isTRUE(s$formas) && agrupado) .data$grupo else NULL),
      size = s$tamanho_ponto, alpha = opacidade, colour = if (is.null(var_cor)) cor_unica else NULL,
      position = if (numerico) "identity" else position_jitter(width = 0.1, height = 0, seed = 1))))
    if (numerico && !identical(s$regressao, "nenhuma")) {
      ajuste <- desenhar_regressao(d, s, cores, var_cor, agrupado)
      camadas <- c(camadas, ajuste$camadas)
      eq_textos <- ajuste$equacoes
    }
    if (numerico) camadas <- c(camadas, list(scale_x_continuous(labels = rotulos_numero(s))))
    valores_y <- d$y
  } else if (s$tipo %in% c("boxplot", "violino")) {
    preenchimento <- if (is.null(var_cor)) cor_unica else NULL
    if (s$tipo == "boxplot") {
      camadas <- c(camadas, list(camada(geom_boxplot, 
        data = d, aes(x = .data$x, y = .data$y, !!!aes_cor("fill"), group = interaction(.data$x, .data$grupo)),
        position = position_dodge2(preserve = "single", padding = 0.15), width = largura,
        fill = preenchimento, colour = if (is.na(contorno)) tinta else contorno,
        linewidth = max(0.2, s$contorno_largura), alpha = opacidade,
        outlier.shape = if (isTRUE(s$observacoes)) NA else 19, outlier.size = s$tamanho_ponto * 0.6)))
    } else {
      camadas <- c(camadas, list(
        camada(geom_violin, data = d, aes(x = .data$x, y = .data$y, !!!aes_cor("fill"), group = interaction(.data$x, .data$grupo)),
                    position = position_dodge(width = largura + 0.1), width = largura, fill = preenchimento,
                    colour = if (is.na(contorno)) tinta else contorno, linewidth = max(0.2, s$contorno_largura),
                    alpha = opacidade, trim = TRUE, scale = "width"),
        camada(geom_boxplot, data = d, aes(x = .data$x, y = .data$y, group = interaction(.data$x, .data$grupo)),
                     position = position_dodge(width = largura + 0.1), width = 0.1, fill = "white",
                     colour = if (is.na(contorno)) tinta else contorno, outlier.shape = NA, linewidth = 0.35)
      ))
    }
    if (isTRUE(s$observacoes)) {
      posicao <- if (agrupado) position_jitterdodge(jitter.width = 0.1, dodge.width = largura + if (s$tipo == "violino") 0.1 else 0, seed = 1) else position_jitter(width = 0.08, height = 0, seed = 1)
      camadas <- c(camadas, list(camada(geom_point, data = d, aes(x = .data$x, y = .data$y, group = .data$grupo, fill = if (agrupado) .data$grupo else NULL),
                                            position = posicao, size = s$tamanho_ponto * 0.55, colour = tinta, alpha = 0.5)))
    }
    valores_y <- d$y
  } else if (s$tipo == "histograma") {
    camadas <- c(camadas, list(camada(geom_histogram, 
      data = d, aes(x = .data$y, !!!aes_cor("fill")), bins = max(3, as.integer(s$bins %||% 15)),
      position = if (agrupado) "identity" else "stack", fill = if (is.null(var_cor)) cor_unica else NULL,
      colour = contorno, linewidth = s$contorno_largura, alpha = if (agrupado) min(opacidade, 0.65) else opacidade)))
    titulo_x_padrao <- s$y
    titulo_y_padrao <- "Frequência"
    limite_zero <- TRUE
    camadas <- c(camadas, list(scale_x_continuous(labels = y_numero)))
    y_numero <- rotulos_numero(utils::modifyList(s, list(decimais = 0)))
    valores_y <- NULL
  } else if (s$tipo == "densidade") {
    camadas <- c(camadas, list(camada(geom_density, 
      data = d, aes(x = .data$y, !!!aes_cor("fill"), !!!aes_cor("colour")),
      fill = if (is.null(var_cor)) cor_unica else NULL, colour = if (is.null(var_cor)) (if (is.na(contorno)) cor_unica else contorno) else NULL,
      alpha = min(opacidade, if (agrupado) 0.45 else 0.7), linewidth = s$espessura_linha)))
    titulo_x_padrao <- s$y
    titulo_y_padrao <- "Densidade"
    limite_zero <- TRUE
    camadas <- c(camadas, list(scale_x_continuous(labels = y_numero)))
    valores_y <- NULL
  }

  g <- ggplot() + camadas

  # Cores, formas e tipos de linha
  rotulos_escala <- rotulos_legenda
  if (!is.null(var_cor)) {
    if (!is.null(eq_textos) && identical(s$equacao, "legenda")) {
      rotulos_escala <- ifelse(niveis_cor %in% names(eq_textos),
                               paste0(texto_simples(niveis_cor), ": ", eq_textos[niveis_cor]), texto_simples(niveis_cor))
    }
    g <- g + scale_fill_manual(values = cores, labels = rotulos_escala, name = legenda_cor, drop = TRUE,
                               aesthetics = c("fill", "colour"))
    if (identical(var_cor, "x")) g <- g + guides(fill = "none", colour = "none")
  }
  rotulos_grupo <- if (!is.null(var_cor) && identical(var_cor, "grupo")) rotulos_escala else rotulos_legenda
  if (agrupado && isTRUE(s$formas)) {
    g <- g + scale_shape_manual(values = rep(FORMAS, length.out = nlevels(d$grupo)), labels = rotulos_grupo, name = legenda_cor)
  }
  if (agrupado && isTRUE(s$tipos_linha)) {
    g <- g + scale_linetype_manual(values = rep(TIPOS_LINHA, length.out = nlevels(d$grupo)), labels = rotulos_grupo, name = legenda_cor)
  }
  if (!(s$tipo %in% TIPOS_SO_Y) && !(s$tipo %in% c("linhas", "dispersao") && x_numerico(d))) {
    g <- g + scale_x_discrete(labels = function(v) formatar_texto(v))
  }

  # Eixo Y: início no zero, limites digitados e números no formato escolhido
  if (is.null(escala_y)) {
    espaco_topo <- if (!identical(s$rotulo, "nenhum") && s$tipo %in% TIPOS_RESUMO) 0.12 else 0.05
    if (!is.null(eq_textos) && identical(s$equacao, "grafico")) espaco_topo <- espaco_topo + 0.075 * length(eq_textos)
    escala_y <- scale_y_continuous(labels = y_numero, expand = expansion(mult = c(if (limite_zero) 0 else 0.05, espaco_topo)))
  }
  g <- g + escala_y
  y_min <- suppressWarnings(as.numeric(s$y_min))
  y_max <- suppressWarnings(as.numeric(s$y_max))
  if (limite_zero && (length(y_min) != 1 || is.na(y_min))) {
    y_min <- if (!is.null(valores_y) && any(valores_y < 0, na.rm = TRUE)) NA else 0
  }
  limites <- c(if (length(y_min) == 1) y_min else NA, if (length(y_max) == 1) y_max else NA)
  if (all(is.na(limites))) limites <- NULL
  if (!is.null(limites) && any(is.na(limites)) && !is.null(valores_y)) {
    faixa <- range(valores_y, na.rm = TRUE)
    # A folga (rótulos, equações) vem da expansão da escala, aplicada também pelo coord.
    if (is.na(limites[1])) limites[1] <- faixa[1]
    if (is.na(limites[2])) limites[2] <- faixa[2]
  }
  if (!is.null(limites) && any(is.na(limites))) limites <- NULL
  g <- g + if (virar) coord_flip(ylim = limites, clip = "off") else coord_cartesian(ylim = limites, clip = "off")

  # Equações de regressão dentro do gráfico
  if (!is.null(eq_textos) && identical(s$equacao, "grafico")) {
    eq <- data.frame(rotulo = if (agrupado) paste0(texto_simples(names(eq_textos)), ": ", eq_textos) else unname(eq_textos),
                     nivel = names(eq_textos), ordem = seq_along(eq_textos))
    eq$vjust <- 1.5 + 1.45 * (eq$ordem - 1)
    g <- g + geom_text(data = eq, aes(x = -Inf, y = Inf, label = .data$rotulo, vjust = .data$vjust),
                       hjust = -0.04, size = tamanho_rotulo * 0.92, family = familia,
                       colour = if (agrupado && !is.null(var_cor)) unname(cores[eq$nivel]) else tinta,
                       inherit.aes = FALSE)
  }

  # Painéis (facetas)
  if (facetado) {
    ncol <- suppressWarnings(as.integer(s$ncol_facetas))
    g <- g + facet_wrap(vars(.data$faceta), ncol = if (length(ncol) == 1 && !is.na(ncol) && ncol > 0) ncol else NULL,
                        scales = s$escala_facetas %||% "fixed", labeller = labeller(.default = function(v) texto_simples(v)))
  }

  g + labs(
    x = titulo_eixo(s$titulo_x, titulo_x_padrao, isTRUE(s$negrito_eixos)),
    y = titulo_eixo(s$titulo_y, titulo_y_padrao, isTRUE(s$negrito_eixos)),
    title = if (nzchar(trimws(s$titulo))) titulo_eixo(s$titulo, "", TRUE) else NULL,
    subtitle = if (nzchar(trimws(s$subtitulo))) formatar_texto(s$subtitulo) else NULL,
    caption = if (nzchar(trimws(s$rodape))) formatar_texto(s$rodape) else NULL
  ) + tema_plota(s)
}

# Camada do ggplot sem os parâmetros NULL (cor fixa só quando não há mapeamento).
camada <- function(geom, ...) {
  argumentos <- list(...)
  do.call(geom, argumentos[!vapply(argumentos, is.null, logical(1))])
}

# Curvas ajustadas por grupo (e por faceta) sobre os dados brutos.
desenhar_regressao <- function(d, s, cores, var_cor, agrupado) {
  partes <- split(d, interaction(d$grupo, d$faceta, drop = TRUE, lex.order = TRUE))
  curvas <- list(); equacoes <- c(); otimos <- list()
  for (p in partes) {
    ajuste <- ajustar_regressao(p$x_num, p$y, s$regressao)
    if (is.null(ajuste)) next
    ajuste$curva$grupo <- p$grupo[1]
    ajuste$curva$faceta <- p$faceta[1]
    curvas[[length(curvas) + 1]] <- ajuste$curva
    if (!is.null(ajuste$equacao) && !tem_faceta(d)) equacoes[as.character(p$grupo[1])] <- ajuste$equacao
    if (isTRUE(s$ponto_otimo) && !is.null(ajuste$otimo)) {
      otimos[[length(otimos) + 1]] <- data.frame(x = ajuste$otimo$x, y = ajuste$otimo$y, grupo = p$grupo[1], faceta = p$faceta[1],
                                                 rotulo = paste0(if (ajuste$otimo$maximo) "máx." else "mín.", " x = ", num_br(ajuste$otimo$x, 1)))
    }
  }
  if (!length(curvas)) return(list(camadas = list(), equacoes = NULL))
  curvas <- do.call(rbind, curvas)
  cor_fixa <- if (is.null(var_cor) || identical(var_cor, "x")) (if (cor_valida(s$cor_unica)) s$cor_unica else CORES_APP$blue) else NULL
  camadas <- list(camada(geom_line, 
    data = curvas, aes(x = .data$x, y = .data$y, group = .data$grupo,
                       colour = if (is.null(cor_fixa)) .data$grupo else NULL,
                       linetype = if (isTRUE(s$tipos_linha) && agrupado) .data$grupo else NULL),
    colour = cor_fixa, linewidth = s$espessura_linha))
  if (length(otimos)) {
    otimos <- do.call(rbind, otimos)
    tinta <- if (cor_valida(s$cor_texto)) s$cor_texto else CORES_APP$ink
    camadas <- c(camadas, list(
      geom_segment(data = otimos, aes(x = .data$x, xend = .data$x, y = -Inf, yend = .data$y), linetype = "22", colour = tinta, linewidth = 0.35),
      geom_text(data = otimos, aes(x = .data$x, y = .data$y, label = .data$rotulo), vjust = -0.9,
                size = s$tamanho * 0.75 / .pt, family = fonte_valida(s$fonte), colour = tinta)
    ))
  }
  list(camadas = camadas, equacoes = if (length(equacoes)) equacoes else NULL)
}

# ---------------------------------------------------------
# Painel com vários gráficos (A, B, C...)
# ---------------------------------------------------------

ESTILOS_LETRAS <- c("A  B  C" = "A", "a  b  c" = "a", "(A)  (B)  (C)" = "(A)", "(a)  (b)  (c)" = "(a)",
                    "A)  B)  C)" = "A)", "Sem letras" = "nenhuma")

montar_painel <- function(graficos, painel) {
  ncol <- max(1L, min(as.integer(painel$ncol %||% 2), length(graficos)))
  estilo <- painel$letras %||% "A"
  familia <- fonte_valida(painel$fonte)
  p <- patchwork::wrap_plots(graficos, ncol = ncol) +
    patchwork::plot_layout(guides = if (isTRUE(painel$legenda_unica)) "collect" else "keep")
  if (estilo != "nenhuma") {
    niveis <- if (grepl("a", estilo)) "a" else "A"
    p <- p + patchwork::plot_annotation(
      tag_levels = niveis,
      tag_prefix = if (startsWith(estilo, "(")) "(" else "",
      tag_suffix = if (grepl("\\)$", estilo)) ")" else ""
    )
  }
  titulo <- trimws(painel$titulo %||% "")
  if (nzchar(titulo)) {
    p <- p + patchwork::plot_annotation(title = formatar_texto(titulo),
                                        theme = theme(plot.title = element_text(family = familia, face = "bold", size = 15, colour = CORES_APP$ink)))
  }
  tamanho_letra <- as.numeric(painel$tamanho_letras %||% 14)
  p & theme(plot.tag = element_text(family = familia, face = "bold", size = tamanho_letra, colour = CORES_APP$ink),
            legend.position = if (isTRUE(painel$legenda_unica)) painel$posicao_legenda %||% "bottom" else NULL)
}

# ---------------------------------------------------------
# Exportação (e prévia, que usa o mesmo caminho: o que se vê é o que se baixa)
# ---------------------------------------------------------

FORMATOS <- c("PNG" = "png", "TIFF" = "tiff", "JPEG" = "jpeg", "PDF" = "pdf", "SVG" = "svg", "EPS" = "eps")
FORMATOS_VETORIAIS <- c("pdf", "svg", "eps")
UNIDADES <- c("cm" = "cm", "mm" = "mm", "polegadas" = "in", "pixels" = "px")

TAMANHOS_PRONTOS <- list(
  personalizado = list(nome = "Personalizado"),
  coluna1 = list(nome = "Artigo, 1 coluna (8,5 × 7 cm)", largura = 8.5, altura = 7, unidade = "cm"),
  coluna15 = list(nome = "Artigo, 1,5 coluna (12 × 9 cm)", largura = 12, altura = 9, unidade = "cm"),
  coluna2 = list(nome = "Artigo, 2 colunas (17,5 × 11 cm)", largura = 17.5, altura = 11, unidade = "cm"),
  a4 = list(nome = "Página A4 com margens (16 × 22 cm)", largura = 16, altura = 22, unidade = "cm"),
  slide = list(nome = "Slide 16:9 (25,4 × 14,3 cm)", largura = 25.4, altura = 14.29, unidade = "cm"),
  quadrado = list(nome = "Quadrado (12 × 12 cm)", largura = 12, altura = 12, unidade = "cm"),
  hd = list(nome = "Tela Full HD (1920 × 1080 px)", largura = 1920, altura = 1080, unidade = "px")
)
OPCOES_TAMANHOS <- stats::setNames(names(TAMANHOS_PRONTOS), vapply(TAMANHOS_PRONTOS, `[[`, character(1), "nome"))

em_polegadas <- function(valor, unidade, dpi = 300) {
  valor <- as.numeric(valor)
  switch(unidade, cm = valor / 2.54, mm = valor / 25.4, px = valor / as.numeric(dpi), valor)
}

validar_exportacao <- function(largura_in, altura_in, dpi) {
  if (any(is.na(c(largura_in, altura_in, dpi)))) stop("Informe largura, altura e resolução.")
  if (largura_in < 0.5 || altura_in < 0.5) stop("O gráfico precisa ter pelo menos 1,3 cm de largura e de altura.")
  if (largura_in > 60 || altura_in > 60) stop("O tamanho máximo é de 150 cm por lado.")
  if (dpi < 50 || dpi > 2400) stop("Use uma resolução entre 50 e 2400 dpi.")
  if (largura_in * dpi * altura_in * dpi > 2.5e8) stop("Imagem grande demais: reduza o tamanho ou a resolução.")
  invisible(TRUE)
}

# Desenha o gráfico com as fontes do app em qualquer dispositivo.
imprimir_com_fontes <- function(grafico, dpi) {
  showtext::showtext_opts(dpi = dpi)
  showtext::showtext_begin()
  on.exit(showtext::showtext_end(), add = TRUE)
  print(grafico)
}

salvar_grafico <- function(arquivo, grafico, formato = "png", dpi = 300, largura_in = 7, altura_in = 5, transparente = FALSE) {
  dpi <- as.numeric(dpi)
  validar_exportacao(largura_in, altura_in, dpi)
  fundo <- if (isTRUE(transparente) && formato %in% c("png", "tiff", "pdf", "svg")) "transparent" else "white"
  px <- c(round(largura_in * dpi), round(altura_in * dpi))
  tipo_bitmap <- if (capabilities("cairo")) "cairo" else NULL
  switch(
    formato,
    png = grDevices::png(arquivo, width = px[1], height = px[2], res = dpi, bg = fundo, type = tipo_bitmap %||% "Xlib"),
    tiff = grDevices::tiff(arquivo, width = px[1], height = px[2], res = dpi, bg = fundo, compression = "lzw", type = tipo_bitmap %||% "Xlib"),
    jpeg = grDevices::jpeg(arquivo, width = px[1], height = px[2], res = dpi, bg = "white", quality = 95, type = tipo_bitmap %||% "Xlib"),
    pdf = grDevices::cairo_pdf(arquivo, width = largura_in, height = altura_in, bg = fundo),
    svg = grDevices::svg(arquivo, width = largura_in, height = altura_in, bg = fundo),
    eps = grDevices::cairo_ps(arquivo, width = largura_in, height = altura_in, bg = "white", fallback_resolution = dpi),
    stop("Formato desconhecido.")
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  imprimir_com_fontes(grafico, if (formato %in% FORMATOS_VETORIAIS) 72 else dpi)
  invisible(arquivo)
}

nome_arquivo <- function(nome, formato) {
  base <- iconv(nome %||% "grafico", to = "ASCII//TRANSLIT")
  base <- gsub("[^A-Za-z0-9]+", "-", tolower(base %||% "grafico"))
  base <- gsub("^-|-$", "", base)
  paste0(if (nzchar(base)) base else "grafico", ".", formato)
}

# ---------------------------------------------------------
# Projeto (.plota): dados e gráficos salvos no computador do usuário
# ---------------------------------------------------------

projeto_para_json <- function(dados, graficos, ordem, painel) {
  jsonlite::toJSON(list(
    app = "Plota", versao = VERSAO_APP,
    dados = lapply(dados, function(tabela) list(colunas = names(tabela), linhas = unname(as.list(tabela)))),
    graficos = unname(lapply(ordem, function(id) graficos[[id]])),
    painel = painel
  ), auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA)
}

json_para_projeto <- function(caminho) {
  bruto <- jsonlite::fromJSON(caminho, simplifyVector = FALSE)
  if (!identical(bruto$app, "Plota")) stop("Este arquivo não é um projeto do Plota.")
  dados <- lapply(bruto$dados, function(tabela) {
    colunas <- lapply(tabela$linhas, function(coluna) vapply(coluna, function(v) if (is.null(v)) "" else as.character(v), character(1)))
    saida <- as.data.frame(colunas, stringsAsFactors = FALSE, col.names = unlist(tabela$colunas), check.names = FALSE)
    saida
  })
  graficos <- lapply(bruto$graficos, function(g) {
    g <- lapply(g, function(v) if (is.null(v)) NA else v)
    g$cores <- if (is.list(g$cores)) lapply(g$cores, as.character) else list()
    completar_spec(g)
  })
  list(dados = dados, graficos = graficos, painel = bruto$painel %||% list())
}
