# Campos do gráfico lidos da interface (as cores dos níveis são tratadas à parte).
CAMPOS <- setdiff(names(PADRAO_GRAFICO), c("cores", "cores_paleta"))

# Campos de aparência copiados pelo botão "Estilo em todos".
CAMPOS_ESTILO <- c("paleta", "cor_unica", "contorno", "contorno_largura", "sem_contorno", "largura_barra", "opacidade",
                   "tamanho_ponto", "espessura_linha", "formas", "tipos_linha", "erro_cor", "erro_largura", "erro_traco",
                   "fonte", "tamanho", "cor_texto", "negrito_eixos", "tamanho_rotulos", "tema", "grade", "legenda",
                   "marcas_dentro", "girar_x", "virgula", "decimais", "erro", "erro_lado", "estatistica")

opcoes_colunas <- function(dados, vazio = NULL) {
  colunas <- names(dados %||% list())
  c(vazio, stats::setNames(colunas, colunas))
}

# Painel de controles de um gráfico. `p` é o prefixo dos ids (muda a cada desenho,
# assim valores antigos de outros gráficos nunca se misturam).
controles_grafico <- function(p, spec, nomes_dados, dados) {
  id <- function(campo) paste0(p, campo)
  cond <- function(campo, valores) sprintf("[%s].indexOf(input['%s']) >= 0", paste0("'", valores, "'", collapse = ","), id(campo))
  nenhum <- c("(nenhum)" = "")
  tipos <- names(TIPOS_GRAFICO)

  tagList(
    div(class = "cabecalho-secao",
      h4(class = "titulo-cartao", tags$span(class = "numero-passo", icon("sliders")), "Configurar gráfico"),
      div(class = "campo-nome", textInput(id("nome"), NULL, value = spec$nome, placeholder = "Nome do gráfico"))
    ),

    recolhivel("Tipo de gráfico", "Barras, linhas, dispersão, boxplot...", "shapes", aberto = TRUE,
      radioButtons(id("tipo"), NULL, selected = spec$tipo, width = "100%",
                   choiceValues = tipos,
                   choiceNames = unname(lapply(TIPOS_GRAFICO, function(t) tags$span(class = "tile-tipo", icon(t$icone), tags$span(t$nome)))))
    ),

    recolhivel("Dados e variáveis", "O que vai em cada eixo, cor e painel", "table-columns", aberto = TRUE,
      selectInput(id("dados"), "Conjunto de dados", choices = nomes_dados, selected = spec$dados, width = "100%"),
      div(class = "grade-campos",
        conditionalPanel(sprintf("!(%s)", cond("tipo", TIPOS_SO_Y)),
          selectInput(id("x"), "Eixo X (tratamentos ou doses)", choices = opcoes_colunas(dados), selected = spec$x)),
        selectInput(id("y"), "Eixo Y (variável resposta)", choices = opcoes_colunas(dados), selected = spec$y),
        selectInput(id("grupo"), "Cor (grupo / 2º fator)", choices = opcoes_colunas(dados, nenhum), selected = spec$grupo),
        selectInput(id("faceta"), "Dividir em painéis por", choices = opcoes_colunas(dados, nenhum), selected = spec$faceta)
      )
    ),

    recolhivel("Estatística e rótulos", "Média, erro, letras, regressão", "calculator",
      conditionalPanel(cond("tipo", TIPOS_RESUMO),
        div(class = "grade-campos",
          selectInput(id("estatistica"), "Resumo", choices = c("Média" = "media", "Mediana" = "mediana", "Soma" = "soma"), selected = spec$estatistica),
          selectInput(id("erro"), "Barra de erro", selected = spec$erro,
                      choices = c("Erro-padrão da média" = "ep", "Desvio-padrão" = "dp", "Intervalo de confiança 95%" = "ic95", "Sem barra de erro" = "nenhum")),
          conditionalPanel(cond("tipo", c("barras", "barras_h")),
            selectInput(id("erro_lado"), "Mostrar erro", choices = c("Para cima e para baixo" = "ambos", "Só para cima" = "superior"), selected = spec$erro_lado)),
          selectInput(id("rotulo"), "Rótulos", selected = spec$rotulo,
                      choices = c("Nenhum" = "nenhum", "Letras de uma coluna" = "letras", "Valor da média" = "valor"))
        ),
        conditionalPanel(sprintf("input['%s'] == 'letras'", id("rotulo")),
          selectInput(id("col_letras"), "Coluna com as letras", choices = opcoes_colunas(dados, nenhum), selected = spec$col_letras, width = "100%")),
        conditionalPanel(cond("tipo", "empilhadas"),
          radioButtons(id("empilhar"), "Empilhar", choices = c("Valores" = "valores", "Percentual (100%)" = "percentual"), selected = spec$empilhar, inline = TRUE))
      ),
      conditionalPanel(cond("tipo", c("linhas", "dispersao")),
        div(class = "grade-campos",
          selectInput(id("regressao"), "Regressão (X numérico)", choices = REGRESSOES, selected = spec$regressao),
          conditionalPanel(sprintf("['nenhuma','loess'].indexOf(input['%s']) < 0", id("regressao")),
            selectInput(id("equacao"), "Equação e R²", selected = spec$equacao,
                        choices = c("No gráfico" = "grafico", "Na legenda" = "legenda", "Não mostrar" = "nenhuma")))
        ),
        conditionalPanel(sprintf("input['%s'] == 'quadratica'", id("regressao")),
          checkboxInput(id("ponto_otimo"), "Marcar dose de máxima (ou mínima)", value = isTRUE(spec$ponto_otimo)))
      ),
      conditionalPanel(cond("tipo", c("barras", "barras_h", "pontos", "boxplot", "violino")),
        checkboxInput(id("observacoes"), "Mostrar as observações (pontos individuais)", value = isTRUE(spec$observacoes))),
      conditionalPanel(cond("tipo", "histograma"),
        sliderInput(id("bins"), "Número de classes", min = 3, max = 60, value = spec$bins, step = 1, width = "100%", ticks = FALSE))
    ),

    recolhivel("Cores e formas", "Paleta, cor de cada nível, contorno, espessuras", "palette",
      div(class = "grade-campos",
        selectInput(id("paleta"), "Paleta", choices = OPCOES_PALETAS, selected = spec$paleta),
        conditionalPanel(sprintf("input['%s'] == '' && %s", id("grupo"), sprintf("!(%s)", cond("tipo", TIPOS_SO_Y))),
          div(class = "caixa-checkbox", checkboxInput(id("colorir_x"), "Uma cor para cada nível do X", value = isTRUE(spec$colorir_x))))
      ),
      uiOutput("cores_niveis"),
      div(class = "grade-aparencia",
        colourpicker::colourInput(id("contorno"), "Contorno", value = spec$contorno, showColour = "background"),
        numericInput(id("contorno_largura"), "Espessura do contorno", value = spec$contorno_largura, min = 0, max = 3, step = 0.1),
        div(class = "caixa-checkbox", checkboxInput(id("sem_contorno"), "Sem contorno", value = isTRUE(spec$sem_contorno))),
        colourpicker::colourInput(id("erro_cor"), "Cor da barra de erro", value = spec$erro_cor, showColour = "background"),
        numericInput(id("erro_largura"), "Espessura do erro", value = spec$erro_largura, min = 0.1, max = 3, step = 0.1),
        numericInput(id("erro_traco"), "Largura do traço do erro", value = spec$erro_traco, min = 0, max = 1, step = 0.05)
      ),
      div(class = "grade-campos",
        sliderInput(id("largura_barra"), "Largura das barras/caixas", min = 0.2, max = 1, value = spec$largura_barra, step = 0.05, ticks = FALSE),
        sliderInput(id("opacidade"), "Opacidade", min = 0.1, max = 1, value = spec$opacidade, step = 0.05, ticks = FALSE),
        sliderInput(id("tamanho_ponto"), "Tamanho dos pontos", min = 0.5, max = 8, value = spec$tamanho_ponto, step = 0.1, ticks = FALSE),
        sliderInput(id("espessura_linha"), "Espessura das linhas", min = 0.2, max = 3, value = spec$espessura_linha, step = 0.1, ticks = FALSE)
      ),
      div(class = "grade-campos",
        checkboxInput(id("formas"), "Formas diferentes por grupo", value = isTRUE(spec$formas)),
        checkboxInput(id("tipos_linha"), "Tracejados diferentes por grupo", value = isTRUE(spec$tipos_linha))
      )
    ),

    recolhivel("Textos e fonte", "Títulos, fonte, tamanho e cor do texto", "font",
      div(class = "explicacao", HTML("Use <code>^</code> e <code>_</code> para expoentes e índices: <code>kg ha^-1</code>, <code>CO_2</code>. Deixe em branco para usar o nome da coluna.")),
      textInput(id("titulo"), "Título", value = spec$titulo, width = "100%"),
      div(class = "grade-campos",
        textInput(id("subtitulo"), "Subtítulo", value = spec$subtitulo),
        textInput(id("rodape"), "Nota de rodapé", value = spec$rodape),
        textInput(id("titulo_x"), "Título do eixo X", value = spec$titulo_x),
        textInput(id("titulo_y"), "Título do eixo Y", value = spec$titulo_y),
        textInput(id("titulo_legenda"), "Título da legenda", value = spec$titulo_legenda),
        div(class = "caixa-checkbox", checkboxInput(id("sem_titulo_legenda"), "Legenda sem título", value = isTRUE(spec$sem_titulo_legenda)))
      ),
      div(class = "grade-aparencia",
        selectInput(id("fonte"), "Fonte", choices = OPCOES_FONTES, selected = spec$fonte),
        numericInput(id("tamanho"), "Tamanho (pt)", value = spec$tamanho, min = 5, max = 40, step = 0.5),
        numericInput(id("tamanho_rotulos"), "Rótulos (× texto)", value = spec$tamanho_rotulos, min = 0.4, max = 3, step = 0.05),
        colourpicker::colourInput(id("cor_texto"), "Cor do texto e eixos", value = spec$cor_texto, showColour = "background"),
        div(class = "caixa-checkbox", checkboxInput(id("negrito_eixos"), "Títulos dos eixos em negrito", value = isTRUE(spec$negrito_eixos)))
      )
    ),

    recolhivel("Eixos, legenda e tema", "Limites, números, grade, posição da legenda", "ruler-combined",
      div(class = "grade-campos",
        selectInput(id("tema"), "Tema", choices = TEMAS, selected = spec$tema),
        selectInput(id("grade"), "Linhas de grade", choices = c("Sem grade" = "nenhuma", "Horizontais" = "y", "Horizontais e verticais" = "xy"), selected = spec$grade),
        selectInput(id("legenda"), "Legenda", choices = LEGENDAS, selected = spec$legenda),
        selectInput(id("girar_x"), "Nomes do eixo X", choices = c("Retos" = "0", "Inclinados (45°)" = "45", "Verticais (90°)" = "90"), selected = spec$girar_x),
        numericInput(id("y_min"), "Y mínimo", value = spec$y_min),
        numericInput(id("y_max"), "Y máximo", value = spec$y_max),
        selectInput(id("decimais"), "Casas decimais do eixo", choices = c("Automático" = "", "0" = "0", "1" = "1", "2" = "2", "3" = "3"),
                    selected = if (is.na(spec$decimais %||% NA)) "" else as.character(spec$decimais)),
        div(class = "caixa-checkbox", checkboxInput(id("virgula"), "Vírgula decimal (1,5)", value = isTRUE(spec$virgula)))
      ),
      div(class = "grade-campos",
        checkboxInput(id("y_zero"), "Barras começam no zero", value = isTRUE(spec$y_zero)),
        checkboxInput(id("marcas_dentro"), "Marcas dos eixos para dentro", value = isTRUE(spec$marcas_dentro))
      ),
      conditionalPanel(sprintf("input['%s'] != ''", id("faceta")),
        div(class = "grade-campos",
          selectInput(id("escala_facetas"), "Escala dos painéis", selected = spec$escala_facetas,
                      choices = c("Mesma escala" = "fixed", "Y livre" = "free_y", "X livre" = "free_x", "Ambos livres" = "free")),
          numericInput(id("ncol_facetas"), "Colunas de painéis", value = spec$ncol_facetas, min = 1, max = 8, step = 1)
        ))
    )
  )
}

server <- function(input, output, session) {

  estado <- reactiveValues(dados = list(), graficos = list(), ordem = character(0), atual = NULL, contador = 0)
  prefixo <- reactiveVal(NULL)          # prefixo dos ids dos controles do gráfico atual
  versao_controles <- reactiveVal(0)    # força redesenhar os controles
  versao_planilha <- reactiveVal(0)     # força redesenhar a planilha

  # ------------------------------------------------------------------ Dados
  adicionar_dados <- function(nome, tabela) {
    if (is.null(tabela) || ncol(tabela) == 0 || nrow(tabela) == 0) stop("Nenhum dado encontrado.")
    nome <- trimws(nome %||% "")
    if (!nzchar(nome)) nome <- paste("Dados", length(estado$dados) + 1)
    nome <- make.unique(c(names(estado$dados), nome), sep = " ")[length(estado$dados) + 1]
    estado$dados[[nome]] <- tabela
    updateSelectInput(session, "dados_ativo", choices = names(estado$dados), selected = nome)
    versao_planilha(versao_planilha() + 1)
    nome
  }

  criar_grafico <- function(spec) {
    estado$contador <- estado$contador + 1
    id <- paste0("g", estado$contador)
    estado$graficos[[id]] <- spec
    estado$ordem <- c(estado$ordem, id)
    estado$atual <- id
    versao_controles(versao_controles() + 1)
    id
  }

  carregar_exemplo <- function(nome, criar = TRUE) {
    tabela <- EXEMPLOS[[nome]]()
    nome_dados <- adicionar_dados(nome, tabela)
    if (criar) {
      spec <- novo_grafico(paste("Gráfico", estado$contador + 1), nome_dados, tabela)
      if (nome == "Cultivares × doses de N") {
        spec <- utils::modifyList(spec, list(x = "Cultivar", y = "Produtividade (t ha^-1)", rotulo = "letras", col_letras = "Letras"))
      } else {
        spec <- utils::modifyList(spec, list(tipo = "linhas", x = "DAS", y = "Massa seca (g planta^-1)", grupo = "Manejo",
                                             titulo_x = "Dias após a semeadura", legenda = "dentro_esq"))
      }
      criar_grafico(spec)
    }
  }

  # Começa com o exemplo de cultivares carregado.
  isolate(carregar_exemplo("Cultivares × doses de N"))

  notificar_erro <- function(e) showNotification(conditionMessage(e), type = "error", duration = 6)

  observeEvent(input$adicionar_colado, {
    tryCatch({
      nome <- adicionar_dados(input$nome_colado, ler_texto_colado(input$texto_colado))
      updateTextAreaInput(session, "texto_colado", value = "")
      updateTextInput(session, "nome_colado", value = "")
      showNotification(sprintf("Dados \"%s\" adicionados.", nome), type = "message")
    }, error = notificar_erro)
  })

  output$abas_arquivo <- renderUI({
    arquivo <- input$arquivo_dados
    if (is.null(arquivo) || !(tolower(tools::file_ext(arquivo$name)) %in% c("xlsx", "xls"))) return(NULL)
    abas <- tryCatch(readxl::excel_sheets(arquivo$datapath), error = function(e) NULL)
    if (length(abas) > 1) selectInput("aba_arquivo", "Aba da planilha", choices = abas, width = "100%")
  })

  observeEvent(input$adicionar_arquivo, {
    arquivo <- input$arquivo_dados
    if (is.null(arquivo)) return(showNotification("Escolha um arquivo primeiro.", type = "warning"))
    tryCatch({
      tabela <- ler_arquivo_dados(arquivo$datapath, arquivo$name, input$aba_arquivo)
      nome <- adicionar_dados(tools::file_path_sans_ext(arquivo$name), tabela)
      showNotification(sprintf("Dados \"%s\" adicionados.", nome), type = "message")
    }, error = notificar_erro)
  })

  observeEvent(input$adicionar_exemplo, carregar_exemplo(input$exemplo, criar = FALSE))

  output$planilha <- renderRHandsontable({
    versao_planilha()
    nome <- input$dados_ativo
    tabela <- isolate(estado$dados[[nome %||% ""]])
    req(tabela)
    rhandsontable(tabela, rowHeaders = TRUE, stretchH = "all", height = 420, useTypes = FALSE) |>
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = TRUE) |>
      hot_cols(columnSorting = FALSE)
  })

  planilha_editada <- reactive(input$planilha) |> debounce(400)

  observeEvent(planilha_editada(), {
    nome <- isolate(input$dados_ativo)
    req(nome, nome %in% names(estado$dados))
    tabela <- tryCatch(hot_to_r(planilha_editada()), error = function(e) NULL)
    req(tabela)
    tabela[] <- lapply(tabela, function(coluna) { coluna <- as.character(coluna); coluna[is.na(coluna)] <- ""; coluna })
    if (!identical(tabela, estado$dados[[nome]])) estado$dados[[nome]] <- as.data.frame(tabela, stringsAsFactors = FALSE, check.names = FALSE)
  })

  output$resumo_dados <- renderUI({
    tabela <- estado$dados[[input$dados_ativo %||% ""]]
    if (is.null(tabela)) return(NULL)
    tipos <- tipos_colunas(tabela)
    tagList(
      tags$span(class = "etiqueta", paste(nrow(tabela), "linhas")),
      tags$span(class = "etiqueta etiqueta-azul", paste(sum(tipos == "numérica"), "numéricas")),
      tags$span(class = "etiqueta etiqueta-dourada", paste(sum(tipos == "texto"), "de texto"))
    )
  })

  observeEvent(input$renomear_dados, {
    req(input$dados_ativo)
    showModal(modalDialog(
      title = "Renomear conjunto de dados",
      textInput("novo_nome_dados", "Novo nome", value = input$dados_ativo, width = "100%"),
      footer = tagList(modalButton("Cancelar"), actionButton("confirmar_renomear", "Renomear", class = "btn-principal")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirmar_renomear, {
    antigo <- input$dados_ativo
    novo <- trimws(input$novo_nome_dados %||% "")
    removeModal()
    if (!nzchar(novo) || identical(novo, antigo)) return()
    if (novo %in% names(estado$dados)) return(showNotification("Já existe um conjunto com esse nome.", type = "warning"))
    nomes <- names(estado$dados)
    nomes[nomes == antigo] <- novo
    names(estado$dados) <- nomes
    for (id in names(estado$graficos)) if (identical(estado$graficos[[id]]$dados, antigo)) estado$graficos[[id]]$dados <- novo
    updateSelectInput(session, "dados_ativo", choices = names(estado$dados), selected = novo)
    versao_controles(versao_controles() + 1)
  })

  observeEvent(input$excluir_dados, {
    nome <- input$dados_ativo
    req(nome)
    usados <- vapply(estado$graficos, function(g) identical(g$dados, nome), logical(1))
    if (any(usados)) {
      return(showNotification(sprintf("Estes dados são usados por %d gráfico(s). Troque o conjunto desses gráficos antes de excluir.", sum(usados)),
                              type = "warning", duration = 6))
    }
    estado$dados[[nome]] <- NULL
    updateSelectInput(session, "dados_ativo", choices = names(estado$dados), selected = names(estado$dados)[1] %||% character(0))
    versao_planilha(versao_planilha() + 1)
  })

  # ------------------------------------------------------------------ Gráficos: lista
  nomes_graficos <- reactive({
    ids <- estado$ordem
    stats::setNames(ids, vapply(ids, function(id) estado$graficos[[id]]$nome %||% id, character(1)))
  })
  # Só muda quando algum nome muda (não a cada ajuste de cor ou fonte).
  nomes_estaveis <- reactiveVal(character(0))
  observe({
    novos <- nomes_graficos()
    if (!identical(novos, isolate(nomes_estaveis()))) nomes_estaveis(novos)
  })

  output$lista_graficos <- renderUI({
    escolhas <- nomes_estaveis()
    if (!length(escolhas)) return(div(class = "explicacao", "Nenhum gráfico ainda. Clique em Novo."))
    radioButtons("grafico_atual", NULL, choices = escolhas, selected = isolate(estado$atual), inline = TRUE)
  })

  observeEvent(input$grafico_atual, {
    if (!identical(input$grafico_atual, estado$atual) && input$grafico_atual %in% estado$ordem) {
      estado$atual <- input$grafico_atual
      versao_controles(versao_controles() + 1)
    }
  })

  selecionar <- function(id) {
    estado$atual <- id
    updateRadioButtons(session, "grafico_atual", selected = id)
    versao_controles(versao_controles() + 1)
  }

  observeEvent(input$novo_grafico, {
    nome_dados <- estado$graficos[[estado$atual %||% ""]]$dados %||% names(estado$dados)[1]
    if (is.null(nome_dados)) return(showNotification("Adicione dados na aba 1 primeiro.", type = "warning"))
    atual <- estado$graficos[[estado$atual %||% ""]]
    spec <- novo_grafico(paste("Gráfico", estado$contador + 1), nome_dados, estado$dados[[nome_dados]])
    if (!is.null(atual)) spec[CAMPOS_ESTILO] <- atual[CAMPOS_ESTILO]
    criar_grafico(spec)
  })

  observeEvent(input$duplicar_grafico, {
    atual <- estado$graficos[[estado$atual %||% ""]]
    req(atual)
    atual$nome <- paste(atual$nome, "(cópia)")
    criar_grafico(atual)
  })

  observeEvent(input$excluir_grafico, {
    id <- estado$atual
    req(id)
    posicao <- match(id, estado$ordem)
    estado$graficos[[id]] <- NULL
    estado$ordem <- setdiff(estado$ordem, id)
    novo <- if (length(estado$ordem)) estado$ordem[min(posicao, length(estado$ordem))] else NULL
    estado$atual <- novo
    versao_controles(versao_controles() + 1)
  })

  observeEvent(input$estilo_todos, {
    atual <- estado$graficos[[estado$atual %||% ""]]
    req(atual, length(estado$ordem) > 1)
    for (id in setdiff(estado$ordem, estado$atual)) {
      estado$graficos[[id]][CAMPOS_ESTILO] <- atual[CAMPOS_ESTILO]
      if (identical(estado$graficos[[id]]$paleta, atual$paleta)) {
        estado$graficos[[id]]$cores <- utils::modifyList(estado$graficos[[id]]$cores %||% list(), atual$cores %||% list())
      }
    }
    showNotification(sprintf("Estilo aplicado a %d gráfico(s).", length(estado$ordem) - 1), type = "message")
  })

  # ------------------------------------------------------------------ Gráficos: controles
  observeEvent(versao_controles(), {
    prefixo(if (is.null(estado$atual)) NULL else paste0(estado$atual, "v", versao_controles(), "_"))
  })

  output$controles <- renderUI({
    p <- prefixo()
    if (is.null(p)) return(div(class = "nenhum-resultado", icon("chart-column"), tags$b("Nenhum gráfico"), "Clique em Novo para começar."))
    spec <- isolate(completar_spec(estado$graficos[[estado$atual]]))
    nomes_dados <- isolate(names(estado$dados))
    controles_grafico(p, spec, nomes_dados, isolate(estado$dados[[spec$dados]]))
  })

  # Colunas dos dados mudaram (edição da planilha): redesenha os controles.
  colunas_dados <- reactiveVal(NULL)
  observe({
    assinatura <- lapply(estado$dados, names)
    anterior <- isolate(colunas_dados())
    if (!identical(assinatura, anterior)) {
      colunas_dados(assinatura)
      if (!is.null(anterior)) isolate(versao_controles(versao_controles() + 1))
    }
  })

  # Guarda no gráfico atual o que foi alterado nos controles.
  observe({
    p <- prefixo()
    req(p)
    valores <- lapply(CAMPOS, function(campo) input[[paste0(p, campo)]])
    names(valores) <- CAMPOS
    valores <- valores[!vapply(valores, is.null, logical(1))]
    if (!length(valores)) return()
    id <- isolate(estado$atual)
    spec <- isolate(estado$graficos[[id]])
    req(spec)
    if ("decimais" %in% names(valores)) valores$decimais <- suppressWarnings(as.integer(valores$decimais))
    if ("girar_x" %in% names(valores)) valores$girar_x <- as.character(valores$girar_x)
    novo <- utils::modifyList(spec, valores)
    # Trocou o conjunto de dados: escolhe variáveis válidas e redesenha os controles.
    if (!identical(novo$dados, spec$dados)) {
      tabela <- isolate(estado$dados[[novo$dados]])
      padrao <- novo_grafico(novo$nome, novo$dados, tabela)
      novo[c("x", "y")] <- padrao[c("x", "y")]
      novo[c("grupo", "faceta", "col_letras")] <- ""
      estado$graficos[[id]] <- novo
      versao_controles(isolate(versao_controles()) + 1)
      return()
    }
    if (!identical(novo, spec)) estado$graficos[[id]] <- novo
  })

  # Níveis que recebem uma cor própria (grupo ou X) no gráfico atual.
  niveis_cor <- reactive({
    p <- prefixo()
    req(p)
    id <- estado$atual
    spec <- completar_spec(estado$graficos[[id]])
    tabela <- estado$dados[[spec$dados]]
    coluna <- if (coluna_existe(tabela, spec$grupo)) spec$grupo else if (isTRUE(spec$colorir_x) && !(spec$tipo %in% TIPOS_SO_Y) && coluna_existe(tabela, spec$x)) spec$x else NULL
    niveis <- if (is.null(coluna)) character(0) else levels(como_fator(tabela[[coluna]]))
    list(p = p, coluna = coluna, niveis = niveis)
  })
  niveis_estaveis <- reactiveVal(NULL)
  observe({
    novo <- niveis_cor()
    if (!identical(novo, isolate(niveis_estaveis()))) niveis_estaveis(novo)
  })

  output$cores_niveis <- renderUI({
    info <- niveis_estaveis()
    req(info)
    p <- info$p
    paleta <- input[[paste0(p, "paleta")]] %||% "plota"
    spec <- isolate(estado$graficos[[estado$atual]])
    personalizadas <- if (identical(spec$cores_paleta, paleta)) spec$cores %||% list() else list()
    if (!length(info$niveis)) {
      return(div(class = "grade-cores",
        colourpicker::colourInput(paste0(p, "cor_unica"), "Cor", value = spec$cor_unica %||% CORES_APP$blue, showColour = "background")))
    }
    padrao <- cores_paleta(paleta, length(info$niveis))
    div(class = "caixa-cores",
      div(class = "subtitulo-cores", paste("Cor de cada nível de", texto_simples(info$coluna))),
      div(class = "grade-cores",
        lapply(seq_along(info$niveis), function(i) {
          nivel <- info$niveis[i]
          valor <- personalizadas[[nivel]] %||% padrao[i]
          colourpicker::colourInput(paste0(p, "cor_", i), texto_simples(nivel), value = valor, showColour = "background")
        })
      )
    )
  })

  # Guarda as cores dos níveis e a cor única.
  observe({
    info <- niveis_estaveis()
    req(info, identical(info$p, prefixo()))
    p <- info$p
    id <- isolate(estado$atual)
    spec <- isolate(estado$graficos[[id]])
    req(spec)
    paleta <- input[[paste0(p, "paleta")]] %||% spec$paleta
    novo <- spec
    unica <- input[[paste0(p, "cor_unica")]]
    if (!is.null(unica)) novo$cor_unica <- unica
    if (length(info$niveis)) {
      cores <- lapply(seq_along(info$niveis), function(i) input[[paste0(p, "cor_", i)]])
      if (all(!vapply(cores, is.null, logical(1)))) {
        cores_padrao <- cores_paleta(paleta, length(info$niveis))
        # Cores ainda iguais à paleta anterior (redesenho pendente) não são gravadas.
        if (identical(spec$cores_paleta, paleta) || identical(unlist(cores), cores_padrao)) {
          novo$cores <- utils::modifyList(spec$cores %||% list(), stats::setNames(cores, info$niveis))
          novo$cores_paleta <- paleta
        }
      }
    }
    if (!identical(novo, spec)) estado$graficos[[id]] <- novo
  })

  # ------------------------------------------------------------------ Tamanho de saída
  tamanho_saida <- function(prefixo_saida) {
    dpi <- suppressWarnings(as.numeric(input[[paste0(prefixo_saida, "_dpi")]] %||% 300))
    unidade <- input[[paste0(prefixo_saida, "_unidade")]] %||% "cm"
    list(
      largura = em_polegadas(input[[paste0(prefixo_saida, "_largura")]], unidade, dpi),
      altura = em_polegadas(input[[paste0(prefixo_saida, "_altura")]], unidade, dpi),
      dpi = dpi, formato = input[[paste0(prefixo_saida, "_formato")]] %||% "png",
      transparente = isTRUE(input[[paste0(prefixo_saida, "_transparente")]])
    )
  }

  for (ps in c("g", "p")) local({
    prefixo_saida <- ps
    unidade_anterior <- reactiveVal("cm")
    observeEvent(input[[paste0(prefixo_saida, "_pronto")]], {
      pronto <- TAMANHOS_PRONTOS[[input[[paste0(prefixo_saida, "_pronto")]]]]
      if (is.null(pronto$largura)) return()
      unidade_anterior(pronto$unidade)
      updateSelectInput(session, paste0(prefixo_saida, "_unidade"), selected = pronto$unidade)
      updateNumericInput(session, paste0(prefixo_saida, "_largura"), value = pronto$largura)
      updateNumericInput(session, paste0(prefixo_saida, "_altura"), value = pronto$altura)
    }, ignoreInit = TRUE)
    # Trocar a unidade converte as medidas (o tamanho físico não muda).
    observeEvent(input[[paste0(prefixo_saida, "_unidade")]], {
      nova <- input[[paste0(prefixo_saida, "_unidade")]]
      antiga <- unidade_anterior()
      unidade_anterior(nova)
      if (identical(nova, antiga)) return()
      dpi <- suppressWarnings(as.numeric(input[[paste0(prefixo_saida, "_dpi")]] %||% 300))
      fator <- em_polegadas(1, antiga, dpi) / em_polegadas(1, nova, dpi)
      casas <- if (nova == "px") 0 else if (nova == "in") 2 else 1
      for (medida in c("_largura", "_altura")) {
        valor <- as.numeric(input[[paste0(prefixo_saida, medida)]])
        if (!is.na(valor)) updateNumericInput(session, paste0(prefixo_saida, medida), value = round(valor * fator, casas))
      }
    }, ignoreInit = TRUE)
    # Medidas digitadas à mão deixam de corresponder ao tamanho pronto.
    observeEvent(list(input[[paste0(prefixo_saida, "_largura")]], input[[paste0(prefixo_saida, "_altura")]]), {
      pronto <- TAMANHOS_PRONTOS[[input[[paste0(prefixo_saida, "_pronto")]] %||% "personalizado"]]
      if (is.null(pronto$largura)) return()
      if (!isTRUE(all.equal(c(pronto$largura, pronto$altura), as.numeric(c(input[[paste0(prefixo_saida, "_largura")]], input[[paste0(prefixo_saida, "_altura")]])))) &&
          identical(input[[paste0(prefixo_saida, "_unidade")]], pronto$unidade)) {
        updateSelectInput(session, paste0(prefixo_saida, "_pronto"), selected = "personalizado")
      }
    }, ignoreInit = TRUE)

    output[[paste0(prefixo_saida, "_info")]] <- renderUI({
      t <- tamanho_saida(prefixo_saida)
      if (any(is.na(c(t$largura, t$altura, t$dpi)))) return(NULL)
      vetorial <- t$formato %in% FORMATOS_VETORIAIS
      tags$span(class = "info-saida",
        sprintf("%s × %s cm", num_br(t$largura * 2.54, 1), num_br(t$altura * 2.54, 1)),
        if (!vetorial) sprintf(" · %s × %s px", format(round(t$largura * t$dpi), big.mark = ".", decimal.mark = ","), format(round(t$altura * t$dpi), big.mark = ".", decimal.mark = ",")),
        if (vetorial) " · vetorial (texto em curvas)")
    })
  })

  # ------------------------------------------------------------------ Prévia e download do gráfico
  spec_atual <- reactive({
    id <- estado$atual
    req(id)
    estado$graficos[[id]]
  }) |> debounce(350)

  grafico_atual <- reactive({
    spec <- spec_atual()
    tabela <- estado$dados[[spec$dados %||% ""]]
    validate(need(!is.null(tabela), "Escolha um conjunto de dados."))
    tryCatch(construir_grafico(spec, tabela), error = function(e) validate(need(FALSE, conditionMessage(e))))
  })

  # A prévia é desenhada no tamanho físico da exportação, na resolução da tela.
  desenhar_previa <- function(grafico, t, largura_px, nome_saida) {
    validate(need(!any(is.na(c(t$largura, t$altura))) && t$largura > 0.2 && t$altura > 0.2, "Informe largura e altura válidas."))
    razao <- session$clientData$pixelratio %||% 1
    largura_px <- max(280, largura_px %||% 700)
    dpi <- largura_px * razao / t$largura
    arquivo <- tempfile(fileext = ".png")
    resultado <- tryCatch({
      grDevices::png(arquivo, width = round(t$largura * dpi), height = round(t$altura * dpi), res = dpi, type = if (capabilities("cairo")) "cairo" else "Xlib")
      tryCatch(imprimir_com_fontes(grafico, dpi), finally = grDevices::dev.off())
      NULL
    }, error = function(e) conditionMessage(e))
    validate(need(is.null(resultado), resultado))
    list(src = arquivo, width = "100%", contentType = "image/png", alt = nome_saida)
  }

  output$previa <- renderImage({
    grafico <- grafico_atual()
    desenhar_previa(grafico, tamanho_saida("g"), session$clientData$output_previa_width, spec_atual()$nome)
  }, deleteFile = TRUE)

  output$baixar_g <- downloadHandler(
    filename = function() nome_arquivo(estado$graficos[[estado$atual]]$nome, input$g_formato %||% "png"),
    content = function(arquivo) {
      t <- tamanho_saida("g")
      spec <- estado$graficos[[estado$atual]]
      grafico <- construir_grafico(spec, estado$dados[[spec$dados]])
      salvar_grafico(arquivo, grafico, t$formato, t$dpi, t$largura, t$altura, t$transparente)
    }
  )

  # ------------------------------------------------------------------ Painel
  # Enquanto o painel tiver todos os gráficos, os novos também entram (até 6).
  escolhas_anteriores <- reactiveVal(character(0))
  observe({
    escolhas <- nomes_estaveis()
    atuais <- isolate(input$painel_graficos) %||% character(0)
    anteriores <- isolate(escolhas_anteriores())
    selecionados <- if (setequal(atuais, anteriores)) utils::head(unname(escolhas), 6) else intersect(atuais, escolhas)
    escolhas_anteriores(unname(escolhas))
    updateSelectizeInput(session, "painel_graficos", choices = escolhas, selected = selecionados)
  })

  painel_atual <- reactive({
    ids <- intersect(input$painel_graficos, estado$ordem)
    validate(need(length(ids) >= 1, "Escolha os gráficos que vão compor o painel."))
    graficos <- lapply(ids, function(id) {
      spec <- estado$graficos[[id]]
      tryCatch(construir_grafico(spec, estado$dados[[spec$dados]]),
               error = function(e) validate(need(FALSE, sprintf("%s: %s", spec$nome, conditionMessage(e)))))
    })
    montar_painel(graficos, list(
      ncol = input$painel_ncol %||% 2, letras = input$painel_letras, tamanho_letras = input$painel_tamanho_letras,
      fonte = input$painel_fonte, titulo = input$painel_titulo, legenda_unica = input$painel_legenda_unica,
      posicao_legenda = input$painel_posicao_legenda
    ))
  }) |> debounce(400)

  output$previa_painel <- renderImage({
    desenhar_previa(painel_atual(), tamanho_saida("p"), session$clientData$output_previa_painel_width, "Painel de gráficos")
  }, deleteFile = TRUE)

  output$baixar_p <- downloadHandler(
    filename = function() nome_arquivo("painel", input$p_formato %||% "png"),
    content = function(arquivo) {
      t <- tamanho_saida("p")
      salvar_grafico(arquivo, painel_atual(), t$formato, t$dpi, t$largura, t$altura, t$transparente)
    }
  )

  # ------------------------------------------------------------------ Projeto
  output$salvar_projeto <- downloadHandler(
    filename = function() paste0("projeto-plota-", format(Sys.Date(), "%Y-%m-%d"), ".plota"),
    content = function(arquivo) {
      painel <- list(graficos = match(input$painel_graficos, estado$ordem), ncol = input$painel_ncol, letras = input$painel_letras,
                     tamanho_letras = input$painel_tamanho_letras, fonte = input$painel_fonte, titulo = input$painel_titulo,
                     legenda_unica = input$painel_legenda_unica, posicao_legenda = input$painel_posicao_legenda)
      writeLines(projeto_para_json(estado$dados, estado$graficos, estado$ordem, painel), arquivo, useBytes = TRUE)
    },
    contentType = "application/json"
  )

  observeEvent(input$abrir_projeto, {
    tryCatch({
      projeto <- json_para_projeto(input$abrir_projeto$datapath)
      if (!length(projeto$dados)) stop("O projeto não contém dados.")
      estado$dados <- projeto$dados
      estado$graficos <- list()
      estado$ordem <- character(0)
      estado$contador <- 0
      mapa <- character(0)
      for (spec in projeto$graficos) {
        estado$contador <- estado$contador + 1
        id <- paste0("g", estado$contador)
        estado$graficos[[id]] <- spec
        estado$ordem <- c(estado$ordem, id)
      }
      estado$atual <- estado$ordem[1] %||% NULL
      updateSelectInput(session, "dados_ativo", choices = names(estado$dados), selected = names(estado$dados)[1])
      versao_planilha(versao_planilha() + 1)
      versao_controles(versao_controles() + 1)
      pn <- projeto$painel
      if (length(pn$graficos)) {
        indices <- suppressWarnings(as.integer(sub("^g", "", unlist(pn$graficos))))
        updateSelectizeInput(session, "painel_graficos", selected = paste0("g", indices[!is.na(indices)]))
      }
      if (!is.null(pn$ncol)) updateNumericInput(session, "painel_ncol", value = pn$ncol)
      if (!is.null(pn$letras)) updateSelectInput(session, "painel_letras", selected = pn$letras)
      if (!is.null(pn$tamanho_letras)) updateNumericInput(session, "painel_tamanho_letras", value = pn$tamanho_letras)
      if (!is.null(pn$fonte)) updateSelectInput(session, "painel_fonte", selected = pn$fonte)
      if (!is.null(pn$titulo)) updateTextInput(session, "painel_titulo", value = pn$titulo)
      if (!is.null(pn$legenda_unica)) updateCheckboxInput(session, "painel_legenda_unica", value = isTRUE(pn$legenda_unica))
      if (!is.null(pn$posicao_legenda)) updateSelectInput(session, "painel_posicao_legenda", selected = pn$posicao_legenda)
      updateTabsetPanel(session, "abas", selected = "graficos")
      showNotification(sprintf("Projeto aberto: %d conjunto(s) de dados e %d gráfico(s).", length(estado$dados), length(estado$ordem)), type = "message")
    }, error = function(e) showNotification(paste("Não foi possível abrir o projeto:", conditionMessage(e)), type = "error", duration = 8))
  })
}
