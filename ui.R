# Formato, resolução e tamanho para baixar um gráfico ou um painel.
controles_exportacao <- function(prefixo, largura = 17.5, altura = 11, rotulo = "Baixar gráfico") {
  div(
    class = "caixa-exportacao",
    div(class = "exportacao-titulo", icon("download"), "Tamanho e formato de saída"),
    div(class = "grade-exportacao",
      selectInput(paste0(prefixo, "_pronto"), "Tamanho pronto", choices = OPCOES_TAMANHOS, selected = "coluna2"),
      div(class = "grade-medidas",
        numericInput(paste0(prefixo, "_largura"), "Largura", value = largura, min = 0.1, step = 0.5),
        numericInput(paste0(prefixo, "_altura"), "Altura", value = altura, min = 0.1, step = 0.5),
        selectInput(paste0(prefixo, "_unidade"), "Unidade", choices = UNIDADES, selected = "cm")
      ),
      div(class = "grade-medidas",
        selectInput(paste0(prefixo, "_formato"), "Formato", choices = FORMATOS, selected = "tiff"),
        selectizeInput(paste0(prefixo, "_dpi"), "Resolução (dpi)", choices = c(72, 150, 300, 600, 1200), selected = 300,
                       options = list(create = TRUE, createFilter = "^[0-9]+$")),
        div(class = "caixa-transparente", checkboxInput(paste0(prefixo, "_transparente"), "Fundo transparente", FALSE))
      )
    ),
    div(class = "exportacao-rodape",
      uiOutput(paste0(prefixo, "_info"), inline = TRUE),
      downloadButton(paste0("baixar_", prefixo), rotulo, icon = icon("download"), class = "btn-principal")
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/app.css"),
    tags$meta(name = "author", content = "Marlenildo"),
    tags$meta(name = "description", content = "Plota: gráficos elegantes para a pesquisa agronômica, prontos para artigos, teses e apresentações."),
    tags$link(rel = "icon", type = "image/png", href = "img/favicon.png"),
    tags$title("Plota · Gráficos elegantes para a pesquisa")
  ),

  div(class = "cabecalho-app",
    tags$img(src = "img/logo_app.png", class = "logo-app", alt = "Logo do Plota"),
    div(class = "titulo-area",
      div(class = "titulo", "Plota"),
      div(class = "descricao-app", "Gráficos elegantes para a pesquisa agronômica"),
      div(class = "subtitulo", "Barras, linhas, interação, regressão e boxplot com fonte, cores e tamanho sob medida. Agrupe em painéis e baixe em alta resolução.")
    ),
    div(class = "acoes-projeto",
      downloadButton("salvar_projeto", "Salvar projeto", icon = icon("floppy-disk"), class = "btn-secundario"),
      div(class = "abrir-projeto",
        fileInput("abrir_projeto", NULL, accept = c(".plota", ".json"), buttonLabel = tagList(icon("folder-open"), "Abrir projeto"),
                  placeholder = NULL, width = "100%"))
    )
  ),

  tabsetPanel(
    id = "abas", type = "tabs",

    # ---------------------------------------------------------------- Dados
    tabPanel(
      title = tagList(tags$span(class = "numero-aba", 1), "Dados"), value = "dados",
      fluidRow(
        class = "grade-trabalho",
        column(4,
          cartao(1, "Adicionar dados",
            tabsetPanel(
              id = "origem_dados", type = "pills",
              tabPanel("Colar", value = "colar",
                div(class = "explicacao espaco-topo", HTML("Copie as células no Excel (com a <b>linha de títulos</b>) e cole abaixo. Vírgula decimal é aceita.")),
                textAreaInput("texto_colado", NULL, rows = 8, width = "100%",
                              placeholder = "Tratamento\tDose\tProdutividade\nT1\t0\t18,4\nT2\t50\t24,1"),
                textInput("nome_colado", "Nome do conjunto", placeholder = "Ex.: Experimento 1"),
                actionButton("adicionar_colado", "Adicionar dados", icon = icon("plus"), class = "btn-principal btn-bloco")
              ),
              tabPanel("Arquivo", value = "arquivo",
                div(class = "explicacao espaco-topo", "Planilha do Excel (.xlsx, .xls) ou texto (.csv), com os títulos na primeira linha."),
                fileInput("arquivo_dados", NULL, accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"),
                          buttonLabel = "Escolher arquivo", placeholder = "Nenhum arquivo", width = "100%"),
                uiOutput("abas_arquivo"),
                actionButton("adicionar_arquivo", "Adicionar dados", icon = icon("plus"), class = "btn-principal btn-bloco")
              ),
              tabPanel("Exemplos", value = "exemplos",
                div(class = "explicacao espaco-topo", "Experimentos agronômicos fictícios para explorar os gráficos."),
                selectInput("exemplo", NULL, choices = names(EXEMPLOS), width = "100%"),
                actionButton("adicionar_exemplo", "Carregar exemplo", icon = icon("seedling"), class = "btn-principal btn-bloco")
              )
            )
          )
        ),
        column(8,
          cartao(2, "Conjuntos de dados", classe = "painel-visual",
            acoes = uiOutput("resumo_dados", inline = TRUE),
            div(class = "linha-conjunto",
              selectInput("dados_ativo", NULL, choices = NULL, width = "100%"),
              actionButton("renomear_dados", NULL, icon = icon("pen"), class = "btn-icone", title = "Renomear conjunto"),
              actionButton("excluir_dados", NULL, icon = icon("trash-can"), class = "btn-icone btn-perigo", title = "Excluir conjunto")
            ),
            div(class = "planilha", rHandsontableOutput("planilha")),
            div(class = "explicacao nota-planilha", "Edite as células à vontade · Ctrl+V cola do Excel · botão direito insere ou remove linhas e colunas.")
          )
        )
      )
    ),

    # ---------------------------------------------------------------- Gráficos
    tabPanel(
      title = tagList(tags$span(class = "numero-aba", 2), "Gráficos"), value = "graficos",
      div(class = "barra-graficos",
        div(class = "lista-graficos", uiOutput("lista_graficos")),
        div(class = "acoes-graficos",
          actionButton("novo_grafico", "Novo", icon = icon("plus"), class = "btn-principal"),
          actionButton("duplicar_grafico", "Duplicar", icon = icon("clone"), class = "btn-secundario"),
          actionButton("estilo_todos", "Estilo em todos", icon = icon("wand-magic-sparkles"), class = "btn-secundario",
                       title = "Aplica fonte, cores, tema e tamanhos deste gráfico a todos os outros"),
          actionButton("excluir_grafico", NULL, icon = icon("trash-can"), class = "btn-icone btn-perigo", title = "Excluir gráfico")
        )
      ),
      fluidRow(
        class = "grade-trabalho",
        column(5, div(class = "painel cartao coluna-controles", uiOutput("controles"))),
        column(7,
          div(class = "coluna-previa",
            cartao(NULL, "Prévia", icone = "eye", classe = "painel-visual",
              acoes = div(class = "tag-secao", "Como será baixado"),
              div(class = "area-previa", imageOutput("previa", height = "auto")),
              controles_exportacao("g")
            )
          )
        )
      )
    ),

    # ---------------------------------------------------------------- Painel
    tabPanel(
      title = tagList(tags$span(class = "numero-aba", 3), "Painel"), value = "painel",
      fluidRow(
        class = "grade-trabalho",
        column(4,
          cartao(NULL, "Montar painel", icone = "table-cells-large", classe = "painel-grupos",
            div(class = "explicacao", "Escolha os gráficos na ordem em que devem aparecer. Eles recebem letras (A, B, C...) como nas figuras de artigos."),
            selectizeInput("painel_graficos", "Gráficos do painel", choices = NULL, multiple = TRUE, width = "100%",
                           options = list(plugins = list("remove_button", "drag_drop"), placeholder = "Escolha dois ou mais gráficos")),
            div(class = "grade-campos",
              numericInput("painel_ncol", "Colunas", value = 2, min = 1, max = 6, step = 1),
              selectInput("painel_letras", "Identificação", choices = ESTILOS_LETRAS, selected = "A"),
              numericInput("painel_tamanho_letras", "Tamanho das letras", value = 14, min = 6, max = 40, step = 1),
              selectInput("painel_fonte", "Fonte das letras", choices = OPCOES_FONTES, selected = "arial")
            ),
            textInput("painel_titulo", "Título geral (opcional)", width = "100%"),
            checkboxInput("painel_legenda_unica", "Juntar legendas iguais em uma só", TRUE),
            conditionalPanel("input.painel_legenda_unica",
              selectInput("painel_posicao_legenda", "Posição da legenda comum",
                          choices = c("Abaixo" = "bottom", "À direita" = "right", "Acima" = "top"), selected = "bottom"))
          )
        ),
        column(8,
          cartao(NULL, "Prévia do painel", icone = "eye", classe = "painel-visual",
            acoes = div(class = "tag-secao", "Como será baixado"),
            div(class = "area-previa", imageOutput("previa_painel", height = "auto")),
            controles_exportacao("p", largura = 17.5, altura = 14, rotulo = "Baixar painel")
          )
        )
      )
    ),

    # ---------------------------------------------------------------- Ajuda
    tabPanel(
      title = tagList(icon("circle-question"), "Ajuda"), value = "ajuda",
      div(class = "painel cartao pagina-ajuda",
        h4(class = "titulo-cartao", icon("lightbulb"), "Dicas rápidas"),
        tags$ul(
          tags$li(HTML("<b>Expoentes e índices:</b> escreva <code>kg ha^-1</code>, <code>m^2</code> ou <code>CO_2</code> em títulos, nomes de colunas e níveis. O Plota desenha kg ha<sup>−1</sup>, m<sup>2</sup> e CO<sub>2</sub>. Para mais de um caractere, use chaves: <code>x^{0,5}</code>.")),
          tags$li(HTML("<b>Letras de médias:</b> inclua na planilha uma coluna com as letras do teste (Tukey, Scott-Knott...) e escolha <i>Rótulos → Letras de uma coluna</i>. Cada combinação de tratamento usa a primeira letra encontrada.")),
          tags$li(HTML("<b>Interação:</b> use <i>Linhas</i> ou <i>Barras</i> com um fator no eixo X e outro em <i>Cor (grupo)</i>. Para doses, escolha uma <i>Regressão</i> e veja a equação e o R².")),
          tags$li(HTML("<b>Várias variáveis:</b> duplique o gráfico e troque só o eixo Y; o estilo é mantido. O botão <i>Estilo em todos</i> iguala fonte, cores e tema de todos os gráficos.")),
          tags$li(HTML("<b>Painel:</b> na aba 3, junte os gráficos em uma figura única com letras A, B, C e uma só legenda.")),
          tags$li(HTML("<b>Publicação:</b> a maioria das revistas pede TIFF ou PNG a 300–600 dpi, ou PDF/EPS vetorial. A prévia usa o mesmo tamanho da exportação, então o que você vê é o que será baixado.")),
          tags$li(HTML("<b>Projeto:</b> <i>Salvar projeto</i> guarda dados e gráficos em um arquivo <code>.plota</code> no seu computador para continuar depois."))
        )
      )
    )
  ),

  div(class = "nota-privacidade", icon("lock"), span(TEXTO_PRIVACIDADE)),

  div(class = "rodape-app",
    span("Desenvolvido por"),
    tags$img(src = "img/logo_marlenildo.png", class = "logo-rodape", alt = "Marlenildo Soluções em Curso"),
    span(class = "versao-app",
      tags$a(href = "https://github.com/Marlenildo/plota/blob/main/CHANGELOG.md", target = "_blank", rel = "noopener",
             title = "Ver novidades desta versão", paste0("Plota v", VERSAO_APP)))
  )
)
