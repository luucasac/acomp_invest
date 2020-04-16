###############################################################################
############################ CARTEIRA EASY INVEST #############################
############################ INTERFACE FUNCTIONS   ############################
###############################################################################

shinyUI(
  dashboardPage(
    header = dashboardHeader(title = "Carteiras de Investimento", disable = TRUE),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      tabsetPanel(
        tabPanel("Carteira - Acompanhamento",
          br()  
          ,fluidPage(
            fluidRow(style = "height:700px; ",
              column(width = 7,
                splitLayout(
                  valueBoxOutput("consolidado_carteira", width = "100%")
                  ,valueBoxOutput("total_carteira", width = "100%")
                  ,valueBoxOutput("rendimento_carteira", width = "100%")
                )
                ,div(style = "display: -webkit-box;",
                  uiOutput("opcoes_carteira_ui")
                  ,div(style = "margin-top: 25px; margin-left: 20px;",
                    actionButton("criar_carteira_botao", label = "", icon = icon("plus"))
                    ,actionButton("carrega_carteira_botao", label = "", icon = icon("upload"))
                    ,actionButton("atualizar_carteira_botao", label = "", icon = icon("sync"))
                    ,actionButton("graficos_acao_botao", label = "", icon = icon("chart-area"))
                    ,actionButton("salvar_carteira_botao", label = "", icon = icon("save"))
                  )
                )
                ,br()
                ,DT::dataTableOutput("composicao_carteira", width = "100%")
              )
              ,column(width = 5,
                plotOutput(
                  outputId = "evolucao_carteira_grafico_ui"
                  ,height = "320px"
                )
                ,br()
                ,plotOutput(
                  outputId = "composicao_carteira_pie_ui"
                  ,height = "320px"
                )
              )
              ,br()
              ,column(width = 11,
                rhandsontable::rHandsontableOutput("operacoes_tabela")
              )
            )
          )
        )
      )
    )
  )
)
