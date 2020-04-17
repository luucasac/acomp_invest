###############################################################################
############################ CARTEIRA EASY INVEST #############################
############################ INTERFACE FUNCTIONS   ############################
###############################################################################

header.actionButton <- function(title, inputId, icon) {
  
  return(
    div(
      title = title
      ,actionButton(
        inputId = inputId
        ,label = ""
        ,style = "margin-left: 10px;margin-top: 12px"
        ,width = "50px"
        ,height = "40px"
        ,icon = icon
      )
    )
  )
}


shinyUI(
  dashboardPage(
    header = dashboardHeader(
      title = "Carteiras de Investimento"
      ,disable = TRUE
    ),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      div(
        style = 
        "
            width: 650px;
            height: 100px;
            border: 0.25px solid #bccadc;
            padding: 10px 30px 15px 10px;
            display: table-cell;
            
        "
        ,uiOutput(
          outputId = "opcoes_carteira_ui"
        )
        ,div(style = "display:inline-flex;"
          ,header.actionButton(
            title = "Criar Carteira"
            ,inputId = "criar_carteira_botao"
            ,icon = icon("plus")
          )
          ,header.actionButton(
            title = "Carregar Carteira"
            ,inputId = "carrega_carteira_botao"
            ,icon = icon("upload")
          )
          ,header.actionButton(
            title = "Atualizar Carteira"
            ,inputId = "atualizar_carteira_botao"
            ,icon = icon("sync")
          )
          ,header.actionButton(
            title = "Gráficos"
            ,inputId = "graficos_acao_botao"
            ,icon = icon("chart-area")
          )
          ,header.actionButton(
            title = "Salvar Carteira"
            ,inputId = "salvar_carteira_botao"
            ,icon = icon("save")
          )
        )
      )
      ,tabsetPanel(
        tabPanel(
          title = "Carteira - Operações"
          ,br()
          ,fluidPage(
            column(
              width = 11
              ,rHandsontableOutput("operacoes_tabela")
            )
            ,column(
              width = 1
              ,div(
                style = "display: inline-block"
                ,header.actionButton(
                  title = "Adicionar Operação"
                  ,inputId = "add_operacao_bt"
                  ,icon = icon("plus")
                )
              )
            )
          )
        )
        ,tabPanel(
          title = "Carteira - Acompanhamento"
          ,br()  
          ,fluidPage(
            fluidRow(style = "height:700px; ",
              column(width = 7,
                splitLayout(
                  valueBoxOutput("consolidado_carteira", width = "100%")
                  ,valueBoxOutput("total_carteira", width = "100%")
                  ,valueBoxOutput("rendimento_carteira", width = "100%")
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
            )
          )
        )
      )
    )
  )
)
