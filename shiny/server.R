###############################################################################
############################ CARTEIRA EASY INVEST #############################
##########################   SERVER SIDE FUNCTIONS  ###########################
###############################################################################

shinyServer(function(input, output, session) {
  
  # observar o evento que cria uma nova carteira
  # iniciando um objeto reativo para guardar os objetos de salvamento
  
  obj.reac <<- reactiveValues()
  
  # carteiras disponiveis
  
  buscarCarteiras <<- function() {
    
    df <- data.frame(
      "nome_carteira" = list.dirs(
        path = "../bases/"
        ,full.names = FALSE
      )
      ,"path_carteira" = list.dirs(
        path = "../bases/"
        ,full.names = TRUE
      )
      ,stringsAsFactors = FALSE
    ) %>% 
      filter(
        str_detect(
          string = nome_carteira
          ,pattern = "bases/$"
          ,negate = TRUE
        )
      )
    
    return(df)
  }
  
  obj.reac$carteira.salvas <<- buscarCarteiras()
  
  # observar o evento para criar uma nova carteira
  
  output$opcoes_carteira_ui <- renderUI({
    
    selectInput(
      inputId = "selecionar_carteira"
      ,label = "Selecione a Carteira"
      ,choices = c("", obj.reac$carteira.salvas$nome_carteira)
      ,selected = ""
      ,selectize = TRUE
      ,multiple = FALSE
    )    
    
  })
  
  observeEvent(input$criar_carteira_botao, {
    
    showModal(
      modalDialog(
        wellPanel(
          textInput(
            inputId = "nome_carteira_nova"
            ,label = "Insira um nome para sua carteira: "
            ,value = ""
            ,placeholder = "ex: Carteira XP"
          )
        )
        ,easyClose = FALSE
        ,title = "Criando nova carteira"
        ,footer = tagList(
          actionButton(
            inputId = "confirmar_carteira_nova"
            ,label = "Confirmar"
          )
          ,modalButton(
            label = "Fechar"
          )
        )
      )
    )
  })
  
  # carregando os arquivos
  
  carregar_carteira <<- expression({
    withProgress({
      obj.reac$carteira$carteira <<- carregarRDS.carteira(
        tp = "carteira"
        ,nome_carteira = obj.reac$carteira$selecionada
      )
      
      obj.reac$carteira$operacoes <<- carregarRDS.carteira(
        tp = "operacoes"
        ,nome_carteira = obj.reac$carteira$selecionada
      )
      
      obj.reac$carteira$series_historicas <<- carregarRDS.carteira(
        tp = "series_historicas"
        ,nome_carteira = obj.reac$carteira$selecionada
      ) 
    }, message = "Carregando Carteira...")
  })
  
  # salvando os arquivos
  
  salvar_carteira <<- expression({
    withProgress({
      salvarRDS.carteira(
        df = obj.reac$carteira$carteira
        ,nome_carteira = obj.reac$carteira$selecionada
        ,tp = "carteira"
      )
      
      salvarRDS.carteira(
        df = obj.reac$carteira$operacoes
        ,nome_carteira = obj.reac$carteira$selecionada
        ,tp = "operacoes"
      )
      
      salvarRDS.carteira(
        df = obj.reac$carteira$series_historicas
        ,nome_carteira = obj.reac$carteira$selecionada
        ,tp = "series_historicas"
      )    
    }, message = "Salvando Carteira...")
  })
  
  atualizar_carteira <<- expression({
    withProgress({
      l <- atualizarCarteira(
        nome_carteira = obj.reac$carteira$selecionada
        ,obj.reac = obj.reac
      )
      
      obj.reac$carteira$series_historicas <<- l$series_historicas
      obj.reac$carteira$operacoes <<- l$operacoes
      obj.reac$carteira$carteira <<- l$carteira
      
      rm(l)
    }, message = "Atualizando Carteira...")
  })  
  
  observeEvent(input$confirmar_carteira_nova, {

    req(input$nome_carteira_nova)
    
    if (
      input$nome_carteira_nova %in% obj.reac$carteira.salvas$nome_carteira
    ) {
      
      removeModal()
      
      showModal(
        modalDialog(
          HTML(
            "Você está tentando inserir um nome repetido!<br>
            Insira um nome diferente.
            ")
          ,easyClose = TRUE
          ,footer = modalButton("Fechar")
        )
      )
      
    } else {
      
      removeModal()
      
      # atribuindo o nome da carteira para um objeto reativo
      
      obj.reac$carteira <<- list()
      
      obj.reac$carteira$selecionada <- as.character(input$nome_carteira_nova)
      
      # criando os arquivos necessarios para carteira
      
      criarCarteira(obj.reac$carteira$selecionada)
      
      obj.reac$carteira.salvas <<- buscarCarteiras()
      
      eval(carregar_carteira)
      
      updateSelectInput(
        session = session
        ,inputId = "selecionar_carteira"
        ,selected = obj.reac$carteira$selecionada
        ,choices = c(
          ""
          ,obj.reac$carteira.salvas
        )
        ,label = "Selecione a Carteira"
      )
      
      showModal(
        modalDialog(
          "Carteira Criada com sucesso"
          ,easyClose = TRUE
          ,footer = modalButton("Fechar")
        )
      )
    }
  })
  
  observeEvent(input$carrega_carteira_botao, {

    req(input$selecionar_carteira)
    
    obj.reac$carteira$selecionada <<- as.character(input$selecionar_carteira)
    
    # carregando os arquivos
    
    eval(carregar_carteira)
    
    showModal(
      modalDialog(
        "Carteira carregada com sucesso"
        ,easyClose = TRUE
        ,footer = modalButton("Fechar")
      )
    )
  })
  
  
  observe({
    
    output$evolucao_carteira_grafico_ui <- renderPlot({
      
      req(obj.reac$carteira$operacoes)
      
      carteira_evolucao <- evolucaoCarteira(
        carteira = obj.reac$carteira$operacoes
        ,l_data = obj.reac$carteira$series_historicas
      ) %>% 
        group_by(
          Data
        ) %>% 
        summarise(
          Valor = sum(Valor * qtd_cotas)
        ) 
      
      # carteira_evolucao$var <- c(0, (diff(carteira_evolucao$Valor) / 
      #   carteira_evolucao$Valor)[-length(carteira_evolucao)])
      
      carteira_evolucao$label <- NA
      
      print(c(
              seq(
                from = 1
                ,to = (length(carteira_evolucao$Valor)-1)
                ,by = 3
              )
              ,length(carteira_evolucao$Valor)
            ))
      
      carteira_evolucao$label[
        c(
          seq(
            from = 1
            ,to = (length(carteira_evolucao$Valor)-1)
            ,by = 3
          )
          ,length(carteira_evolucao$Valor)
        )
      ] <- paste0(
        "R$ "
        ,format(
          x = carteira_evolucao$Valor[
            c(
              seq(
                from = 1
                ,to = (length(carteira_evolucao$Valor)-1)
                ,by = 3
              )
              ,length(carteira_evolucao$Valor)
            )
          ]
          ,digits = 2
          ,big.mark = "."
          ,decimal.mark = ","
        )
      )
      
      xlim_ev <- c(
        min(carteira_evolucao$Data) - 2
        ,max(carteira_evolucao$Data) + 2
      )
      
      ggplot(
        data = carteira_evolucao
        ,mapping =  aes(
          x = Data, y = Valor, label = label
      ))+
        geom_line(color = "firebrick")+
        geom_label()+
        ggtitle(
          label = paste0("Evolução Carteira: ", obj.reac$carteira.selecionada)
        )+
        theme(
          axis.text = element_blank()
          # ,axis.text.x = element_text(
          #   face = "bold"
          #   ,size = 12
          #   ,family = "consolas"
          #   
          # )
        )+
        theme_economist(base_family = "consolas")+
        ylab("")+
        xlab("")+
        xlim(xlim_ev)
    })
    
    output$composicao_carteira_pie_ui <- renderPlot({
      
      req(obj.reac$carteira$operacoes)
      
      obj.reac$carteira$operacoes %>% 
        mutate(
          vl_atual = as.numeric(vl_atual)
          ,qtd_cotas = as.numeric(qtd_cotas)
        ) %>% 
        group_by(tp_invest) %>% 
        summarise(
          valor = sum(vl_atual * qtd_cotas)
        ) %>% 
        ggplot(
          mapping = aes(
            x = ""
            ,y = valor
            ,fill = factor(tp_invest)
            ,label = paste0(
              "R$ "
              ,format(
                x = valor
                ,digits = 2
                ,big.mark = "."
                ,decimal.mark = ","
              )
            )
          )
        )+
        geom_bar(width = 200, stat = "identity")+
        geom_label(position = position_stack(
          vjust = 0.5
          )
          ,show.legend = FALSE
        )+
        theme_economist(
          base_family = "consolas"
          ,horizontal = TRUE
          ,dkpanel = FALSE
        )+
        theme(
          axis.title.x = element_blank()
          ,axis.text.x = element_blank()
          ,axis.ticks.x = element_blank()
          ,axis.title.y = element_blank()
          ,axis.text.y = element_blank()
          ,axis.ticks.y = element_blank()
          ,panel.background = element_rect(colour = "white")
          ,plot.background = element_rect(
            color = "#ffffff"
            ,fill = "#ffffff"
            ,colour = "#ffffff"
          )
          # ,legend.position = "none",
          # ,legend.position = "left"
        )+
        xlab("")+
        ylab("")+
        guides(
          fill = guide_legend(
            title = ""
            # ,label = ""
            # ,direction = "vertical"
            # ,
          )
        )+
        coord_polar(
          theta = "y"
          ,start = 0
          ,clip = "on"
          ,direction = 1
        )
      
      
    })
    
    output$composicao_carteira <- DT::renderDataTable({
      
      DT::datatable(
        data = obj.reac$carteira$carteira,
        colnames = c(
          "Cod Título", "Nome Título", "Tipo Título", "Vl Compra",
          "Vl Atual", "Var (dia)", "Qtde", "Rendimento",
          "Ganhos"
        )
        ,rownames = NULL
        ,class = "display compact nowrap dataTable no-footer dtr-inline collapsed"
        ,escape = FALSE
        ,selection = "single"
        ,options = list(
          dom = "t"
          ,pageLength = 30
          ,autoWidth = TRUE
          ,columnDefs = list(
            list(
              className = 'dt-center'
              ,targets = 0:8
            )
          )
        )
      )
    })

    output$operacoes_tabela <- renderRHandsontable({

      req(obj.reac$carteira$operacoes)

      rhandsontable(
        data = obj.reac$carteira$operacoes
        ,colHeaders = c(
          "Código", "Nome", "Código Yahoo", "Data Compra", "Valor Compra"
          ,"Qtde", "Tipo", "Valor Atual", "Última Atualização"
          ,"Rendimento","Var (dia)"
        )
        ,rowHeaders = NULL
        ,className = "htCenter"
      ) %>%
        rhandsontable::hot_cols(
          colWidths = c(70,220,100,80,60,80,150,80,140,100,60,80)
        )

    })

    output$consolidado_carteira <- renderValueBox({

      req(obj.reac$carteira$operacoes)

      valor_consolidado <- round(
        x = sum(
          ((
              as.numeric(
                obj.reac$carteira$operacoes$vl_atual
            ) *
              as.numeric(
                obj.reac$carteira$operacoes$qtd_cotas
              )
            ) -
            (
              as.numeric(
                obj.reac$carteira$operacoes$vl_compra
            ) *
              as.numeric(
                obj.reac$carteira$operacoes$qtd_cotas
              )
            )
          )
        )
        ,digits =  2
      )

      valueBox(
        value = paste0(
          "R$ ", format(
            x = valor_consolidado
            ,format = "f"
            ,big.mark = "."
            ,decimal.mark = ","
          )
        )
        ,subtitle = "Rendimento (R$)"
      )
    })

    output$rendimento_carteira <- renderValueBox({

      req(obj.reac$carteira$operacoes)

      rendimento_carteira <- round(
        x = ((
          sum(
            as.numeric(
              obj.reac$carteira$operacoes$vl_atual
            ) *
            as.numeric(
              obj.reac$carteira$operacoes$qtd_cotas
            )
          )) /
        sum(
          as.numeric(
              obj.reac$carteira$operacoes$vl_compra
          ) *
            as.numeric(
              obj.reac$carteira$operacoes$qtd_cotas
            )
        )) - 1
        ,digits =  4
      ) * 100

      valueBox(
        value = paste0(rendimento_carteira, "%"),
        subtitle = "Rendimento (%)"
      )
    })

    output$total_carteira <- renderValueBox({

      req(obj.reac$carteira$operacoes)

      rendimento_carteira <- round(
        x = sum(
          as.numeric(
            obj.reac$carteira$operacoes$vl_atual
          ) *
          as.numeric(
            obj.reac$carteira$operacoes$qtd_cotas
          )
        )
        ,digits =  2
      )

      valueBox(
        value = paste0(
          "R$"
          ,format(
            x = rendimento_carteira
            ,digits = 0
            ,format = "f"
            ,big.mark = "."
            ,decimal.mark = ","
          )
        ),
        subtitle = "Valor da Carteira (R$)"
      )
    })
  })

  observeEvent(input$salvar_carteira_botao, {
    
    req(input$selecionar_carteira)
    
    obj.reac$carteira$operacoes <<- 
      hot_to_r(input$operacoes_tabela)
    
    if (
      
      nrow(
        obj.reac$carteira$operacoes
      ) == 1 & obj.reac$carteira$operacoes$cod_tit[1] == ""
    
    ) {
      
      showModal(
        modalDialog(
          "Insira valores na tabela de operações para efetivar o salvamento!"
          ,easyClose = TRUE
          ,footer = modalButton(
            label = "Fechar"
          )
        )
      )
      
    } else {
      
      if (
          obj.reac$carteira$carteira[1] == ""
        ) {
        
        showModal(
          modalDialog(
            "Gostaria de atualizar a carteira e as séries históricas?"
            ,footer = tagList(
              actionButton(
                inputId = "atualizar_carteira_botao_modal"
                ,label = "Confirmar"
              )
              ,actionButton(
                inputId = "apenas_salvar_carteira"
                ,label = "Não"
              )
              ,modalButton(
                label = "Cancelar"
              )
            )
          )
        )
        
      } else {
        
        eval(salvar_carteira)
      }
    }
  })
  
  observeEvent(input$atualizar_carteira_botao_modal, {
    
    removeModal()
    
    obj.reac$carteira$operacoes <<- 
      hot_to_r(input$operacoes_tabela)
        
    eval(atualizar_carteira)
    
    eval(salvar_carteira)
    
    showModal(
      modalDialog(
        "Carteira atualizada e salva com sucesso!"
        ,easyClose = TRUE
        ,footer = modalButton("Fechar")
      )
    )
  })
  
  observeEvent(input$atualizar_carteira_botao, {
    
    req(input$selecionar_carteira)
    
    if (
      nrow(
        obj.reac$carteira$operacoes
      ) <= 1 & obj.reac$carteira$operacoes[1] == ""

    ) {
      
      showModal(
        modalDialog(
          HTML(
            "Você não possui operações válidas!<br>
            Adicione operações para fazer atualização da carteira
            "
          )
        )
      )
      
    } else {
      
      eval(atualizar_carteira)
      
      
      showModal(
        modalDialog(
          "Carteira atualizada e salva com sucesso!"
          ,easyClose = TRUE
          ,footer = modalButton("Fechar")
        )
      )
    }
  })
  
  observeEvent(input$apenas_salvar_carteira, {
    
    removeModal()
    
    eval(salvar_carteira)
    
    showModal(
      modalDialog(
        "Carteira salva com sucesso!"
        ,easyClose = TRUE
        ,footer = modalButton("Fechar")
      )
    )
  })
})
