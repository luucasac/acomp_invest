###############################################################################
############################## FUNCOES ANALISE ################################
###############################################################################


evolucaoCarteira <- function(carteira, l_data) {
  
  tratamentoTS <- function(ts_data) {
    
    df <- as.data.frame(ts_data, stringsAsFactos = FALSE)
    
    df$data <- as.Date(rownames(ts_data))
    
    rownames(df) <- NULL
    
    acao_nome <- str_extract(names(ts_data)[1], "^[^\\.]+")
    
    {df <- df[
      df$data >= min(
        as.Date(
          carteira$dt_compra[
            str_extract(
              string = carteira$cod_tit_yahoo
              ,pattern =  "^[^\\.]+"
            ) == acao_nome
          ]
        )
        )
    ,] %>% 
      select(contains("Close", ignore.case = TRUE), data) %>% 
      mutate(Ativo = acao_nome) %>% 
      `colnames<-`(c("Valor", "Data", "Ativo"))}
    
    return(df)
    
  }
  
  evolucao <- bind_rows(lapply(l_data, tratamentoTS))
  
  evolucao$Data <- as.Date(evolucao$Data)
  
  evolucao <- evolucao %>% 
    bind_rows(carteira %>% 
    mutate(
      Ativo = str_remove(cod_tit_yahoo, "\\.SA")
      ,Data = as.Date(substr(ult_atualizacao, 1, 10))
    ) %>% 
    select(Valor = vl_atual, Data, Ativo)) %>% 
    arrange(Ativo, Data)
    
  evolucao_carteira_detalhe <- evolucao %>% 
    left_join(
      select(carteira, cod_tit_yahoo, qtd_cotas, dt_compra) %>% 
        mutate(cod_tit_yahoo = str_remove(cod_tit_yahoo, "\\.SA"),
          dt_compra = as.Date(dt_compra))
      ,by = c("Ativo" = "cod_tit_yahoo", "Data" = "dt_compra")
    ) %>% 
    mutate(
      qtd_cotas = ifelse(is.na(qtd_cotas), 0, qtd_cotas)
    ) %>% 
    group_by(
      Data, Ativo, Valor
    ) %>% 
    summarise(
      qtd_cotas = sum(as.numeric(qtd_cotas))
    ) %>% 
    arrange(Ativo, Data) %>% 
      group_by(
      Data, Ativo
    )
  
  lista_Nova <- split(evolucao_carteira_detalhe, f = evolucao_carteira_detalhe$Ativo)
  
  evolucao_carteira <- bind_rows(lapply(lista_Nova, function(df) {
    
    df$qtd_cotas <- cumsum(df$qtd_cotas)
      
    return(df)
  }))
  
  return(evolucao_carteira)
  
}

consolidaCarteira <- function(evolucao_carteira) {
  
  evolucao_carteira_consolidado <- evolucao_carteira %>% 
    group_by(Data) %>% 
    summarise(
      Valor = round(sum(Valor * qtd_cotas), 2))
  
  return(evolucao_carteira_consolidado)
  
}

graficoEvolucaoCarteira <- function(evolucao_carteira) {
  
  evolucao_carteira_consolidado <- consolidaCarteira(evolucao_carteira)
  
  evolucao_carteira_consolidado$Data <- as.Date(
    evolucao_carteira_consolidado$Data
  )
  
  evolucao_carteira_consolidado$Valor <- as.numeric(
    evolucao_carteira_consolidado$Valor
  )
  
  
  return(
    chartjs(height = "300vh") %>%
      cjsTitle("Evolução Carteira") %>% 
      cjsLine(labels = evolucao_carteira_consolidado$Data) %>%
      cjsSeries(
        data = evolucao_carteira_consolidado$Valor
        , label = "Carteira")
  )
}