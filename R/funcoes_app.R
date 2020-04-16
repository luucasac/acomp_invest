###############################################################################
############################ CARTEIRA EASY INVEST #############################
###############################################################################

updateSymbols <- function(path) {
  
  symbols_to_update <- unique(
    readRDS(path)$cod_acao_yahoo
  )
  
  getSymbols(
    Symbols = unique(symbols_to_update), src = "yahoo"
    , auto.assign = TRUE)
  
  convertDf <- function(ts_data) {
    
    df <- as.data.frame(
      ts_data, stringsAsFactors = FALSE
    )
    
    return(df)
  }
  
  lista_acoes <- lapply(ls(pattern = "^[A-Z]"), function(x) eval(parse(text = x)))
  
  l <- lapply(lista_acoes, convertDf)
  
  return(l)
}

getRecentValue <- function(ativo) {
  
  if (ativo %in% c(
    "KNRI11.SA", "HGRE11.SA", "FLMA11.SA", "MXRF11.SA"
  )) {
    
    html <- read_html(
      paste0(
        "https://www.infomoney.com.br/cotacoes/fundos-imobiliarios-",
        tolower(str_remove(ativo, ".SA")), "/grafico/" 
      )
    )
    
    valor_html <- html_nodes(
      x = html
      ,xpath = "/html/body/div[4]/div/div[1]/div[1]/div/div[3]/div[1]/p"
    )
    
    vl_atual <- str_extract(html_text(valor_html), "\\d{1,3}[\\.|\\,]\\d{2}")
    
    vl_atual <- str_replace(vl_atual, ",", ".")
    
    variacao_html <- html_nodes(
      x = html
      ,xpath = "/html/body/div[4]/div/div[1]/div[1]/div/div[3]/div[2]"
    )
    
    variacao <- as.numeric(
      str_remove(
        str_extract(
          html_text(variacao_html)
          ,"[\\+|\\-]{0,1}\\d{1,3}[\\.|\\,]{0,1}\\d{0,2}%"
        ), "%"
      )
    ) / 100
    
    dt_atualizacao <- str_remove(Sys.time(), " -03")
    
    return(
      data.frame(
        "acao" = ativo
        ,"ult_atualizacao" = dt_atualizacao
        ,"ult_valor" = vl_atual
        ,"var_dia" = variacao
        ,stringsAsFactors = FALSE
      )
    )
  }
  
  if (ativo == "BOVA11") {

    html <- read_html(
      paste0("https://finance.yahoo.com/quote/%5EBVSP?p=", ativo,"&.tsrc=fin-srch")
    )
  } else {

    html <- read_html(
      paste0("https://finance.yahoo.com/quote/", ativo,"?p=", ativo,"&.tsrc=fin-srch")
    )
  }
  
  valor_html <- html_nodes(
    x = html
    ,xpath = "//*[@id=\"quote-header-info\"]"
  )
  
  vl_atual <- str_extract(html_text(valor_html), "\\d{1,2}[\\.|\\,]\\d{2}")
  
  variacao <- as.numeric(
    str_extract(
      str_extract(
        html_text(valor_html)
        ,"\\([\\+|\\-]{0,1}\\d{1,3}[\\.|\\,]{0,1}\\d{0,2}%\\)"
      ), "[\\d|\\-|\\+|\\.|\\,]+"
    )
  ) / 100
  
  vl_atual <- str_replace(vl_atual, ",", ".")
  
  dt_atualizacao <- str_remove(Sys.time(), " -03")
  
  return(
    data.frame(
      "acao" = ativo
      ,"ult_atualizacao" = dt_atualizacao
      ,"ult_valor" = vl_atual
      ,"var_dia" = variacao
      ,stringsAsFactors = FALSE
    )
  )
}


updateCarteira <- function(path) {
  
  carteira_easyinvest <- readRDS(
    paste0(path, "//carteira.rds")
    # "./bases/carteira_easyinvest_atual.rds"
  )
  
  # carteira_easyinvest <- readRDS(
  #   "./bases/EasyInvest/carteira.rds"
  #   # "./bases/carteira_easyinvest_atual.rds"
  # )
  
  if (!is.null(carteira_easyinvest$ult_atualizacao)) {
    
    
    
    series_historicas <- updateSymbols(path)
    
  } else {
    
    series_historicas <- readRDS(
      paste0(path, "//series_historicas.rds")
      # "./bases/series_historicas.rds"
    )    
    
  }
  
  carteira_easyinvest$cod_tit <- as.character(carteira_easyinvest$cod_tit)
  carteira_easyinvest$nome_tit <- as.character(carteira_easyinvest$nome_tit)
  carteira_easyinvest$cod_acao_yahoo <- as.character(carteira_easyinvest$cod_acao_yahoo)
  carteira_easyinvest$tp_invest <- as.character(carteira_easyinvest$tp_invest)
  
  l <- as.character(unique(carteira_easyinvest$cod_acao_yahoo))
  
  ultimos_valores <- bind_rows(lapply(l, getRecentValue))
  
  carteira_easyinvest$vl_atual <- left_join(
    select(carteira_easyinvest, -vl_atual),
    select(ultimos_valores, acao, ult_valor)
    , by = c("cod_acao_yahoo" = "acao")) %>% 
      select(ult_valor) %>% 
    unname() %>% 
    unlist()
  
  carteira_easyinvest$var_dia <- left_join(
    select(carteira_easyinvest, -var_dia), 
    select(ultimos_valores, acao, var_dia)
    , by = c("cod_acao_yahoo" = "acao")) %>% 
      select(var_dia) %>% 
    unname() %>% 
    unlist()
  
  carteira_easyinvest$vl_atual <- as.numeric(carteira_easyinvest$vl_atual)
  
  carteira_easyinvest$ult_atualizacao <- 
    left_join(
      select(carteira_easyinvest, cod_acao_yahoo), select(ultimos_valores, acao, ult_atualizacao)
      , by = c("cod_acao_yahoo" = "acao")
    ) %>% 
    select(ult_atualizacao) %>% 
    unname() %>% 
    unlist()
  
  carteira_easyinvest$rendimento <- 
    ifelse(
      is.na(carteira_easyinvest$vl_atual)
      ,NA
      ,round(
        ((carteira_easyinvest$vl_atual - carteira_easyinvest$vl_compra) / 
        (carteira_easyinvest$vl_compra)) * 100, 2)
    )
  
  carteira_easyinvest$vl_atual <- round(as.numeric(carteira_easyinvest$vl_atual), 2)
  
  saveRDS(carteira_easyinvest, 
    paste0(path, "//carteira.rds")
  )
  
  saveRDS(carteira_easyinvest, 
    paste0(path, "//series_historicas")
  )
  
}

tratamentoTimeSeries <- function(ts_data) {
      
  df <- as.data.frame(ts_data, stringsAsFactos = FALSE)
  
  df$data <- as.Date(rownames(ts_data))
  
  rownames(df) <- NULL
  
  acao_nome <- str_extract(names(ts_data)[1], "^[^\\.]+")
  
  {df <- df %>% 
    select(contains("Close", ignore.case = TRUE), data) %>% 
    mutate(
      Ativo = acao_nome
    ) %>% 
    `colnames<-`(c("Valor", "Data", "Ativo")) %>% 
    mutate(
      Valor = zoo::na.locf0(Valor)
    )  
  }
  
  return(df)
      
}

evolucaoCarteira <- function(carteira, l_data) {
  
  tratamentoTS <- function(ts_data) {
    
    df <- as.data.frame(ts_data, stringsAsFactos = FALSE)
    
    df$data <- as.Date(rownames(ts_data))
    
    rownames(df) <- NULL
    
    acao_nome <- str_extract(names(ts_data)[1], "^[^\\.]+")
    
    {df <- df[
      df$data >= min(
        as.Date(
          carteira$dt_compra[str_extract(carteira$cod_acao_yahoo, "^[^\\.]+") == acao_nome])
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
      Ativo = str_remove(cod_acao_yahoo, "\\.SA")
      ,Data = as.Date(substr(ult_atualizacao, 1, 10))
    ) %>% 
    select(Valor = vl_atual, Data, Ativo)) %>% 
    arrange(Ativo, Data)
    
  evolucao_carteira_detalhe <- evolucao %>% 
    left_join(
      select(carteira, cod_acao_yahoo, qtd_cotas, dt_compra) %>% 
        mutate(cod_acao_yahoo = str_remove(cod_acao_yahoo, "\\.SA"),
          dt_compra = as.Date(dt_compra))
      ,by = c("Ativo" = "cod_acao_yahoo", "Data" = "dt_compra")
    ) %>% 
    mutate(
      qtd_cotas = ifelse(is.na(qtd_cotas), 0, qtd_cotas)
    ) %>% 
    group_by(
      Data, Ativo, Valor
    ) %>% 
    summarise(
      qtd_cotas = sum(qtd_cotas)
    ) %>% 
    arrange(Ativo, Data) %>% 
      group_by(
      Data, Ativo
    )
  
  lista_Nova <- split(evolucao_carteira_detalhe, f = evolucao_carteira_detalhe$Ativo)
  
  evolucao_carteira_detalhe <- bind_rows(lapply(lista_Nova, function(df) {
    
    df$qtd_cotas <- cumsum(df$qtd_cotas)
      
    return(df)
  }))
  
  return(evolucao_carteira_detalhe)
  
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
  
  return(
    chartjs(height = "20vh") %>%
      cjsTitle("Evolução Carteira") %>% 
      cjsLine(labels = evolucao_carteira_consolidado$Data) %>%
      cjsSeries(
        data = evolucao_carteira_consolidado$Valor
        , label = "Carteira")
  )
}