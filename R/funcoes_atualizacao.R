###############################################################################
############################ FUNCOES ATUALIZACAO ##############################
###############################################################################

atualizarTitulos <- function(nome_carteira) {
  
  symbols_to_update <- carregarRDS.carteira(
    nome_carteira = nome_carteira
    , tp = "operacoes"
  )$cod_tit_yahoo %>% 
    as.character()
  
  getSymbols(
    Symbols = unique(symbols_to_update), src = "yahoo"
    , auto.assign = TRUE)
  
  convertDf <- function(ts_data) {
    
    df <- as.data.frame(
      ts_data, stringsAsFactors = FALSE
    )
    
    return(df)
  }
  
  lista_acoes <- lapply(
    ls(pattern = "^[A-Z]")
    , function(x) eval(parse(text = x))
  )
  
  l <- lapply(lista_acoes, convertDf)
  
  return(l)
}

pegarValorRecente <- function(ativo, nome_carteira) {
  
  carteira <- carregarRDS.carteira(
    nome_carteira = nome_carteira
    ,"operacoes"
  ) %>% 
    filter(cod_tit == ativo) %>% 
    head(1)
  
  ativo_yahoo <- paste0(ativo, ".SA")
  
  if (carteira$tp_invest == "Fundo Imobiliário") {
    
    html <- read_html(
      paste0(
        "https://finance.yahoo.com/quote/"
        ,ativo_yahoo,"?p="
        ,ativo_yahoo
        ,"&.tsrc=fin-srch"
      )
    )
    
  } else {
    
    html <- read_html(
      paste0("https://finance.yahoo.com/quote/", ativo_yahoo,"?p=", ativo,"&.tsrc=fin-srch")
    )
  }

  valor_html <- html_nodes(
    x = html
    ,xpath = "//*[@id=\"quote-header-info\"]/div[3]"
  )
  
  vl_atual <- as.numeric(
    str_extract(
      string = html_text(valor_html), 
      pattern =  "\\d{1,3}[\\.|\\,]\\d{2}"
    )
  )
  
  variacao <- as.numeric(
    str_extract(
      str_extract(
        html_text(valor_html)
        ,"\\([\\+|\\-]{0,1}\\d{1,3}[\\.|\\,]{0,1}\\d{0,2}%\\)"
      ), "[\\d|\\-|\\+|\\.|\\,]+"
    )
  )
  
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

consolidaOperacoes <- function(operacoes) {
  
  operacoes$vl_compra <- as.numeric(operacoes$vl_compra)
  operacoes$qtd_cotas <- as.integer(operacoes$qtd_cotas)
  
  carteira_consolidada <- operacoes %>%
    group_by(cod_tit, nome_tit, tp_invest) %>% 
    summarise(
      vl_compra = round(
        x = sum(
          vl_compra * qtd_cotas
        ) / sum(
          qtd_cotas)
        ,digits =  2
      )
      ,vl_atual = round(
        x = sum(
          vl_atual * qtd_cotas
        ) / sum(
          qtd_cotas)
        ,digits =  2
      )
      ,var_dia = round(
        x = mean(
          var_dia), 4)
      ,qtd_cotas = sum(qtd_cotas)
      ,rendimento = round(
        x = ((
          vl_atual * qtd_cotas) / 
            (vl_compra * qtd_cotas)) - 1
        ,digits =  4
      )
      ,ganhos = 
        (vl_atual - vl_compra) * qtd_cotas
    )
  
  carteira_consolidada <- bind_rows(
    carteira_consolidada,
    data.frame(
      "cod_tit" = ""
      ,"nome_tit" = "<b>Carteira</b>"
      ,"tp_invest" = ""
      ,"vl_compra" = round(
        as.numeric(
          sum(
            (operacoes$vl_compra * operacoes$qtd_cotas), na.rm = TRUE) / 
            sum(operacoes$qtd_cotas)
          )
        )
      ,"vl_atual" = round(
        as.numeric(
          sum(
            (operacoes$vl_atual * operacoes$qtd_cotas), na.rm = TRUE
          ) / sum(
            operacoes$qtd_cotas, na.rm = TRUE
            )), 2
      )
      ,"var_dia" = round(
          x = sum(
          (operacoes$var_dia/100) * operacoes$qtd_cotas, na.rm = TRUE) /
            sum(operacoes$qtd_cotas, na.rm = TRUE)
          ,digits = 4
      ) * 100
      ,"qtd_cotas" = as.integer(
        sum(
          operacoes$qtd_cotas, na.rm = TRUE
        )
      )
      ,"rendimento" = as.numeric(
        round(x = ((
            sum(
              (operacoes$vl_atual * operacoes$qtd_cotas), na.rm = TRUE
            ) /
            sum(
              operacoes$qtd_cotas, na.rm = TRUE)
            ) / 
            (sum(
              (operacoes$vl_compra * operacoes$qtd_cotas), na.rm = TRUE
            ) / 
                sum(
                  operacoes$qtd_cotas, na.rm = TRUE
                  )
            ) - 1)
          ,digits =  2
        )
      )
      ,"ganhos" = round(
        x = sum((operacoes$vl_atual - 
            operacoes$vl_compra) * operacoes$qtd_cotas
          , na.rm = TRUE)
        ,digits = 2
      )
      ,stringsAsFactors = FALSE
    )
  )
  
  carteira_consolidada$var_dia <- paste0(
    ifelse(
      is.na(carteira_consolidada$var_dia),
      "",
      ifelse(
      carteira_consolidada$var_dia <= 0
        ,paste0(
          "<b><span style='color: #ff0000'>", 
          round(
            x = carteira_consolidada$var_dia
            ,digits =  2
          ),
          "%",
          "</span></b>"
        )
        ,ifelse(
          carteira_consolidada$var_dia <= 0.03,
          paste0(
            "<b><span style='color: #0000ff'>", 
            round(
              x = carteira_consolidada$var_dia
              ,digits =  2
            ), 
            "%",
            "</span></b>"
          ),
          paste0(
            "<b><span style='color: #00cc00'>", 
            round(
              x = carteira_consolidada$var_dia
              , 2
            ),
            "%",
            "</span></b>"
          )
        )
      )
    )
  )
  
  carteira_consolidada$rendimento <- paste0(
    ifelse(
      is.na(carteira_consolidada$rendimento),
      "",
      ifelse(
      carteira_consolidada$rendimento <= 0
        ,paste0(
          "<b><span style='color: #ff0000'>", 
          round(
            x = carteira_consolidada$rendimento * 100
            ,digits =  4
          ),
          "%",
          "</span></b>"
        )
        ,ifelse(
          carteira_consolidada$rendimento <= 0.03,
          paste0(
            "<b><span style='color: #0000ff'>", 
            round(
              x = carteira_consolidada$rendimento * 100
              ,digits =  4
            ), 
            "%",
            "</span></b>"
          ),
          paste0(
            "<b><span style='color: #00cc00'>", 
            round(
              x = carteira_consolidada$rendimento * 100
              ,digits =  4
            ),
            "%",
            "</span></b>"
          )
        )
      )
    )
  )
  
  carteira_consolidada$ganhos <- ifelse(
    carteira_consolidada$ganhos < 0
    ,paste0(
      "<b><span style='color: #ff0000'>R$ "
      ,format(
        round(
          carteira_consolidada$ganhos
          ,digits =  2
        )
        ,format = "f"
        ,big.mark = "."
        ,decimal.mark = ","
      )
      ,"</span></b>"
    )
    ,ifelse(
      carteira_consolidada$ganhos < 50
      ,paste0(
        "<b><span style='color: #0000ff'>R$ "
        ,format(
          round(
            carteira_consolidada$ganhos
            ,digits =  2
          )
          ,format = "f"
          ,big.mark = "."
          ,decimal.mark = ","
        )
        ,"</span></b>"
      )
      ,paste0(
        "<b><span style='color: #00cc00'>R$ "
        ,format(
          round(
            carteira_consolidada$ganhos
            ,digits =  2
          )
          ,format = "f"
          ,big.mark = "."
          ,decimal.mark = ","
        )
        ,"</span></b>"
      )
    )
  )
  
  carteira_consolidada$vl_atual <- paste0("R$ "
  ,formatC(
    x = carteira_consolidada$vl_atual
    ,digits = 2
    ,format = "f"
    ,big.mark = "."
    ,decimal.mark = ","
    )
  )
  
  carteira_consolidada$vl_compra <- paste0("R$ "
  ,formatC(
    x = carteira_consolidada$vl_compra
    ,digits = 2
    ,format = "f"
    ,big.mark = "."
    ,decimal.mark = ","
    )
  )
  
  return(carteira_consolidada)
}

atualizarCarteira <- function(nome_carteira, obj.reac) {
  
  series_historicas <- carregarRDS.carteira(
    nome_carteira = nome_carteira
    ,tp = "series_historicas"
  )
  
  operacoes <- carregarRDS.carteira(
    nome_carteira = nome_carteira
    ,tp = "operacoes"
  )
  if (
    length(series_historicas) == 0
  ) {
    
    series_historicas <- atualizarTitulos(nome_carteira)
    
  } else if (max(as.Date(rownames(series_historicas[[1]]))) != Sys.Date() - 1) {
    
    series_historicas <- atualizarTitulos(nome_carteira)
    
    } else {
    
    series_historicas <- carregarRDS.carteira(
      nome_carteira = nome_carteira
      ,tp = "series_historicas"
    )    
    
  }
  
  l <- as.character(
    unique(operacoes$cod_tit)
  )
  
  ultimos_valores <- bind_rows(
    lapply(
      X = l
      ,FUN = pegarValorRecente
      ,nome_carteira
    )
  )
  
  operacoes$cod_tit <- as.character(operacoes$cod_tit)
  operacoes$nome_tit <- as.character(operacoes$nome_tit)
  operacoes$cod_tit_yahoo <- as.character(operacoes$cod_tit_yahoo)
  operacoes$dt_compra <- as.character(operacoes$dt_compra)
  operacoes$tp_invest <- as.character(operacoes$tp_invest)
  
  if (is.null(operacoes$vl_atual)) {
    
    operacoes$vl_atual <- left_join(
      operacoes,
      select(ultimos_valores, acao, ult_valor)
      , by = c("cod_tit" = "acao")) %>% 
        select(ult_valor) %>% 
      unname() %>% 
      unlist()
    
  } else {
    
    operacoes$vl_atual <- left_join(
      select(operacoes, -vl_atual),
      select(ultimos_valores, acao, ult_valor)
      , by = c("cod_tit" = "acao")) %>% 
        select(ult_valor) %>% 
      unname() %>% 
      unlist()
    
  }
  
  if (is.null(operacoes$var_dia)) {
    
    operacoes$var_dia <- left_join(
      operacoes,
      select(ultimos_valores, acao, var_dia)
      , by = c("cod_tit" = "acao")) %>% 
        select(var_dia) %>% 
      unname() %>% 
      unlist()
    
  } else {

    operacoes$var_dia <- left_join(
      select(operacoes, -var_dia),
      select(ultimos_valores, acao, var_dia)
      , by = c("cod_tit" = "acao")) %>% 
        select(var_dia) %>% 
      unname() %>% 
      unlist()
  }
  
  operacoes$vl_atual <- round(as.numeric(operacoes$vl_atual), 2)
  operacoes$vl_compra <- round(as.numeric(operacoes$vl_compra), 2)
  operacoes$var_dia <- round(as.numeric(operacoes$var_dia), 4)
  
  operacoes$ult_atualizacao <- 
    left_join(
      select(operacoes, cod_tit), select(ultimos_valores, acao, ult_atualizacao)
      , by = c("cod_tit" = "acao")
    ) %>% 
    select(ult_atualizacao) %>% 
    unname() %>% 
    unlist()
  
  operacoes$rendimento <- ifelse(
      is.na(operacoes$vl_atual)
      ,NA
      ,round(
        ((operacoes$vl_atual - operacoes$vl_compra) / 
        (operacoes$vl_compra)) * 100, 2)
    )
  
  carteira <- consolidaOperacoes(operacoes)
  
  lista <- list(
    "operacoes" = operacoes
    ,"series_historicas" = series_historicas
    ,"carteira" = carteira
  )  
  
  return(lista)
}
