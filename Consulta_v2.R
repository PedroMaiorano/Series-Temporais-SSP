aplicarEstilosAoGrafico <- function(graficoId) {
  runjs(sprintf('
    var graficoDiv = document.getElementById("%s");
    graficoDiv.style.borderRadius = "50px";  // ou qualquer valor desejado
    graficoDiv.style.boxShadow = "8px 8px 16px #888888";
  ', graficoId))
}

get_acf <- function(dados, lag_max, coluna, tipo = "correlation"){
  
  dados_filtrados = dados %>% select({{coluna}})
  
  return(dados_filtrados %>% ACF(lag_max = lag_max,type = tipo))
}


dados_filtrados_tempo <- function(dados, granularidade = "ano") {
  data <- dados %>% 
    filter(data_bo_reg != -1) %>% 
    mutate(data_bo_reg = ymd(data_bo_reg)) %>%  
    filter(year(data_bo_reg) >= 2003) %>% 
    filter(year(data_bo_reg) <= 2022)
  
  if(granularidade == "sem"){
    semestre_data = data %>%  select(data_bo_reg) %>% map( ~ semester(.x, with_year  = TRUE)) %>% unlist() %>% yearmon() %>% yearmonth()
    serie_semestre <- data %>% mutate(granularidade = semestre_data) %>%  group_by(granularidade)%>% summarise( total = n())
    
    invervalo_semestre = new_interval(month = 6)
    
    
    serie <-
      serie_semestre %>%
      build_tsibble(
        index = granularidade,
        interval = invervalo_semestre
      ) 
    
    
    
  }else{
    index_col <- switch(granularidade,
                        "ano" = year(data$data_bo_reg),
                        "tri" = yearquarter(data$data_bo_reg),
                        "mes" = yearmonth(data$data_bo_reg),
                        "semana" = yearweek(data$data_bo_reg)
    )
    
    serie <- data %>% 
      select(data_bo_reg) %>% 
      mutate(granularidade = index_col) %>% 
      group_by(granularidade) %>% 
      summarise(total = n()) %>% 
      as_tsibble(index = granularidade)
  }
  return(serie)
}




########################

get_transformacoes <- function(series_temporal, var = "total") {
  if(var == "box_cox"){
  lambda <- series_temporal %>% 
    features(total, features = guerrero)  %>% 
    pull(lambda_guerrero)}
  
  series_temporal <- series_temporal %>%
    mutate(log = if (var == "log") log(total) else NULL,
           sqrt = if (var == "sqrt") sqrt(total) else NULL,
           inversa = if (var == "inversa") 1/total else NULL,
           box_cox = if (var == "box_cox") box_cox(total, lambda) else NULL)
  if (var != "total"){
    series_temporal <- series_temporal[,-2]}
  
  return(series_temporal)
}





diff <- function(serie,lag_ , dif){
  serie_modificada = serie %>% mutate(transformacao = difference(!!sym(colnames(serie)[2]), lag = lag_ , differences = dif))
  if(colnames(serie_modificada)[2] == "transformacao"){
    return(serie_modificada)}
  else{
    return(serie_modificada %>% select(-2))}
  }




#####
teste_adf <- function(serie_temporal,var){
  adf_test  =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% adf.test()
  print(adf_test)
  
}

teste_kpss <- function(serie_temporal,var){
  serie_temporal %>% features({{var}}, unitroot_kpss) }

teste_kpss_numero_diff <- function(serie_temporal,var){
  serie_temporal %>% features({{var}}, unitroot_ndiffs ) }


teste_kpss_season_diff <- function(serie_temporal,var){
  serie_temporal %>% features({{var}}, unitroot_nsdiffs ) }



teste_cox_stuart <- function(serie_temporal,var){
  cox_stuart =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% cs.test()
  print(cox_stuart)
  
}

teste_kendall<- function(serie_temporal,var){
  kendall =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% mk.test()
  print(kendall)
  
}


teste_WAVK <- function(serie_temporal,var){
  serie =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) 
  
  WAVK = notrend_test(serie, test = "WAVK", factor.length = "adaptive.selection")
  
  print(WAVK)}

teste_seasonal <- function(serie_temporal,var  ,teste = "combined"){
  frequencia  = serie_temporal %>% pull(granularidade) %>% guess_frequency()
  sesonal =  serie_temporal %>%   select({{var}}) %>% filter(!is.na({{var}})) %>% pull({{var}}) %>% isSeasonal(test = teste,freq = frequencia)
  print(sesonal)}


####
#teste <- dados_filtrados_tempo(homicidioDoloso_parcial, "mes")
#teste
#teste2 <- teste_kpss(teste, total)
#teste2
#
#teste3 <- teste_kpss_numero_diff(teste, total)
#teste3
#
#
#teste4 <- teste_kpss_season_diff(teste, total)
#teste4
#
#teste5 <- teste_cox_stuart(teste,total)
#teste5
#
#teste6 <- teste_kendall(teste,total)
#
#teste7<- teste_WAVK(teste,total)
#teste7
#
#teste8<- teste_seasonal(teste,total)




get_acf_sig <- function(serie_temporal, ci  = 0.95,var){
  lag_max = serie_temporal %>% pull(total) %>% length()
  
  ci = qnorm((1+0.95)/2)/sqrt(lag_max) # calcula o intervalo de confiança
  
  # autocorrelacao
  not_sig = !get_acf(serie_temporal,lag_max,var) %>% pull(acf) %>% between(-ci,ci) # calcula quem passou do intervalo de confiança
  sequencia = rle(not_sig) # faz o Run-length encoding (RLE) do vetor not_sig calcula o tamanho das sequencias de true e false
  
  #candidatos_MA = cumsum(sequencia$lengths)  %>% tibble() %>% slice( which(sequencia$values == TRUE) ) %>% pull() # candidatos 
  
  # autocorrelacao parcial
  
  not_sig_partial =  !get_acf(serie_temporal,lag_max,var,"partial") %>% pull(acf) %>% between(-ci,ci)
  sequencia_partial = rle(not_sig_partial) 
  
  #candidatos_AR = cumsum(sequencia_partial$lengths)  %>% tibble() %>% slice( which(sequencia_partial$values == TRUE) ) %>% pull()
  
  
  # suguestão para o user
  if(length(sequencia$lengths) == 2){
    modelo = glue('MA{sequencia$lengths[1]}')
    p = 0
    q = sequencia$lengths[1]
    
  } else if(length(sequencia_partial$lengths) == 2){
    modelo = glue('AR{sequencia_partial$lengths[1]}')
    q = 0
    p = sequencia$lengths[1]
    
  }else if(length(sequencia$lengths) >= 2 | length(sequencia_partial$lengths) >= 2){
    
    modelo = glue('ARMA({sequencia_partial$lengths[1]},{sequencia$lengths[1]})')
    q = sequencia$lengths[1]
    p = sequencia_partial$lengths[1]
  } else{ modelo = 'não foi possível selecionar um modelo'}
  
  # ajuste o modelo
  
  formula = as.formula(glue('{var} ~  0 + pdq({p},0,{q}) + PDQ(0,0,0)'))
  formula_arima = as.formula(glue('{var} ~  0 + PDQ(0,0,0)'))
  formula_PQD= as.formula(glue('{var} ~  0'))
  
  
  fit <- serie_temporal %>% 
    model(modelo_sugerido = ARIMA(formula),
          modelo_arima = ARIMA(formula_arima, stepwise = FALSE),
          modelo_arima_default = ARIMA(formula_PQD, stepwise = FALSE))
  
  
  
  return(fit)
  
  
}


plot_raiz <- function(fit,modelo){
  plot  = fit %>% select({{modelo}}) %>% pull() %>% gg_arma() + theme_pubclean() + ggtitle(glue('{modelo}')) + theme(plot.title = element_text(hjust = 0.5))
  return(plot)
  
}



plot_res <- function(fit,modelo){
  plot  = fit %>% select({{modelo}}) %>% pull() %>% gg_tsresiduals() + ggtitle(glue('{modelo}'))
  return(plot)
  
  
}



plot_predicao <- function(fit,modelo,h,serie){
  fit %>% select({{modelo}}) %>%  pull() %>% forecast::forecast(h = {{h}}) %>% autoplot(serie) +  theme_pubclean()
}



plot_report <- function(fit,modelo){
  fit %>% select({{modelo}}) %>% pull() %>% report() 
}


teste_ljung_box <- function(objeto_fit,modelo){
  modelo_selecionado <- objeto_fit %>% select({{modelo}}) %>% pull() %>% pull()
  p = modelo_selecionado[[1]]$fit$spec$p
  q = modelo_selecionado[[1]]$fit$spec$q
  P = modelo_selecionado[[1]]$fit$spec$P
  Q = modelo_selecionado[[1]]$fit$spec$Q
  freq = modelo_selecionado[[1]]$fit$spec$period
  df = p +q + P + Q
  objeto_fit %>% select({{modelo}}) %>% pull() %>% augment() %>% features(.innov, ljung_box, lag = 2*freq, dof = df)
  
  
  
}



######



get_pqd <- function(serie_temporal,d,ci  = 0.95){
  # intervalo de conf
  
  lag_max = serie_temporal %>% pull(transformacao) %>% length()
  if (d != 0){
    serie_temporal <- serie_temporal %>% mutate(transformacao = difference(transformacao, lag = 1 , differences = d))
  }
  
  ci = qnorm((1+0.95)/2)/sqrt(lag_max) 
  
  
  # autocorrelacao
  
  acf = get_acf(serie_temporal,lag_max,"transformacao") %>% pull(acf)
  not_sig = !acf %>%  between(-ci,ci) # calcula quem passou do intervalo de confiança
  sequencia = rle(not_sig) 
  q = sequencia$lengths[1]
  acf_truncado_acf <- acf[q:length(acf)]
  sinais_acf =  (acf_truncado_acf - lag(acf_truncado_acf))  %>% na.remove()
  sinais_obs_acf <- sum(sinais_acf > 0)
  p_valor_acf <- binom.test(sinais_obs_acf,length(sinais_acf))$p.value
  
  
  
  
  # auto autocorrelacao parcial
  pacf = get_acf(serie_temporal,lag_max,"transformacao", "partial") %>% pull(acf)
  not_sig_partial =  !pacf %>% between(-ci,ci)
  sequencia_partial = rle(not_sig_partial) 
  p = sequencia_partial$lengths[1]
  pacf_truncado_acf <- pacf[p:length(pacf)]
  sinais_pacf =  (pacf_truncado_acf - lag(pacf_truncado_acf))  %>% na.remove()
  sinais_obs_pacf <- sum(sinais_pacf > 0)
  p_valor_pacf <- binom.test(sinais_obs_pacf,length(sinais_pacf))$p.value
  
  arma = (p > 0) & (q > 0)
  if(arma){
    truncar = abs(p - q)
    pacf_truncado_arima <- pacf[truncar:length(pacf)]
    
    sinais_pacf_arima =  (pacf_truncado_arima - lag(pacf_truncado_arima))  %>% na.remove()
    sinais_obs_pacf_arima <- sum(sinais_pacf_arima > 0)
    p_valor_pacf_arima <- binom.test(sinais_obs_pacf_arima,length(sinais_pacf_arima))$p.value
    
    
    acf_truncado_arima <- acf[truncar:length(acf)]
    sinais_acf_arima =  (acf_truncado_arima - lag(acf_truncado_arima))  %>% na.remove()
    sinais_obs_acf_arima <- sum(sinais_acf_arima > 0)
    p_valor_acf_arima <- binom.test(sinais_obs_acf_arima,length(sinais_acf_arima))$p.value
  }
  
  
  
  
  
  
  # medias moveis
  if ((which(sequencia$values)[1] == 1) & (p_valor_pacf > 0.05) & !arma) {
    p = 0
    modelo = glue('pdq(0,{d},{q})')
  } else if ((which(sequencia_partial$values)[1] == 1) & (p_valor_acf > 0.05) & !arma) {
    q = 0
    modelo = glue('pdq({p},{d},0)')
  } else if (arma & (p_valor_pacf_arima > 0.05)) {
    modelo = glue('pdq({p},{d},{q})')
  } else {
    modelo = 'Não é possível encontrar um modelo'
  }
  
  ruido_branco = (p == 0) & (q==0)
  return(list('modelo' = modelo,
              'ruido_branco' = ruido_branco))
  
}









get_transformacoes_modelo <- function(serie,tipo_transformacao = "identidade"){
  if(tipo_transformacao == "box_cox"){lambda = serie %>% features(!!sym(colnames(serie)[2]), features = guerrero) %>% pull(lambda_guerrero)}
  serie_modificada = switch(tipo_transformacao,
                            
                            identidade = {
                              serie %>%
                                mutate(transformacao = !!sym(colnames(serie)[2]))
                            },
                            
                            
                            log = {
                              serie %>%
                                mutate(transformacao = log(!!sym(colnames(serie)[2])))
                            },
                            sqrt = {
                              serie %>%
                                mutate(transformacao = sqrt(!!sym(colnames(serie)[2])))
                            },
                            inversa = {
                              serie %>%
                                mutate(transformacao = 1/!!sym(colnames(serie)[2]))
                            },
                            box_cox = {
                              serie %>%
                                mutate(transformacao = box_cox(!!sym(colnames(serie)[2]), lambda))
                            },
  )
  return(serie_modificada)      
}




#<ARIMA(0,1,1)(1,0,1)[12]>
#PDQ


fit_serie <- function(serie,tipo_transformacao = 'identidade', d,dif ='1:2' , seasonal_dif = '0:1' ,p = '0:5',q = '0:5',P = '0:2',Q = '0:2',lag_seasonal_dif = "automatico",modelos){
  serie_modificada = get_transformacoes_modelo(serie,tipo_transformacao)
  
  
  pqd_modelo_sugerido =  get_pqd(serie_modificada,d=d)['modelo']
  if(pqd_modelo_sugerido['ruido_branco'] == "Verifique se a série é ruído branco")
  {return(123)}
  
  else{
    
    
    
    if(lag_seasonal_dif == "automatico"){lag_seasonal_dif =  serie_modificada %>% pull(granularidade) %>% guess_frequency() %>% floor() }
    
    
    
    # Ajuste dos modelos
    
    string_transformacao = switch(tipo_transformacao,
                                  identidade = 'total',
                                  box_cox = glue('box_cox(total, {lambda})'),
                                  sqrt = 'sqrt(total)',
                                  inversa = '1/total',
                                  log = 'log(total)')
    
    #usuario
    
    PQD = ifelse(seasonal_dif == 0,'PDQ(0,0,0)', glue('PDQ({P},{seasonal_dif},{Q}, period = {lag_seasonal_dif})'))
    string_modelo_usuario= glue('0 + pdq({p},{dif},{q}) + {PQD}')
    formula_usuario = as.formula(paste(string_transformacao,'~',string_modelo_usuario))
    
    
    # modelo sugerido
    
    string_modelo_sugerido = glue('0 + {pqd_modelo_sugerido} + PDQ(0,0,0)')
    formula_modelo_sugerido = as.formula(paste(string_transformacao,'~',string_modelo_sugerido))
    
    
    # SARIMA
    
    formula_SARIMA = as.formula(glue('{string_transformacao} ~ 0'))
    
    
    # ARMA
    string_modelo_ARMA = glue('0 + pdq(1:5,0,1:5) + PDQ(0,0,0)')
    formula_modelo_ARMA= as.formula(paste(string_transformacao,'~',string_modelo_ARMA))
    
    # ARIMA
    
    string_modelo_ARIMA = glue('0 + pdq(1:5,1:2,1:5) + PDQ(0,0,0)')
    formula_modelo_ARIMA= as.formula(paste(string_transformacao,'~',string_modelo_ARIMA))
    
    
    fit_ARMA<- if ('arma' %in% modelos) {
      serie %>%
        model(arma = ARIMA(formula_modelo_ARMA, stepwise = FALSE))
    } else {
      NA
    }
    
    fit_ARIMA<- if ('arima' %in% modelos) {
      
      serie %>%
        model(arima = ARIMA(formula_modelo_ARIMA, stepwise = FALSE))
    } else {
      NA
    }
    
    
    
    fit_usuario <- if ('usuario' %in% modelos) {
      serie %>%
        model(usuario = ARIMA(formula_usuario, stepwise = FALSE))
    } else {
      NA
    }
    
    fit_ingenuo <- if ('modelo_pia' %in% modelos & pqd_modelo_sugerido != "Não é possível encontrar um modelo") {
      serie %>%
        model(modelo_pia = ARIMA(formula_modelo_sugerido, stepwise = FALSE))
    } else if ('modelo_pia' %in% modelos & pqd_modelo_sugerido == "Não é possível encontrar um modelo") {
      'Não foi possível encontrar um modelo'
    } else if (!('modelo_pia' %in% modelos)) {
      NA
    }
    
    fit_AUTO_sarima <- if ('auto_sarima' %in% modelos) {
      serie %>%
        model(auto_sarima = ARIMA(formula_SARIMA, stepwise = FALSE))
    } else {
      NA
    }
    
    
    
    
    fit = tibble(
      usuario = fit_usuario,
      modelo_pia = fit_ingenuo,
      arma = fit_ARMA,
      auto_sarima = fit_AUTO_sarima,
      arima = fit_ARIMA,
      
      .name_repair = c('minimal')
    )
    
    # para o relatório
    fit <- fit %>% select(!where(is.na))
    
    return(fit)
    
    
    # para o relatorio
    
  }
  
  
}


