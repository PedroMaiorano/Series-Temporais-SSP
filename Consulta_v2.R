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
  
  index_col <- switch(granularidade,
                      "ano" = year(data$data_bo_reg),
                      "sem" = yearhalf(data$data_bo_reg),
                      "tri" = yearquarter(data$data_bo_reg),
                      "mes" = yearmonth(data$data_bo_reg),
                      "semana" = yearweek(data$data_bo_reg)
  )
  
  data %>% 
    select(data_bo_reg) %>% 
    mutate(granularidade = index_col) %>% 
    group_by(granularidade) %>% 
    summarise(total = n()) %>% 
    as_tsibble(index = granularidade)
  
  
}


########################

get_transformacoes <- function(series_temporal, var = "total") {
  
  lambda <- series_temporal %>% 
    features(total, features = guerrero)  %>% 
    pull(lambda_guerrero)
  
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
  plot  = fit %>% select({{modelo}}) %>% gg_arma() + theme_pubclean() + ggtitle(glue('{modelo}')) + theme(plot.title = element_text(hjust = 0.5))
  return(plot)
  
}




plot_res <- function(fit,modelo){
  plot  = fit %>% select({{modelo}}) %>% gg_tsresiduals() + ggtitle(glue('{modelo}'))
  return(plot)
  
  
}


plot_predicao <- function(fit,modelo,h,serie){
  fit %>% select({{modelo}}) %>% forecast::forecast(h = {{h}}) %>% autoplot(serie)
}


plot_report <- function(fit,modelo){
  fit %>% select({{modelo}}) %>% report() 
}




######



get_pqd <- function(serie_temporal, ci  = 0.95){
  var = colnames(serie_temporal)[2]
  lag_max = serie_temporal %>% pull(!!sym(colnames(serie_temporal)[2])) %>% length()
  
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
    p = sequencia_partial$lengths[1]
    
  }else if(length(sequencia$lengths) >= 2 | length(sequencia_partial$lengths) >= 2){
    
    modelo = glue('ARMA({sequencia_partial$lengths[1]},{sequencia$lengths[1]})')
    q = sequencia$lengths[1]
    p = sequencia_partial$lengths[1]
  } else{ modelo = 'não foi possível selecionar um modelo'}
  
  return(glue('pdq({p},0,{q})'))
  
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


fit_serie <- function(serie,tipo_transformacao = 'identidade', dif ='1:5' , seasonal_dif = '1:5' ,p = '1:5',q = '1:5',P = '1:5',Q = '1:5',lag_seasonal_dif = "automatico"){
  serie_modificada <- get_transformacoes_modelo(serie,tipo_transformacao)
  # definir parâmetros automático
  
  if(lag_seasonal_dif == "automatico"){lag_seasonal_dif =  serie_modificada %>% pull(granularidade) %>% guess_frequency()}
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
  print(string_modelo_usuario)
  
  
  # modelo sugerido
  
  pqd_modelo_sugerido =  get_pqd(serie_modificada)
  string_modelo_sugerido = glue('0 + {pqd_modelo_sugerido} + PDQ(0,0,0)')
  formula_modelo_sugerido = as.formula(paste(string_transformacao,'~',string_modelo_sugerido))
  
  
  # SARIMA
  
  formula_SARIMA = as.formula(glue('{string_transformacao} ~ 0'))
  
  fit =  serie %>%
    model(usuario = ARIMA(formula_usuario,stepwise = FALSE),
          ingenuo = ARIMA(formula_modelo_sugerido),
          AUTO_sarima = ARIMA(formula_SARIMA,stepwise = FALSE))
  
  
  
  # para o relatorio
  
  
  return(fit)
  
}

