    Side_hom <- 
      fluidPage(
        #h3("Filtro dos dados"),
       # br(),
        dateRangeInput('dateRange2',
                       label = "Filtrar por data",
                       start = "2017-01-01", end = "2022-12-31",
                       min = "2017-01-01", max = "2022-12-31",
                       separator = " - ", format = "dd/mm/yy",
                       startview = 'year', language = 'pt-BR', weekstart = 1
        ),
        pickerInput(
          inputId = "cor_pele",
          label = "Cor da pele", 
          choices = c("amarela","branca","ignorada","não informado", "outros", "parda", "preta", "vermelha"),
          selected = c("amarela","branca","ignorada","não informado", "outros", "parda", "preta", "vermelha"),
          options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
        ),
        pickerInput(
          inputId = "sexo",
          label = "Sexo", 
          choices = c("feminino","indefinido","masculino"),
          selected = c("feminino","indefinido","masculino"),
          options = list(
            `actions-box` = TRUE), 
          multiple = TRUE
        ),
        sliderInput("idade_slider", "Filtrar por Idade:",
                    min = -1, max = 97, value = c(-1, 97)),
        actionButton("aplicar","Modificar os dados"),
        actionButton("reset","Não usar Filtro"))
    
                             
    
    
    Side_fem <- 
                 fluidPage(
                  
                   
                   dateRangeInput('dateRange2',
                                  label = "Filtrar por data",
                                  start = "2015-04-12", end = "2022-12-31",
                                  min = "2015-04-12", max = "2022-12-31",
                                  separator = " - ", format = "dd/mm/yy",
                                  startview = 'year', language = 'pt-BR', weekstart = 1),
                   pickerInput(
                     inputId = "cor_pele",
                     label = "Cor da pele", 
                     choices = c("amarela","branca","ignorada","outros","parda","preta"),
                     selected = c("amarela","branca","ignorada","outros","parda","preta"),
                     options = list(
                       `actions-box` = TRUE), 
                     multiple = TRUE),
                   sliderInput("idade_slider", "Filtrar por Idade:", min = -1, max = 87, value = c(-1,87)),
                   actionButton("aplicar","Modificar os dados"),
                   actionButton("reset","Não usar Filtro"))  
    
    
    
    Side_lat <-  fluidPage( 
                     
                      
                      dateRangeInput('dateRange2',
                                              label = "Filtrar por data",
                                              start = "2018-01-03", end = "2022-12-31",
                                              min = "2018-01-03", max = "2022-12-31",
                                              separator = " - ", format = "dd/mm/yy",
                                              startview = 'year', language = 'pt-BR', weekstart = 1),
                     pickerInput(inputId = "cor_pele",
                                 label = "Cor da pele", 
                                 choices = c("amarela","branca","não informado","parda","preta","vermelha"),
                                 selected = c("amarela","branca","não informado","parda","preta","vermelha"),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE),
                    pickerInput(inputId = "sexo",
                                 label = "Sexo", 
                                 choices = c("feminino","indefinido","masculino"),
                                 selected = c("feminino","indefinido","masculino"),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE),
                    sliderInput("idade_slider", "Filtrar por Idade:",
                                           min = -1, max = 118, value = c(-1, 118)),
                    actionButton("aplicar","Modificar os dados"),
                    actionButton("reset","Não usar Filtro"))  
    
    Side_lcs <- fluidPage(
                        
                         
                         dateRangeInput('dateRange2',
                                              label = "Filtrar por data",
                                              start = "2016-09-14", end = "2022-12-31",
                                              min = "2016-09-14", max = "2022-12-31",
                                              separator = " - ", format = "dd/mm/yy",
                                              startview = 'year', language = 'pt-BR', weekstart = 1),
                         pickerInput(
                                 inputId = "cor_pele",
                                 label = "Cor da pele", 
                                 choices = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
                                 selected = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE),
                         pickerInput(
                                 inputId = "sexo",
                                 label = "Sexo", 
                                 choices = c("feminino","indefinido","masculino"),
                                 selected = c("feminino","indefinido","masculino"),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE
                               ),
                               sliderInput("idade_slider", "Filtrar por Idade:",
                                           min = -1, max = 88, value = c(-1, 88)),
                         actionButton("aplicar","Modificar os dados"),
                         actionButton("reset","Não usar Filtro"))  
                               
                              
    
    Side_mdi <- fluidPage(
                       
                        
                        dateRangeInput('dateRange2',
                                              label = "Filtrar por data",
                                              start = "2013-01-01", end = "2022-12-31",
                                              min = "2013-01-01", max = "2022-12-31",
                                              separator = " - ", format = "dd/mm/yy",
                                              startview = 'year', language = 'pt-BR', weekstart = 1),
                        pickerInput(
                                 inputId = "cor_pele",
                                 label = "Cor da pele", 
                                 choices = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
                                 selected = c("amarela","branca","ignorada","não informado","outros","parda","preta"),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE),
                        pickerInput(
                                 inputId = "sexo",
                                 label = "Sexo", 
                                 choices = c("feminino","indefinido","masculino","não informado"),
                                 selected = c("feminino","indefinido","masculino","não informado"),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE),
                               sliderInput("idade_slider", "Filtrar por Idade:",
                                           min = -1, max = 77, value = c(-1, 77)),
                               
                        fluidPage(
                                 column(7,pickerInput(
                                   inputId = "corp",
                                   label = "Situação do policial", 
                                   choices = c("folga","serviço"),
                                   selected = c("folga","serviço"),
                                   options = list(
                                     `actions-box` = TRUE), 
                                   multiple = TRUE
                                 ))),
                                 
                                 actionButton("aplicar","Modificar os dados"),
                                 actionButton("reset","Não usar Filtro"))  
    
    Side_mor <- fluidPage(
                        
                         
                         dateRangeInput('dateRange2',
                                              label = "Filtrar por data",
                                              start = "2013-01-01", end = "2022-12-31",
                                              min = "2013-01-01", max = "2022-12-31",
                                              separator = " - ", format = "dd/mm/yy",
                                              startview = 'year', language = 'pt-BR', weekstart = 1),
                         pickerInput(
                                 inputId = "descr_local",
                                 label = "Descrição do local de ocorrência", 
                                  choices = c("area não ocupada","centro comerc./empresarial","comércio e serviços","condominio comercial","condominio residencial","entidade assistencial","escritório","estabelecimento bancário","estabelecimento de ensino","estabelecimento industrial","estabelecimento prisional","estacionamento particular","estrada de ferro","favela","garagem coletiva de prédio","garagem ou abrigo de residência","hospedagem","internet","lazer e recreação","local clandestino/ilegal","outros","repartição pública","residência","restaurante e afins","rodovia/estrada","saúde","serviços e bens públicos","shopping center","sindicato","templo e afins","terminal/estação","unidade rural","veículo em movimento","via pública"),
                                 selected = c("area não ocupada","centro comerc./empresarial","comércio e serviços","condominio comercial","condominio residencial","entidade assistencial","escritório","estabelecimento bancário","estabelecimento de ensino","estabelecimento industrial","estabelecimento prisional","estacionamento particular","estrada de ferro","favela","garagem coletiva de prédio","garagem ou abrigo de residência","hospedagem","internet","lazer e recreação","local clandestino/ilegal","outros","repartição pública","residência","restaurante e afins","rodovia/estrada","saúde","serviços e bens públicos","shopping center","sindicato","templo e afins","terminal/estação","unidade rural","veículo em movimento","via pública"),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE),
                         pickerInput(
                                 inputId = "desdobramento",
                                 label = "Desdobramento da ocorrência", 
                                 choices = c(
                                   "i - encontro de cadáver sem lesões aparentes",
                                   "ii - dúvidas razoáveis quanto a suicídio ou morte provocada",
                                   "iii - morte acidental",
                                   "iv - morte súbita ou natural",
                                   "não informado"
                                 ),
                                 selected = c(
                                   "i - encontro de cadáver sem lesões aparentes",
                                   "ii - dúvidas razoáveis quanto a suicídio ou morte provocada",
                                   "iii - morte acidental",
                                   "iv - morte súbita ou natural",
                                   "não informado"
                                 ),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE
                               ),
                               sliderInput("idade_slider", "Filtrar por Idade:",
                                           min = -1, max = 123, value = c(-1,123)),
                               
                         actionButton("aplicar","Modificar os dados"),
                         actionButton("reset","Não usar Filtro"))  
    
    Side_iml <- fluidPage(
                       
                        
                        dateRangeInput('dateRange2',
                                              label = "Filtrar por data",
                                              start = "2016-04-01", end = "2022-12-31",
                                              min = "2016-04-01", max = "2022-12-31",
                                              separator = " - ", format = "dd/mm/yy",
                                              startview = 'year', language = 'pt-BR', weekstart = ),
                               
                        actionButton("aplicar","Modificar os dados"),
                        actionButton("reset","Não usar Filtro"))  
    
    Side_dad <- fluidPage(
                        
                         
                         dateRangeInput('dateRange2',
                                              label = "Filtrar por data",
                                              start = "2021-01-05", end = "2022-12-31",
                                              min = "2021-01-05", max = "2022-12-31",
                                              separator = " - ", format = "dd/mm/yy",
                                              startview = 'year', language = 'pt-BR', weekstart = 1),
                        pickerInput(
                                 inputId = "natureza",
                                 label = "Natureza da ocorrência", 
                                 choices = c(
                                   "estupro",
                                   "estupro de vulnerável",
                                   "furto - outros",
                                   "furto de carga",
                                   "furto de veículo",
                                   "homicídio doloso",
                                   "homicídio doloso em estab. prisional",
                                   "homicídio doloso por acidente de trânsito",
                                   "latrocínio",
                                   "lesão corporal seguida de morte",
                                   "roubo - outros",
                                   "roubo a banco",
                                   "roubo de carga",
                                   "roubo de veículo"
                                 ),
                                 selected = c(
                                   "estupro",
                                   "estupro de vulnerável",
                                   "furto - outros",
                                   "furto de carga",
                                   "furto de veículo",
                                   "homicídio doloso",
                                   "homicídio doloso em estab. prisional",
                                   "homicídio doloso por acidente de trânsito",
                                   "latrocínio",
                                   "lesão corporal seguida de morte",
                                   "roubo - outros",
                                   "roubo a banco",
                                   "roubo de carga",
                                   "roubo de veículo"
                                 ),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE
                               ),
                               
                            pickerInput(
                                 inputId = "descr_local",
                                 label = "Descrição do local da ocorrência", 
                                 choices = c(
                                   "area não ocupada",
                                   "centro comerc./empresarial",
                                   "comércio e serviços",
                                   "condomínio comercial",
                                   "condomínio residencial",
                                   "entidade assistencial",
                                   "escritório",
                                   "estabelecimento bancário",
                                   "estabelecimento comercial",
                                   "estabelecimento de ensino",
                                   "estabelecimento industrial",
                                   "estabelecimento prisional",
                                   "estacionamento com vigilância",
                                   "estacionamento particular",
                                   "estacionamento público",
                                   "estrada de ferro",
                                   "favela",
                                   "garagem coletiva de prédio",
                                   "garagem ou abrigo de residência",
                                   "hospedagem",
                                   "internet",
                                   "lazer e recreação",
                                   "local clandestino/ilegal",
                                   "não informado",
                                   "outros",
                                   "repartição pública",
                                   "residência",
                                   "restaurante e afins",
                                   "rodovia/estrada",
                                   "saúde",
                                   "serviços e bens públicos",
                                   "shopping center",
                                   "sindicato",
                                   "templo e afins",
                                   "terminal/estação",
                                   "unidade rural",
                                   "veículo em movimento",
                                   "via pública"
                                 ),
                                 selected = c(
                                   "area não ocupada",
                                   "centro comerc./empresarial",
                                   "comércio e serviços",
                                   "condomínio comercial",
                                   "condomínio residencial",
                                   "entidade assistencial",
                                   "escritório",
                                   "estabelecimento bancário",
                                   "estabelecimento comercial",
                                   "estabelecimento de ensino",
                                   "estabelecimento industrial",
                                   "estabelecimento prisional",
                                   "estacionamento com vigilância",
                                   "estacionamento particular",
                                   "estacionamento público",
                                   "estrada de ferro",
                                   "favela",
                                   "garagem coletiva de prédio",
                                   "garagem ou abrigo de residência",
                                   "hospedagem",
                                   "internet",
                                   "lazer e recreação",
                                   "local clandestino/ilegal",
                                   "não informado",
                                   "outros",
                                   "repartição pública",
                                   "residência",
                                   "restaurante e afins",
                                   "rodovia/estrada",
                                   "saúde",
                                   "serviços e bens públicos",
                                   "shopping center",
                                   "sindicato",
                                   "templo e afins",
                                   "terminal/estação",
                                   "unidade rural",
                                   "veículo em movimento",
                                   "via pública"
                                 ),
                                 options = list(
                                   `actions-box` = TRUE), 
                                 multiple = TRUE
                               ),
                               
                         actionButton("aplicar","Modificar os dados"),
                         actionButton("reset","Não usar Filtro"))  
    
    
    Side_rouc <-  fluidPage(
                         
                          
                          dateRangeInput('dateRange2',
                                               label = "Filtrar por data",
                                               start = "2010-01-01", end = "2022-12-31",
                                               min = "2010-01-01", max = "2022-12-31",
                                               separator = " - ", format = "dd/mm/yy",
                                               startview = 'year', language = 'pt-BR', weekstart = 1),
                          sliderTextInput(
                                  inputId = "quantidade_celular",
                                  label = "Quantidade furtada", 
                                  choices = c(
                                    "0", "1", "10", "100", "1000", "101", "1011", "102", "103", "104", "1050", "106", "107",
                                    "1080", "109", "11", "110", "1100", "111", "1111", "1115", "1117", "1119", "112", "113", "114",
                                    "1145", "115", "116", "1169", "117", "118", "119", "1197", "12", "120", "121", "1210", "1213", "123",
                                    "124", "125", "126", "127", "128", "1281", "129", "13", "130", "1300", "131", "132", "133", "134",
                                    "135", "1351", "1353", "1354", "1355", "1358", "137", "138", "14", "140", "1404", "141", "142", "143",
                                    "144", "146", "1470", "14728864", "148", "149", "15", "150", "1500", "1513", "152", "153", "1530",
                                    "155", "1550", "1553", "156", "1560", "157", "159", "16", "160", "1612", "1615", "162", "163", "1650",
                                    "1654", "166", "1665", "167", "1677", "1679", "168", "168455", "17", "170", "1707", "171", "1729",
                                    "1733", "174", "1741", "175", "176", "1777", "178", "1786", "179", "1798", "18", "180", "1800", "1804",
                                    "1808", "181", "1819", "182", "183", "184", "185", "1854", "186", "1867", "1871", "1880", "1883", "189",
                                    "19", "190", "1906", "191", "192", "1920", "193", "195", "1957", "196", "1965", "1966", "1968", "1971",
                                    "198", "1980", "1981", "1989", "199", "1990", "1996", "1999", "2", "20", "200", "2000", "201", "204",
                                    "207", "21", "210", "212", "213", "218", "219", "2199", "22", "220", "221", "222", "226", "228",
                                    "229", "23", "230", "231", "233", "239", "24", "240", "249", "25", "250", "2500", "251", "253",
                                    "256", "257", "258", "26", "260", "264", "265", "266", "27", "274", "2751", "28", "280", "2800",
                                    "29", "291", "2944", "297", "298", "3", "30", "300", "3000", "301", "305", "308", "309", "31",
                                    "3100", "311", "314", "32", "329", "33", "330", "34", "346", "3465", "348", "35", "350", "3551",
                                    "36", "37", "370", "376", "378", "38", "380", "384", "39", "4", "40", "41", "42", "424", "43",
                                    "433", "44", "4400", "45", "46", "47", "48", "483", "486", "488", "49", "492", "498", "499", "5",
                                    "50", "500", "5000", "501", "51", "52", "53", "54", "542", "547", "55", "551", "557", "56", "57",
                                    "58", "59", "6", "60", "600", "6000", "61", "612", "62", "621", "629", "63", "64", "65", "66",
                                    "660", "67", "670", "672", "68", "688", "69", "7", "70", "700", "71", "72", "720", "73", "730",
                                    "74", "75", "750", "751", "76", "768", "77", "779", "78", "785", "79", "799", "8", "80", "800",
                                    "804", "81", "815", "82", "83", "84", "85", "850", "87", "88", "880", "89", "899", "9", "90",
                                    "91", "92", "93", "930", "938", "94", "9413", "949", "9579", "96", "9675", "97", "98", "9844",
                                    "9860", "99", "999", "9999", "não informado"
                                  ),
                                  selected = c(
                                    "0", "1", "10", "100", "1000", "101", "1011", "102", "103", "104", "1050", "106", "107",
                                    "1080", "109", "11", "110", "1100", "111", "1111", "1115", "1117", "1119", "112", "113", "114",
                                    "1145", "115", "116", "1169", "117", "118", "119", "1197", "12", "120", "121", "1210", "1213", "123",
                                    "124", "125", "126", "127", "128", "1281", "129", "13", "130", "1300", "131", "132", "133", "134",
                                    "135", "1351", "1353", "1354", "1355", "1358", "137", "138", "14", "140", "1404", "141", "142", "143",
                                    "144", "146", "1470", "14728864", "148", "149", "15", "150", "1500", "1513", "152", "153", "1530",
                                    "155", "1550", "1553", "156", "1560", "157", "159", "16", "160", "1612", "1615", "162", "163", "1650",
                                    "1654", "166", "1665", "167", "1677", "1679", "168", "168455", "17", "170", "1707", "171", "1729",
                                    "1733", "174", "1741", "175", "176", "1777", "178", "1786", "179", "1798", "18", "180", "1800", "1804",
                                    "1808", "181", "1819", "182", "183", "184", "185", "1854", "186", "1867", "1871", "1880", "1883", "189",
                                    "19", "190", "1906", "191", "192", "1920", "193", "195", "1957", "196", "1965", "1966", "1968", "1971",
                                    "198", "1980", "1981", "1989", "199", "1990", "1996", "1999", "2", "20", "200", "2000", "201", "204",
                                    "207", "21", "210", "212", "213", "218", "219", "2199", "22", "220", "221", "222", "226", "228",
                                    "229", "23", "230", "231", "233", "239", "24", "240", "249", "25", "250", "2500", "251", "253",
                                    "256", "257", "258", "26", "260", "264", "265", "266", "27", "274", "2751", "28", "280", "2800",
                                    "29", "291", "2944", "297", "298", "3", "30", "300", "3000", "301", "305", "308", "309", "31",
                                    "3100", "311", "314", "32", "329", "33", "330", "34", "346", "3465", "348", "35", "350", "3551",
                                    "36", "37", "370", "376", "378", "38", "380", "384", "39", "4", "40", "41", "42", "424", "43",
                                    "433", "44", "4400", "45", "46", "47", "48", "483", "486", "488", "49", "492", "498", "499", "5",
                                    "50", "500", "5000", "501", "51", "52", "53", "54", "542", "547", "55", "551", "557", "56", "57",
                                    "58", "59", "6", "60", "600", "6000", "61", "612", "62", "621", "629", "63", "64", "65", "66",
                                    "660", "67", "670", "672", "68", "688", "69", "7", "70", "700", "71", "72", "720", "73", "730",
                                    "74", "75", "750", "751", "76", "768", "77", "779", "78", "785", "79", "799", "8", "80", "800",
                                    "804", "81", "815", "82", "83", "84", "85", "850", "87", "88", "880", "89", "899", "9", "90",
                                    "91", "92", "93", "930", "938", "94", "9413", "949", "9579", "96", "9675", "97", "98", "9844",
                                    "9860", "99", "999", "9999", "não informado"
                                  )[c(1,356)]
                                ),
                            pickerInput(
                                  inputId = "descr_local",
                                  label = "Descrição do local", 
                                  choices = c(
                                    "area não ocupada", "centro comerc./empresarial", "comércio e serviços",
                                    "condomínio comercial", "condomínio residencial", "entidade assistencial",
                                    "escritório", "estabelecimento bancário", "estabelecimento de ensino",
                                    "estabelecimento industrial", "estabelecimento prisional",
                                    "estacionamento com vigilância", "estacionamento particular",
                                    "estacionamento público", "estrada de ferro", "favela",
                                    "garagem coletiva de prédio", "garagem ou abrigo de residência", "hospedagem",
                                    "internet", "lazer e recreação", "local clandestino/ilegal", "não informado",
                                    "outros", "repartição pública", "residência", "restaurante e afins",
                                    "rodovia/estrada", "saúde", "serviços e bens públicos", "shopping center",
                                    "sindicato", "templo e afins", "terminal/estação", "unidade rural",
                                    "veículo em movimento", "via pública"
                                  ),
                                  selected = c(
                                    "area não ocupada", "centro comerc./empresarial", "comércio e serviços",
                                    "condomínio comercial", "condomínio residencial", "entidade assistencial",
                                    "escritório", "estabelecimento bancário", "estabelecimento de ensino",
                                    "estabelecimento industrial", "estabelecimento prisional",
                                    "estacionamento com vigilância", "estacionamento particular",
                                    "estacionamento público", "estrada de ferro", "favela",
                                    "garagem coletiva de prédio", "garagem ou abrigo de residência", "hospedagem",
                                    "internet", "lazer e recreação", "local clandestino/ilegal", "não informado",
                                    "outros", "repartição pública", "residência", "restaurante e afins",
                                    "rodovia/estrada", "saúde", "serviços e bens públicos", "shopping center",
                                    "sindicato", "templo e afins", "terminal/estação", "unidade rural",
                                    "veículo em movimento", "via pública"
                                  ),
                                  options = list(
                                    `actions-box` = TRUE), 
                                  multiple = TRUE
                                ),
                                
                          actionButton("aplicar","Modificar os dados"),
                          actionButton("reset","Não usar Filtro"))  
    
    Side_furc <- fluidPage(
                       
                        
                        dateRangeInput('dateRange2',
                                               label = "Filtrar por data",
                                               start = "2010-01-01", end = "2022-12-31",
                                               min = "2010-01-01", max = "2022-12-31",
                                               separator = " - ", format = "dd/mm/yy",
                                               startview = 'year', language = 'pt-BR', weekstart = 1),
                        sliderTextInput(
                                  inputId = "quantidade_celular",
                                  label = "Quantidade furtada", 
                                  choices = c(
                                    "0", "1", "10", "100", "1000", "1006", "1007", "101", "1011", "102", "103",
                                    "105", "106", "108", "1080", "109", "11", "110", "1100", "111", "1119", "112",
                                    "113", "1150", "116", "1161", "117", "1179", "1188", "119", "12", "120", "121",
                                    "122", "123", "124", "125", "126", "1270", "129", "13", "130", "131", "132",
                                    "133", "135", "1353", "1353133", "1354", "135401508", "1354410068891", "1355",
                                    "1357", "1358692", "1359", "136", "1367", "137", "138", "1382", "139", "14",
                                    "140", "142", "1440", "146", "147", "15", "150", "1500", "152", "154", "156",
                                    "157", "159", "1597", "16", "160", "162", "165", "166", "169", "1693", "17",
                                    "170", "171", "173", "176", "17731", "178", "1783", "18", "180", "181", "1815",
                                    "1818", "182", "1845", "19", "191", "1918", "1941", "195", "1951", "1959",
                                    "1966", "197", "1970", "1974", "198", "1999", "2", "20", "200", "2000", "203",
                                    "206", "208", "21", "210", "213", "214", "216", "217", "218", "22", "223",
                                    "2240", "23", "24", "240", "248", "249", "25", "250", "254", "255", "256",
                                    "258", "259", "26", "264", "265", "27", "270", "274", "278", "28", "280",
                                    "282", "286", "288", "29", "293", "2996", "3", "30", "300", "303", "304", "31",
                                    "310", "316", "319", "32", "320", "33", "34", "340", "35", "3500", "3531",
                                    "3581", "36", "360", "361", "364", "37", "373", "38", "39", "399", "4", "40",
                                    "400", "4000", "405", "41", "417", "42", "4204", "424", "426", "43", "433",
                                    "44", "45", "46", "47", "470", "48", "480", "49", "498", "5", "50", "500",
                                    "501", "51", "52", "522", "53", "54", "542", "547", "56", "560", "57", "570",
                                    "58", "59", "6", "60", "600", "6000", "61", "62", "63", "64", "647", "649",
                                    "65", "651", "66", "67", "68", "69", "697", "699", "7", "70", "700", "71",
                                    "710", "72", "73", "74", "749", "75", "750", "76", "77", "78", "79", "798",
                                    "8", "80", "800", "81", "84", "85", "86", "860", "87", "88", "89", "899", "9",
                                    "90", "900", "91", "92", "93", "95", "96", "961", "9736", "98", "99", "9999",
                                    "9999999", "99999999", "não informado"
                                  ),
                                  selected = c(
                                    "0", "1", "10", "100", "1000", "1006", "1007", "101", "1011", "102", "103",
                                    "105", "106", "108", "1080", "109", "11", "110", "1100", "111", "1119", "112",
                                    "113", "1150", "116", "1161", "117", "1179", "1188", "119", "12", "120", "121",
                                    "122", "123", "124", "125", "126", "1270", "129", "13", "130", "131", "132",
                                    "133", "135", "1353", "1353133", "1354", "135401508", "1354410068891", "1355",
                                    "1357", "1358692", "1359", "136", "1367", "137", "138", "1382", "139", "14",
                                    "140", "142", "1440", "146", "147", "15", "150", "1500", "152", "154", "156",
                                    "157", "159", "1597", "16", "160", "162", "165", "166", "169", "1693", "17",
                                    "170", "171", "173", "176", "17731", "178", "1783", "18", "180", "181", "1815",
                                    "1818", "182", "1845", "19", "191", "1918", "1941", "195", "1951", "1959",
                                    "1966", "197", "1970", "1974", "198", "1999", "2", "20", "200", "2000", "203",
                                    "206", "208", "21", "210", "213", "214", "216", "217", "218", "22", "223",
                                    "2240", "23", "24", "240", "248", "249", "25", "250", "254", "255", "256",
                                    "258", "259", "26", "264", "265", "27", "270", "274", "278", "28", "280",
                                    "282", "286", "288", "29", "293", "2996", "3", "30", "300", "303", "304", "31",
                                    "310", "316", "319", "32", "320", "33", "34", "340", "35", "3500", "3531",
                                    "3581", "36", "360", "361", "364", "37", "373", "38", "39", "399", "4", "40",
                                    "400", "4000", "405", "41", "417", "42", "4204", "424", "426", "43", "433",
                                    "44", "45", "46", "47", "470", "48", "480", "49", "498", "5", "50", "500",
                                    "501", "51", "52", "522", "53", "54", "542", "547", "56", "560", "57", "570",
                                    "58", "59", "6", "60", "600", "6000", "61", "62", "63", "64", "647", "649",
                                    "65", "651", "66", "67", "68", "69", "697", "699", "7", "70", "700", "71",
                                    "710", "72", "73", "74", "749", "75", "750", "76", "77", "78", "79", "798",
                                    "8", "80", "800", "81", "84", "85", "86", "860", "87", "88", "89", "899", "9",
                                    "90", "900", "91", "92", "93", "95", "96", "961", "9736", "98", "99", "9999",
                                    "9999999", "99999999", "não informado"
                                  )[c(1,283)]
                                ),
                                pickerInput(
                                  inputId = "descr_local",
                                  label = "Descrição do local", 
                                  choices = c(
                                    "area não ocupada", "carro forte", "centro comerc./empresarial", 
                                    "comércio e serviços", "condominio comercial", "condominio residencial", 
                                    "entidade assistencial", "escritório", "estabelecimento bancário", 
                                    "estabelecimento de ensino", "estabelecimento industrial", 
                                    "estabelecimento prisional", "estacionamento com vigilância", 
                                    "estacionamento particular", "estacionamento público", "estrada de ferro", 
                                    "favela", "garagem coletiva de prédio", "garagem ou abrigo de residência", 
                                    "hospedagem", "internet", "lazer e recreação", "local clandestino/ilegal", 
                                    "outros", "repartição pública", "residência", "restaurante e afins", 
                                    "rodovia/estrada", "saúde", "serviços e bens públicos", 
                                    "shopping center", "sindicato", "templo e afins", "terminal/estação", 
                                    "unidade rural", "veículo em movimento", "via pública"
                                  ),
                                  selected = c(
                                    "area não ocupada", "carro forte", "centro comerc./empresarial", 
                                    "comércio e serviços", "condominio comercial", "condominio residencial", 
                                    "entidade assistencial", "escritório", "estabelecimento bancário", 
                                    "estabelecimento de ensino", "estabelecimento industrial", 
                                    "estabelecimento prisional", "estacionamento com vigilância", 
                                    "estacionamento particular", "estacionamento público", "estrada de ferro", 
                                    "favela", "garagem coletiva de prédio", "garagem ou abrigo de residência", 
                                    "hospedagem", "internet", "lazer e recreação", "local clandestino/ilegal", 
                                    "outros", "repartição pública", "residência", "restaurante e afins", 
                                    "rodovia/estrada", "saúde", "serviços e bens públicos", 
                                    "shopping center", "sindicato", "templo e afins", "terminal/estação", 
                                    "unidade rural", "veículo em movimento", "via pública"
                                  ),
                                  options = list(
                                    `actions-box` = TRUE), 
                                  multiple = TRUE
                                ),
                                
                        actionButton("aplicar","Modificar os dados"),
                        actionButton("reset","Não usar Filtro"))  
    
    Side_rouv <- fluidPage(
                         
                          
                          dateRangeInput('dateRange2',
                                               label = "Filtrar por data",
                                               start = "2003-01-01", end = "2022-12-31",
                                               min = "2003-01-01", max = "2022-12-31",
                                               separator = " - ", format = "dd/mm/yy",
                                               startview = 'year', language = 'pt-BR', weekstart = 1
                                ),
                                pickerInput(
                                  inputId = "tipo_veiculo",
                                  label = "Tipo de veiculo", 
                                  choices = c(
                                    "automóvel",
                                    "bicicleta",
                                    "bonde",
                                    "caminhão",
                                    "caminhão trator",
                                    "caminhonete",
                                    "camioneta",
                                    "carro de mão",
                                    "carroça",
                                    "chassi-plataforma",
                                    "ciclomoto",
                                    "inexistente",
                                    "micro-ônibus",
                                    "motociclo",
                                    "motoneta",
                                    "motor casa",
                                    "não informado",
                                    "ônibus",
                                    "quadriciclo",
                                    "reboque",
                                    "semi-reboque",
                                    "side-car",
                                    "trator esteiras",
                                    "trator misto",
                                    "trator rodas",
                                    "triciclo",
                                    "utilitário"
                                  ),
                                  selected = c(
                                    "automóvel",
                                    "bicicleta",
                                    "bonde",
                                    "caminhão",
                                    "caminhão trator",
                                    "caminhonete",
                                    "camioneta",
                                    "carro de mão",
                                    "carroça",
                                    "chassi-plataforma",
                                    "ciclomoto",
                                    "inexistente",
                                    "micro-ônibus",
                                    "motociclo",
                                    "motoneta",
                                    "motor casa",
                                    "não informado",
                                    "ônibus",
                                    "quadriciclo",
                                    "reboque",
                                    "semi-reboque",
                                    "side-car",
                                    "trator esteiras",
                                    "trator misto",
                                    "trator rodas",
                                    "triciclo",
                                    "utilitário"
                                  ),
                                  options = list(
                                    `actions-box` = TRUE), 
                                  multiple = TRUE
                                ),
                                
                                pickerInput(
                                  inputId = "ano_veiculo",
                                  label = "Ano do veiculo", 
                                  choices = c(
                                    "0", "10", "1000", "1007", "1010", "1011", "1014", "1034", "1039", "1111",
                                    "1112", "1113", "113", "1180", "12", "120", "1200", "1212", "124", "125",
                                    "1313", "1314", "1316", "1318", "1367", "1414", "1415", "1418", "150", "1501",
                                    "1513", "1517", "1519", "160", "1600", "1617", "1618", "1620", "1634", "1718",
                                    "1720", "180", "1900", "1914", "1938", "1959", "1960", "1961", "1962", "1963",
                                    "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973",
                                    "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983",
                                    "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993",
                                    "1994", "1995", "1996", "1997", "1998", "1999", "200", "2000", "2001", "2002",
                                    "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                    "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
                                    "2023", "2033", "205", "2055", "206", "2066", "207", "208", "209", "2099",
                                    "2102", "2107", "211", "2112", "2113", "2122", "217", "219", "2204", "2206",
                                    "2209", "2220", "230", "2301", "2322", "2422", "2423", "2425", "2428", "2429",
                                    "250", "265", "275", "283", "2912", "2913", "292", "2924", "300", "315", "323",
                                    "4", "4000", "412", "416", "4275", "4283", "4292", "5", "50", "5049", "506",
                                    "521", "5310", "580", "60", "600", "608", "65", "6997", "7", "7180", "750",
                                    "8", "802", "809", "85", "9", "910", "915", "93", "97", "98", "não informado"
                                  ),
                                  selected = c(
                                    "0", "10", "1000", "1007", "1010", "1011", "1014", "1034", "1039", "1111",
                                    "1112", "1113", "113", "1180", "12", "120", "1200", "1212", "124", "125",
                                    "1313", "1314", "1316", "1318", "1367", "1414", "1415", "1418", "150", "1501",
                                    "1513", "1517", "1519", "160", "1600", "1617", "1618", "1620", "1634", "1718",
                                    "1720", "180", "1900", "1914", "1938", "1959", "1960", "1961", "1962", "1963",
                                    "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973",
                                    "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983",
                                    "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993",
                                    "1994", "1995", "1996", "1997", "1998", "1999", "200", "2000", "2001", "2002",
                                    "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012",
                                    "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022",
                                    "2023", "2033", "205", "2055", "206", "2066", "207", "208", "209", "2099",
                                    "2102", "2107", "211", "2112", "2113", "2122", "217", "219", "2204", "2206",
                                    "2209", "2220", "230", "2301", "2322", "2422", "2423", "2425", "2428", "2429",
                                    "250", "265", "275", "283", "2912", "2913", "292", "2924", "300", "315", "323",
                                    "4", "4000", "412", "416", "4275", "4283", "4292", "5", "50", "5049", "506",
                                    "521", "5310", "580", "60", "600", "608", "65", "6997", "7", "7180", "750",
                                    "8", "802", "809", "85", "9", "910", "915", "93", "97", "98", "não informado"
                                  ),
                                  options = list(
                                    `actions-box` = TRUE), 
                                  multiple = TRUE
                                ),
                                
                          actionButton("aplicar","Modificar os dados"),
                          actionButton("reset","Não usar Filtro"))  
    Side_furv <- fluidPage(
                        
                         
                         dateRangeInput('dateRange2',
                                               label = "Filtrar por data",
                                               start = "2003-01-01", end = "2022-12-31",
                                               min = "2003-01-01", max = "2022-12-31",
                                               separator = " - ", format = "dd/mm/yy",
                                               startview = 'year', language = 'pt-BR', weekstart = 1
                                ),
                        pickerInput(
                                  inputId = "tipo_veiculo",
                                  label = "Tipo de veiculo", 
                                  choices = c(
                                    "automóvel",
                                    "bicicleta",
                                    "caminhão",
                                    "caminhão trator",
                                    "caminhonete",
                                    "camioneta",
                                    "carroça",
                                    "charrete",
                                    "chassi-plataforma",
                                    "ciclomoto",
                                    "inexistente",
                                    "micro-ônibus",
                                    "motociclo",
                                    "motoneta",
                                    "motor casa",
                                    "não informado",
                                    "ônibus",
                                    "quadriciclo",
                                    "reboque",
                                    "semi-reboque",
                                    "side-car",
                                    "trator esteiras",
                                    "trator misto",
                                    "trator rodas",
                                    "triciclo",
                                    "utilitário"
                                  ),
                                  selected = c(
                                    "automóvel",
                                    "bicicleta",
                                    "caminhão",
                                    "caminhão trator",
                                    "caminhonete",
                                    "camioneta",
                                    "carroça",
                                    "charrete",
                                    "chassi-plataforma",
                                    "ciclomoto",
                                    "inexistente",
                                    "micro-ônibus",
                                    "motociclo",
                                    "motoneta",
                                    "motor casa",
                                    "não informado",
                                    "ônibus",
                                    "quadriciclo",
                                    "reboque",
                                    "semi-reboque",
                                    "side-car",
                                    "trator esteiras",
                                    "trator misto",
                                    "trator rodas",
                                    "triciclo",
                                    "utilitário"
                                  ),
                                  options = list(
                                    `actions-box` = TRUE), 
                                  multiple = TRUE
                                ),
                                
                                pickerInput(
                                  inputId = "ano_veiculo",
                                  label = "Ano do veiculo", 
                                  choices = c(
                                    -1, 1892, 1910, 1911, 1912, 1914, 1928, 1929, 1932, 1939, 1940,
                                    1944, 1945, 1946, 1948, 1949, 1950, 1951, 1952, 1954, 1955, 1957,
                                    1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,
                                    1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
                                    1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
                                    1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
                                    2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
                                    2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
                                    2090
                                  ),
                                  selected = c(
                                    -1, 1892, 1910, 1911, 1912, 1914, 1928, 1929, 1932, 1939, 1940,
                                    1944, 1945, 1946, 1948, 1949, 1950, 1951, 1952, 1954, 1955, 1957,
                                    1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,
                                    1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979,
                                    1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
                                    1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
                                    2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
                                    2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
                                    2090
                                  ),
                                  options = list(
                                    `actions-box` = TRUE), 
                                  multiple = TRUE
                                ),
                                
                        actionButton("aplicar","Modificar os dados"),
                        actionButton("reset","Não usar Filtro"))  
    
    vetor <- c(
      "id_bo"                  
      ,"id_laudo"     
      ,"datahora_iml_reg"         
      ,"index"                  
      ,"data_bo_reg"            
      ,"id_dp_reg"                  
      ,"data_ocorr"             
      ,"hora_ocorr"              
      ,"num_logradouro_ocorr"   
      ,"latitude_ocorr"         
      ,"longitude_ocorr"    
      ,"datahora_inicio_bo"          
      ,"datahora_bo_reg"     
      ,"marca_veiculo"          
      ,"cor_veiculo"            
      ,"cidade_veiculo"         
      ,"uf_veiculo"             
      ,"ano_fab_veiculo"        
      ,"ano_mod_veiculo"     
      ,"V1"                  
      ,"qtde_celular","marca_celular",
      "logradouro_ocorr")
  
    
    
    # Vetor de interesse
    vetor_interesse <- c("nome_dp_circ", "causa_morte_ocorr", "conclusao_ocorr", "idade_vitima",
                         "nome_dp_reg", "nome_municipio_circ", "nome_municipio_reg",
                         "descricao_local_ocorr", "vitima_fatal_ocorr", "tipo_pessoa",
                         "sexo_pessoa", "idade_pessoa", "cor_pele_pessoa", "profissao_pessoa",
                         "autoria_bo", "solucao_bo", "bairro_ocorr", "cidade_ocorr",
                         "flagrante_ocorr", "especie_ocorr", "status_ocorr", "desdobramento_ocorr",
                         "natureza_jur_ocorr", "tipo_veiculo", "coorp_envolvida_ocorr",
                         "coorp_situacao_ocorr", "descricao_conduta_ocorr",
                         "natureza_apurada_ocorr")
    
    # Criar um dicionário
    dicionario <- setNames(gsub("_", " ", gsub("ocorr", "", trimws(vetor_interesse))), vetor_interesse)
