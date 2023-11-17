#Server


server <- function(input, output, session) {
  estado <- reactiveVal("inicio")
  tema <- reactiveVal(NULL)
  dado1 <- reactiveVal(NULL)
  dado2 <- reactiveVal(NULL)
  dado3 <- reactiveVal(NULL)
  dado4 <- reactiveVal(NULL)
  dado5 <- reactiveVal(NULL)
  dado6 <- reactiveVal(NULL)
  dado7 <- reactiveVal(NULL)
  dado8 <- reactiveVal(NULL)
  dado9 <- reactiveVal(NULL)
  dado10 <- reactiveVal(NULL)
  dado11 <- reactiveVal(NULL)
  dado12 <- reactiveVal(NULL)
  
  dado_real <- reactiveVal(NULL)
  dado_filtrado <- reactiveVal(NULL)
  
  Side1 <- reactiveVal(NULL)
  m <- reactiveVal(NULL)
  
  startApp <- reactiveVal(TRUE)
  
  lista <- reactiveVal(list("inicial"))
  Lista_Trans <- reactiveVal(list(NULL))
  Lista_Serie <- reactiveVal(list(NULL))
  Lista_Tempo <- reactiveVal(c("semanal","mensal","trimestral","semestral","anual"))
  Lista_Diff <- reactiveVal(list(NULL))
  Serie_Atual <- reactiveVal(NULL)
  Serie_Tempo <- reactiveVal(NULL)
  Serie_Transformacao  <- reactiveVal(NULL)
  Granularidade  <- reactiveVal(NULL)
  Graph_Tempo <- reactiveVal(NULL)
  
  
  output$image <- shiny::renderImage({
    filename <- "ssp.png"  # Caminho para a imagem
    list(src = filename, contentType = "image/png")
  })
  
  shinyjs::disable("consulta")
  shinyjs::disable("exp")
  shinyjs::disable("map")
  shinyjs::disable("series")
  shinyjs::disable("diference")
  
  
  
  
  
  observe({
    dado_filtrado(dado_real())
  })
  
  observeEvent(input$button1,{
    estado("consulta")
  })
  
  
  criarObservador <- function(inputId, estado) {
    observeEvent(input[[inputId]], {
      estado(inputId)
    })
  }
  
  criarObservador2 <- function(inputId, tema) {
    observeEvent(input[[inputId]], {
      tema(inputId)
      shinyjs::enable("consulta")
      shinyjs::enable("exp")
      shinyjs::enable("map")
      shinyjs::enable("series")
      #shinyjs::enable("diference")
      
      output$inicial <- renderUI({ 
        mainPanel(
          fluidRow(
            column(12, align = "center", h1(paste0(tema()), style = "text-align: center;"))
          ),
          fluidRow(
            column(3,actionButton("tes", "Escolher outro tema", class = "custom-button") ),
            column(2,offset = 6, actionButton("button1", "Continuar", class = "custom-button"))),
          
          if (tema() == "IML") {
            
            Side1(Side_iml)
            
            if (is.null(dado7())) {
              dado7(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/iml_parcial.csv"))
              dado7(dado7() %>% mutate(data_ocorr = as.Date(datahora_iml_reg)))
              dado7(dado7() %>% mutate(data_bo_reg = as.Date(datahora_iml_reg)))
            }
            dado_real(dado7())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("Os dados apresentam informações básicas sobre todas as entradas de óbitos no IML desde 2013, quando foi implantado no Estado, pela Superintendência da Polícia Técnico-Científica, o serviço digital Gestão de Laudos (GDL). (Fonte: GDL da SPTC).")
              )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
          }
          
          
          else if (tema() == "Latrocínio") {
            Side1(Side_lat)
            
            
            if (is.null(dado3())) {
              dado3(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/latrocinio_parcial.csv"))
            }
            
            dado_real(dado3())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>O latrocínio é um crime hediondo descrito pelo parágrafo 3 do artigo 157 do código penal, é definido como a subtração de coisa móvel alheia mediante grave ameaça ou violência a pessoa, em que da violência resultar lesão corporal grave ou morte, sem agravantes ou alterações de pena descritas.</p>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de janeiro de 2018 a dezembro de 2022.</p>
      ")
              )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
            
            
          }
          
          
          else if (tema() == "LCSM") {
            Side1(Side_lcs)
            
            if (is.null(dado4())) {
              dado4(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/lesaoCSM_parcial.csv"))
            }
            
            dado_real(dado4())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>A lesão corporal seguida de morte é descrita pelo parágrafo 3 do artigo 129 do código penal, definida pela ofensa à integridade corporal ou saúde de outrem tendo por resultado a morte, com as circunstâncias evidenciando que o agente não quis o resultado, nem assumiu o risco de produzi-lo.</p>
        <p>É considerado agravado quando:</p>
        <ul>
          <li>Se a lesão for praticada contra ascendente, descendente, irmão, cônjuge ou companheiro, ou com quem conviva ou tenha convivido, ou, ainda, prevalecendo-se o agente das relações domésticas, de coabitação ou de hospitalidade</li>
          <li>Se a lesão for praticada contra autoridade ou agente descrito nos arts. 142 e 144 da Constituição Federal, integrantes do sistema prisional e da Força Nacional de Segurança Pública, no exercício da função ou em decorrência dela, ou contra seu cônjuge, companheiro ou parente consanguíneo até terceiro grau, em razão dessa condição, a pena é aumentada de um a dois terços</li>
          <li>Se a lesão for praticada contra a mulher, por razões da condição do sexo feminino</li>
        </ul>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
      ")
              )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
          }
          
          
          else if (tema() == "Roubo de Veículo") {
            Side1(Side_rouv)
            
            
            if (is.null(dado11())) {
              dado11(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/rouboVeiculo_parcial.csv"))
              dado11(  dado11() %>%
                         mutate(data_bo_reg = as.Date(datahora_bo_reg),
                                nome_municipio_circ=cidade_ocorr))
              
            }
            
            dado_real(dado11())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>O roubo é descrito no artigo 157 do Código Penal, é caracterizado pela subtração de bem material mediante grave ameaça ou violência, é agravado quando:</p>
        <ul>
          <li>Há o concurso de duas ou mais pessoas</li>
          <li>A vítima está em serviço de transporte de valores e o agente conhece tal circunstância</li>
          <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
          <li>O agente mantém a vítima em seu poder, restringindo sua liberdade</li>
          <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
          <li>A violência ou grave ameaça é exercida com emprego de arma branca ou arma de fogo</li>
          <li>Há destruição ou rompimento de obstáculo mediante o emprego de explosivo ou de artefato análogo que cause perigo comum</li>
        </ul>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
      ")
              )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
            
          }
          
          
          else if (tema() == "Furto de Veículo") {
            Side1(Side_furv)
            
            if (is.null(dado12())) {
              dado12(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/furtoVeiculo_parcial.csv"))
              dado12(  dado12() %>%
                         mutate(data_bo_reg = as.Date(datahora_bo_reg),
                                nome_municipio_circ=cidade_ocorr))
              
            }
            
            dado_real(dado12())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>O furto é descrito no artigo 155 do Código Penal, é caracterizado pela subtração de bem material alheio (destacando-se a ausência de ameaça grave ou violência), é agravado se:</p>
        <ul>
          <li>O crime é praticado durante o repouso noturno</li>
          <li>O crime é cometido com destruição ou rompimento de obstáculo à subtração da coisa</li>
          <li>O crime é cometido com abuso de confiança, ou mediante fraude, escalada ou destreza</li>
          <li>O crime é cometido com emprego de chave falsa</li>
          <li>O crime é cometido mediante concurso de duas ou mais pessoas</li>
          <li>Houver emprego de explosivo ou de artefato análogo que cause perigo comum</li>
          <li>O furto mediante fraude é cometido por meio de dispositivo eletrônico ou informático, conectado ou não à rede de computadores, com ou sem a violação de mecanismo de segurança ou a utilização de programa malicioso, ou por qualquer outro meio fraudulento análogo</li>
          <li>O crime é praticado mediante a utilização de servidor mantido fora do território nacional</li>
          <li>O crime é praticado contra idoso ou vulnerável</li>
          <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
          <li>A subtração for de semovente domesticável de produção, ainda que abatido ou dividido em partes no local da subtração</li>
          <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
        </ul>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
      ")            )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
          }
          
          
          else if (tema() == "MDIP") {
            Side1(Side_mdi)
            
            if (is.null(dado5())) {
              dado5(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/morteDIP_parcial.csv"))
            }
            
            dado_real(dado5())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>A morte decorrente de intervenção policial é um crime militar impróprio, tipificado no artigo 205 artigo 9º, inciso II, alínea b da Constituição Federal de 1988, caracterizado pelos abusos cometidos pelas forças policiais.</p>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
      ")            )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
          }
          
          
          else if (tema() == "Morte Suspeita") {
            Side1(Side_mor)
            
            if (is.null(dado6())) {
              dado6(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/morteSuspeita_parcial.csv"))
              dado6(dado6() %>% mutate(data_bo_reg = as.Date(datahora_bo_reg)))
            }
            
            dado_real(dado6())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>A morte suspeita é definida nos incisos I a IV, do Artigo 2º, da Portaria DGP nº 14/2005, descrita como:</p>
        <ol>
          <li>Encontro de cadáver sem lesões aparentes: Encontro de cadáver, ou parte relevante deste, em qualquer estágio de decomposição, no qual existam lesões aparentes ou quaisquer outras circunstâncias que, mesmo indiciariamente, apontem para a produção violenta da morte</li>
          <li>Dúvidas razoáveis quanto a suicídio ou morte provocada: Morte violenta em que subsistam dúvidas razoáveis quanto a tratar-se de suicídio ou morte provocada por outrem</li>
          <li>Morte acidental: Morte não natural onde existam indícios de causação acidental do evento exclusivamente por ato não intencional da própria vítima</li>
          <li>Morte súbita e natural: Morte súbita, sem causa determinante aparente, ocorrida de modo imprevisto, com a vítima fora do respectivo domicílio e sem a assistência de médico, familiar ou responsável</li>
        </ol>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
      ")            )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
          }
          
          
          else if (tema() == "Dados Criminais") {
            Side1(Side_dad)
            
            if (is.null(dado8())) {
              dado8(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/dadosCriminais_parcial.csv", header = TRUE))
            }
            
            dado_real(dado8())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("Contém todos os dados de boletins de ocorrência, dos temas presentes, do ano de 2022.")            )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
          }
          
          else if (tema() == "Roubo de Celular") {
            Side1(Side_rouc)
            
            if (is.null(dado9())) {
              dado9(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/rouboCelular_parcial.csv"))
              dado9(  dado9() %>%
                        mutate(data_bo_reg = as.Date(datahora_bo_reg),
                               nome_municipio_circ=cidade_ocorr
                        ))
              
            }
            
            dado_real(dado9())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>O roubo é descrito no artigo 157 do Código Penal, é caracterizado pela subtração de bem material mediante grave ameaça ou violência, é agravado quando:</p>
        <ul>
          <li>Há o concurso de duas ou mais pessoas</li>
          <li>A vítima está em serviço de transporte de valores e o agente conhece tal circunstância</li>
          <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
          <li>O agente mantém a vítima em seu poder, restringindo sua liberdade</li>
          <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
          <li>A violência ou grave ameaça é exercida com emprego de arma branca ou arma de fogo</li>
          <li>Há destruição ou rompimento de obstáculo mediante o emprego de explosivo ou de artefato análogo que cause perigo comum</li>
        </ul>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
      ")            )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
          }
          
          
          else if (tema() == "Furto de Celular") {
            Side1(Side_furc)
            column(8,
                   textOutput("furto_texto")
            )
            
            if (is.null(dado10())) {
              
              dado10(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/furtoCelular_parcial.csv"))
              dado10(  dado10() %>%
                         mutate(qtde_celular = ifelse(qtde_celular == "não informado", "-1", qtde_celular),
                                data_bo_reg = as.Date(datahora_bo_reg),
                                nome_municipio_circ=cidade_ocorr
                         ))
              
            }
            
            dado_real(dado10())
            
            output$furto_texto <- renderUI({
              fluidRow(
                column(8, 
                       HTML("
        <p>O furto é descrito no artigo 155 do Código Penal, é caracterizado pela subtração de bem material alheio (destacando-se a ausência de ameaça grave ou violência), é agravado se:</p>
        <ul>
          <li>O crime é praticado durante o repouso noturno</li>
          <li>O crime é cometido com destruição ou rompimento de obstáculo à subtração da coisa</li>
          <li>O crime é cometido com abuso de confiança, ou mediante fraude, escalada ou destreza</li>
          <li>O crime é cometido com emprego de chave falsa</li>
          <li>O crime é cometido mediante concurso de duas ou mais pessoas</li>
          <li>Houver emprego de explosivo ou de artefato análogo que cause perigo comum</li>
          <li>O furto mediante fraude é cometido por meio de dispositivo eletrônico ou informático, conectado ou não à rede de computadores, com ou sem a violação de mecanismo de segurança ou a utilização de programa malicioso, ou por qualquer outro meio fraudulento análogo</li>
          <li>O crime é praticado mediante a utilização de servidor mantido fora do território nacional</li>
          <li>O crime é praticado contra idoso ou vulnerável</li>
          <li>A subtração for de veículo automotor que venha a ser transportado para outro Estado ou para o exterior</li>
          <li>A subtração for de semovente domesticável de produção, ainda que abatido ou dividido em partes no local da subtração</li>
          <li>A subtração for de substâncias explosivas ou de acessórios que, conjunta ou isoladamente, possibilitem sua fabricação, montagem ou emprego</li>
        </ul>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de a .</p>
      ")
                )
              )
            })
            
          }
          
          
          else if (tema() == "Homicídio") {
            Side1(Side_hom)
            
            
            if (is.null(dado1())) {
              dado1(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/homicidioDoloso_parcial.csv"))
            }
            
            dado_real(dado1())
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
      <p>O homicídio doloso é descrito pelo artigo 121 do Código Penal, além de suas qualificações e alterações de pena. O crime é descrito pelo assassinato intencional de uma pessoa física por outra, constando agravantes tais como:</p>
      <ul>
        <li>Motivação fútil</li>
        <li>Mediante paga ou promessa de recompensa, com emprego de veneno, fogo, explosivo, asfixia, tortura ou outro meio insidioso ou cruel, ou de que possa resultar perigo comum</li>
        <li>À traição, de emboscada, ou mediante dissimulação ou outro recurso que dificulte ou torne impossível a defesa do ofendido</li>
        <li>Para assegurar a execução, a ocultação, a impunidade ou vantagem de outro crime</li>
        <li>Contra a mulher por razões da condição de sexo feminino (feminicídio)</li>
        <li>Contra autoridade ou agente descrito nos artigos 142 e 144 da Constituição Federal, integrantes do sistema prisional e da Força Nacional de Segurança Pública, no exercício da função ou em decorrência dela, ou contra seu cônjuge, companheiro ou parente consanguíneo até terceiro grau, em razão dessa condição</li>
        <li>Com emprego de arma de fogo de uso restrito ou proibido</li>
        <li>Contra menor de 14 anos</li>
      </ul>
      <p>A base traz algumas dessas qualificações incluindo feminicídio, trazendo boletins de janeiro de 2003 a dezembro de 2022.</p>
    ")
              )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
            
            
            
            
            
            
          }
          
          
          
          else if (tema() == "Feminicídio") {
            Side1(Side_fem)
            
            
            if (is.null(dado2())) {
              dado2(fread("C:/Users/zabuz/Desktop/Faculdade/Séries Temporais/Dados/feminicidio_parcial.csv"))
              dado2(dado2() %>% mutate(data_bo_reg = as.Date(datahora_bo_reg)))
            }
            
            dado_real(dado2())
            
            fluidRow(
              column(8, box(
                title = "Informações legais",
                width = 12,
                style = "background-color: #F0F8FF; border: 2px solid #4682B4;",
                HTML("
        <p>O feminicídio, tal qual o homicídio, é descrito pelo artigo 121 do Código Penal, é definido como o assassinato de uma mulher em razão das condições do sexo feminino, de forma que o homicídio torna-se feminicídio quando, em conjunto ao assassinato, há violência doméstica ou familiar e menosprezo ou discriminação à condição da mulher, tendo como agravantes quando o crime é praticado:</p>
        <ul>
          <li>Durante a gestação ou nos 3 meses posteriores ao parto</li>
          <li>Contra pessoa menor de 14 (catorze) anos, maior de 60 (sessenta) anos, com deficiência ou portadora de doenças degenerativas que acarretem condição limitante ou de vulnerabilidade física ou mental</li>
          <li>Na presença física ou virtual de descendente ou de ascendente da vítima</li>
          <li>Em descumprimento das medidas protetivas de urgência previstas nos incisos I, II e III do caput do art. 22 da Lei nº 11.340, de 7 de agosto de 2006</li>
        </ul>
        <p>A base traz boletins de ocorrência tangentes ao crime, com dados indo de abril de 2015 a dezembro de 2022.</p>
      ")          )
              ),
              column(4, box(
                title = "Informações da base",
                width = 12,
                style = "background-color: #DFF0D8; border: 2px solid #3C763D; text-align: center;",
                HTML("
      Informações da base ##")
              )
              ))
            
          }
          
          
        )
        
      })
    })
  }
  
  
  criarObservador("inicio", estado)
  criarObservador("series", estado)
  criarObservador("map", estado)
  criarObservador("diference", estado)
  criarObservador("info", estado)
  criarObservador("exp", estado)
  criarObservador("consulta", estado)
  
  criarObservador2("Homicídio", tema)
  criarObservador2("Feminicídio", tema)
  criarObservador2("Latrocínio", tema)
  criarObservador2("LCSM", tema)
  criarObservador2("MDIP", tema)
  criarObservador2("Morte Suspeita", tema)
  criarObservador2("IML", tema)
  criarObservador2("Dados Criminais", tema)
  criarObservador2("Roubo de Celular", tema)
  criarObservador2("Furto de Celular", tema)
  criarObservador2("Roubo de Veículo", tema)
  criarObservador2("Furto de Veículo", tema)
  
  render_initial_content <- function(){
    mainPanel(
      titlePanel("Escolha um tema"),
      br(),
      div(
        id = "mostrar_categ",
        
        fluidRow(id = "botoes_categorias_1",
                 column(2,
                        actionButton("Homicídio", "Homicídio", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Feminicídio", "Feminicídio", class = "custom-button",
                                     style = "margin-right: 50px;")
                 ),
                 column(2,
                        actionButton("Latrocínio", "Latrocínio", class = "custom-button")
                 ),
                 column(2,
                        actionButton("LCSM", "LCSM", class = "custom-button")
                 )
        ),
        fluidRow(id = "botoes_categorias_2",
                 column(2,
                        actionButton("MDIP", "MDIP", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Morte Suspeita", "Morte Suspeita", class = "custom-button")
                 ),
                 column(2,
                        actionButton("IML", "IML", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Dados Criminais", "Dados Criminais", class = "custom-button")
                 )
        ),
        fluidRow(id = "botoes_categorias_3",
                 column(2,
                        actionButton("Roubo de Celular", "Roubo de Celular", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Furto de Celular", "Furto de Celular", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Roubo de Veículo", "Roubo de Veículo", class = "custom-button")
                 ),
                 column(2,
                        actionButton("Furto de Veículo", "Furto de Veículo", class = "custom-button")
                 )
        )),
      hr())
  }
  
  output$inicial <- renderUI({
    render_initial_content()
  })
  
  observeEvent(input$inicio,{
    output$inicial <- renderUI ({
      render_initial_content()
      
    })
  })
  
  observeEvent(input$tes,{
    output$inicial <- renderUI ({
      render_initial_content()
      
    })
  })
  
  observe({
    # Verifica se é o início da aplicação
    if (is.null(input$inicio) && startApp()) {
      # Aciona a lógica desejada
      print("Botão Clicado ao iniciar a aplicação")
      # Atualiza a variável reativa para evitar repetições
      startApp(FALSE)
    }
  })
  
  observe(input$inicio)
  
  
  
  output$conteudo <- renderUI({
    conteudo <- switch(estado(),
                       "inicio" = {
                         
                         sidebarLayout(
                           sidebarPanel(
                             titlePanel("Painel Interativo de análises"),
                             fluidRow(
                               column(12,h3("Sobre")),
                               column(12, p("O PIA foi desenvolvido com base nos dados fornecidos pela Secretaria de Segurança Pública do Estado de São Paulo ao longo dos anos. Este portal oferece uma variedade de ferramentas que permitem aos usuários realizar análises exploratórias, criar visualizações gráficas e espaciais ao longo do tempo, tudo isso com a opção de filtrar ou não os dados conforme necessário.",
                               ))),
                             hr(), 
                             titlePanel("Objetivos"),
                             fluidRow(
                               column(12, p("Essa plataforma é o resultado do projeto da disciplina de Séries Temporais onde visa tornar acessível a análise de dados, permitino que pessoas de diferentes campos e níveis explorem insights de maneira intuitiva e personalizada. Como resultado a teoria é transportada para a pática.",
                               )))
                           ),
                           
                           uiOutput("inicial")
                         )
                         
                         
                       },
                       
                       "series" = {
                         fluidPage(
                           
                           fluidRow(
                             column(4,
                                    verticalLayout(
                                      h1(tema(),style = "text-align: center;"),
                                      sidebarPanel(width=12,
                                                   fluidRow(
                                                     column(10, 
                                                            h2("Transformação da série")
                                                     ),
                                                     br(),
                                                     column(1,
                                                            actionButton("toggle_button", "+")
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.toggle_button % 2 == 1",
                                                     pickerInput(
                                                       inputId = "serie_escolha",
                                                       label = "Série exibida",
                                                       choices = lista(),
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = FALSE, 
                                                       selected = lista()[1]
                                                     ),
                                                     pickerInput(
                                                       inputId = "cont_filtro",
                                                       label = "Selecione o tempo",
                                                       choices = Lista_Tempo(),
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = FALSE 
                                                     ),
                                                     pickerInput(
                                                       inputId = "cont_trans",
                                                       label = "Selecione a transformação da série",
                                                       choices = c("total","log","raiz quadrada","inversa","box cox"),
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = FALSE, 
                                                       selected = "total"  
                                                     ),
                                                     fluidRow(column(8,textInput("serie_nome", "De um nome a sua série", value = "Série transformada")),
                                                              column(3,br(),actionButton("serie_salvar","Salvar serie")))
                                                   )
                                      )),
                                    sidebarPanel(width=12,
                                                 fluidRow(
                                                   column(10, 
                                                          h2("Diferenciação da Série")
                                                   ),
                                                   br(),
                                                   column(1,
                                                          actionButton("toggle_button2", "+")
                                                   )
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.toggle_button2 % 2 == 1",
                                                   pickerInput(
                                                     inputId = "serie_escolha_diff",
                                                     label = "Selecione a série alvo:",
                                                     choices = lista()[-1],
                                                     options = list(
                                                       `actions-box` = TRUE),
                                                     multiple = FALSE, 
                                                     selected = NULL 
                                                   ),
                                                   pickerInput(
                                                     inputId = "Col_diff",
                                                     label = "Diferenciação",
                                                     choices = NULL,
                                                     options = list(
                                                       `actions-box` = TRUE),
                                                     multiple = FALSE
                                                   ),
                                                   fluidRow(column(6,
                                                                   numericInput("serie_lag", "Lag da série", 1, min = 1, max = 24, step = NA)),
                                                            column(6,numericInput("serie_ordem", "Ordem de diferenciação", 1, min = 1, max = 24, step = NA))),
                                                   
                                                   fluidRow(column(8,textInput("serie_nome2", "De um nome a sua série", value = "Série transformada")),
                                                            column(3,br(),actionButton("serie_gerar","Gerar serie")))
                                                   
                                                 )),
                                    sidebarPanel(width=12,
                                                 fluidRow(
                                                   column(10, 
                                                          h2("Testes")
                                                   ),
                                                   br(),
                                                   column(1,
                                                          actionButton("toggle_button3", "+")
                                                   )
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.toggle_button3 % 2 == 1",
                                                   pickerInput(
                                                     inputId = "serie_escolha_teste",
                                                     label = "Selecione a série salva para testar:",
                                                     choices = lista()[-1],
                                                     options = list(
                                                       `actions-box` = TRUE),
                                                     multiple = FALSE, 
                                                     selected = NULL 
                                                   ),
                                                   pickerInput(
                                                     inputId = "Col_diff2",
                                                     label = "Selecione o modo da série para testar:",
                                                     choices = NULL,
                                                     options = list(
                                                       `actions-box` = TRUE),
                                                     multiple = FALSE
                                                   ),
                                                   
                                                   fluidRow(
                                                            column(3, offset = 8, br(),actionButton("serie_testar","Testar série")))
                                                   
                                                 ))
                                    
                                    
                             ),
                             mainPanel(tabsetPanel(
                               tabPanel("Serie",
                                        plotlyOutput("cont"),
                               ),
                               tabPanel("Autocorrelação",
                                        plotlyOutput("auto")),
                               
                               tabPanel("Teste ADF",
                                        verbatimTextOutput("ADF")),
                               
                               tabPanel("Testes KPSS",
                                        fluidRow(
                                          box(title = "Teste de KPSS para estacionariedade",
                                              textOutput("KPSS")
                                          )
                                        ),
                                        
                                        fluidRow(
                                          box(title = "Teste KPSS número de diferenciações para estacionariedade",
                                              textOutput("KPSS_DIFF")
                                          )
                                        ),
                                        
                                        fluidRow(
                                          box(title = "Teste KPSS número de diferenciações para sazionalidade",
                                              textOutput("KPSS_SAZO")
                                          )
                                        )),
                               
                               tabPanel("Teste Cox Stuart",
                                        verbatimTextOutput("cox_stuart")),
                               
                               tabPanel("Teste de Kendal",
                                        verbatimTextOutput("kendal")),
                               
                               tabPanel("Teste WAVK",
                                        verbatimTextOutput("WAVK")),
                               
                               tabPanel("Objeto",
                                        tableOutput("summary"))
                             )
                             
                             )))
                         
                         
                         
                         
                         
                         
                         
                       }
                                 ,
                       
                       "map" = {
                         fluidRow(
                           leafletOutput("MAPA"),
                           absolutePanel(id = "controls", class = "panel panel-default",
                                         draggable = TRUE, height = "auto",
                                         h2("Teste de Painel"),
                                         h2("Teste de Painel"),
                                         h2("Teste de Painel"),
                                         h2("Teste de Painel")
                           ),
                           #verbatimTextOutput("cluster_legend")
                         )
                       },
                       "diference" = {
      
                         
                         
                         
                       },
                       "consulta" = {
                         fluidPage(
                           
                           fluidRow(
                             column(4,
                                    verticalLayout(
                                      h1(tema(),style = "text-align: center;"),
                                      Side1(),
                                      sidebarPanel(width=12,
                                                   fluidRow(
                                                     column(10, 
                                                            h2("Configurações avançadas")
                                                     ),
                                                     br(),
                                                     column(1,
                                                            actionButton("toggle_button2", "+")
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.toggle_button2 % 2 == 1",
                                                     pickerInput(
                                                       inputId = "colunas_selecionadas",
                                                       label = "Selecione as colunas:",
                                                       choices = colnames(dado_real()),
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = TRUE, 
                                                       selected = colnames(dado_real())  
                                                     )
                                                   )),
                                      sidebarPanel(width=12,
                                                   fluidRow(
                                                     column(5,
                                                            downloadButton("download_full","Download tema.csv")),
                                                     
                                                     column(6,
                                                            downloadButton("download_filtrado","Download tema_filtrado.csv")),
                                                   ))
                                      
                                    )),
                             
                             
                             mainPanel(DTOutput("tabela_1"))
                             
                           )
                         )
                         
                       },
                       "exp" = {
                         fluidPage(
                           
                           fluidRow(
                             column(4,
                                    verticalLayout(
                                      h1(tema(),style = "text-align: center;"),
                                      Side1(),
                                      sidebarPanel(width=12,
                                                   fluidRow(
                                                     column(10, 
                                                            h2("Configurações avançadas")
                                                     ),
                                                     br(),
                                                     column(1,
                                                            actionButton("toggle_button2", "+")
                                                     )
                                                   )
                                      ))),
                             
                             
                             mainPanel(tabsetPanel(
                               tabPanel("Cidades com mais ocorrências",
                                        br(),
                                        
                                        plotlyOutput("cidade")),
                               tabPanel("obs2"),
                               tabPanel("obs3")
                             ))
                             
                           )
                         )
                         
                       },
                       fluidRow(
                         column(12, h2("Conteúdo da página Padrão")),
                         column(12, p("Este é o conteúdo da página Padrão."))
                       )
    )
    
    return(conteudo)
  })
  
  observeEvent(input$toggle_sidebar, {
    if (shinyjs::hasClass("sidebarPanelId", "collapsed")) {
      shinyjs::removeClass("sidebarPanelId", "collapsed")
    } else {
      shinyjs::addClass("sidebarPanelId", "collapsed")
    }
  })
  
  observeEvent(input$aplicar, {
    ##limpar
    faixa_etaria <- NULL
    data <- NULL
    cor <- NULL
    tveiculo <- NULL
    anov <- NULL
    qtd_cel <- NULL
    descr_local <- NULL
    natureza <- NULL
    sexo <- NULL
    corp <- NULL
    ##
    faixa_etaria <- input$idade_slider
    data <- input$dateRange2
    cor <- input$cor_pele
    tveiculo <- input$tipo_veiculo
    anov <- input$ano_mod_veiculo
    qtd_cel <- input$quantidade_celular
    descr_local <- input$descr_local
    natureza <- input$natureza  
    sexo <- input$sexo
    corp <- input$corp 
    
    desdobramento <- input$desdobramento
    
    if (!is.null(faixa_etaria) || !is.null(data) || !is.null(cor) || !is.null(tveiculo) || !is.null(anov) || !is.null(qtd_cel) || !is.null(descr_local) || !is.null(natureza) || !is.null(sexo) || !is.null(desdobramento) || !is.null(corp)) {
      
      filtro <- TRUE
      
      if (!is.null(data)) {
        filtro <- filtro & (dado_real()$data_ocorr >= data[1] & dado_real()$data_ocorr <= data[2])
      }
      
      if(tema()== "Homicídio" | tema()=="Feminicídio" | tema()=="Latrocínio" | tema()=="LCSM" | tema()=="MDIP"){
        if (!is.null(cor)) {
          filtro <- filtro & (dado_real()$cor_pele %in% cor)
        }}
      
      if(tema()== "Homicídio" | tema()=="Feminicídio" | tema()=="Latrocínio" | tema()=="LCSM" | tema()=="MDIP" | tema()=="Morte Suspeita"){
        if (!is.null(faixa_etaria)) {
          filtro <- filtro & (dado_real()$idade_pessoa >= faixa_etaria[1] & dado_real()$idade_pessoa <= faixa_etaria[2])
        }}
      
      if (tema()== "Homicídio"| tema()== "Latrocínio" | tema()== "LCSM" | tema()=="MDIP" ) {
        if (!is.null(sexo)) {
          filtro <- filtro & (dado_real()$sexo_pessoa %in% sexo)
        }}
      
      if(tema()== "MDIP"){
        if (!is.null(corp)) {
          filtro <- filtro & (dado_real()$coorp_situacao_ocorr %in% corp)
        }}
      
      if(tema()=="Morte Suspeita"){
        if (!is.null(desdobramento)) {
          filtro <- filtro & (dado_real()$desdobramento_ocorr %in% desdobramento)
        }}
      
      if(tema()=="Roubo de Veículo" | tema() == "Furto de Veículo" ){
        if (!is.null(tveiculo)) {
          filtro <- filtro & (dado_real()$tipo_veiculo %in% tveiculo)
        }
        
        if (!is.null(anov)) {
          filtro <- filtro & (dado_real()$ano_veiculo %in% anov)
        }}
      
      if(tema()=="Roubo de Celular" | tema()=="Furto de Celular"){
        if (!is.null(qtd_cel)) {
          filtro <- filtro & (dado_real()$qtde_celular >= as.integer(qtd_cel[1]) & dado_real()$qtde_celular <= as.integer(qtd_cel[2]))
        }}
      
      if(tema()=="Roubo de Celular" | tema()=="Furto de Celular" | tema()=="Morte Suspeita" | tema()=="Dados Criminais"){
        if (!is.null(descr_local)) {
          filtro <- filtro & (dado_real()$descricao_local_ocorr %in% descr_local)
        }}
      
      if(tema()=="Dados Criminais"){
        if (!is.null(natureza)) {
          filtro <- filtro & (dado_real()$natureza_apurada_ocorr %in% natureza)
        }}
      
      
      dado_filtrado(dado_real()[filtro, ])
      
      faixa_etaria <- NULL
      data <- NULL
      cor <- NULL
      tveiculo <- NULL
      anov <- NULL
      qtd_cel <- NULL
      descr_local <- NULL
      natureza <- NULL
      sexo <- NULL
      corp <- NULL
      
    } 
  })
  
  output$tabela_1 <- renderDataTable({
    if (!is.null(dado_real())) {
      colunas_selecionadas <- input$colunas_selecionadas
      dados <- dado_filtrado()
      
      if (length(colunas_selecionadas) > 0) {
        dados <- dados[, ..colunas_selecionadas, drop = FALSE]
      }
      
      datatable(dados, 
                options = list(
                  paging = TRUE,
                  pageLength = 10,
                  lengthMenu = c(10, 25, 50),
                  searching = TRUE,
                  ordering = TRUE,
                  responsive = TRUE,
                  scrollX = TRUE
                ),
                rownames = FALSE,
                class = 'display'
      )}})
  
  observeEvent(input$inicio, {
    output$MAPA <- renderLeaflet({})
    
  })
  
  observeEvent(input$map, {
    output$MAPA <- renderLeaflet({
      df2 <- dado_real()
      df2$longitude_ocorr <- as.numeric(df2$longitude_ocorr)
      df2$latitude_ocorr <- as.numeric(df2$latitude_ocorr)
      df2 <- na.omit(df2)
      
      nocorr <- nrow(df2)  # Conta o número de ocorrências após limpar os dados
      
      m(leaflet(df2) %>%
          addTiles() %>%
          setView(lng = -46.5, lat = -23.5, zoom = 7) %>%
          
          addCircleMarkers(
            ~longitude_ocorr, ~latitude_ocorr,
            radius = 7,
            color = "blue",
            popup = ~paste("Location: ", nome_municipio_circ),
            label = ~nome_municipio_circ,
            clusterOptions = markerClusterOptions()
          ) 
        #addLegend(position = "bottomright", pal = "YlGnBu", values = c(nocorr / 10, nocorr / 5, nocorr / 4, nocorr / 2))
      )
      m() 
    })
  })
  
  
  
    
  ##Download
  output$download_full <- downloadHandler(
    filename = function() {
      paste(tema(), ".csv", sep = "")
    },
    content = function(file) {
      dados <- dado_real()
      write.csv(dados, file, row.names = FALSE)
    },
    contentType = "text/csv" 
  )
  
  output$download_filtrado <- downloadHandler(
    filename = function() {
      paste(tema(), "_filtrado.csv", sep = "")
    },
    content = function(file) {
      colunas_selecionadas <- input$colunas_selecionadas
      dados <- dado_filtrado()
      if (length(colunas_selecionadas) > 0) {
        dados <- dados[, ..colunas_selecionadas, drop = FALSE]
      }
      write.csv(dados, file, row.names = FALSE)
    }
  )
  
  output$cidade <- renderPlotly({
    
    dados <- dado_filtrado()
    
    contagem_municipios <- table(dados$nome_municipio_circ)
    
    
    
    
    
    
    # Ordenar a contagem em ordem decrescente
    contagem_municipios <- sort(contagem_municipios, decreasing = TRUE)
    
    # Selecionar os 10 municípios com mais ocorrências
    top_10_municipios <- names(contagem_municipios)[1:10]
    
    # Filtrar o dataframe para incluir apenas os 10 principais municípios
    
    dados_filtrados <- dados[dados$nome_municipio_circ %in% top_10_municipios, ]
    
    
    
    # Calcular a porcentagem das 10 maiores cidades em relação ao total
    porcentagem_total <- sum(contagem_municipios[1:10]) / sum(contagem_municipios) * 100
    
    
    plot <- plot_ly(data = dados_filtrados, x = ~nome_municipio_circ, type = 'histogram') %>%
      layout(
        title = "Top 10 Municípios com Mais Ocorrências",
        xaxis = list(title = "Nome do Município"),
        yaxis = list(title = "Contagem de Ocorrências"),
        legend = list(y = 1.1)
      )
    # Adicionar a legenda de porcentagem
    plot <- plot %>% add_annotations(
      x = 4,  # Ajuste a posição no eixo x
      y = 1.1*max(contagem_municipios),  # Ajuste a posição no eixo y (aumente o valor para movê-lo para cima)
      text = paste0("Top 10 cidades representam ", round(porcentagem_total, 2), "% do total"),
      showarrow = FALSE,
      font = list(size = 12)
    )
    
    
    plot
  })
  
  

  
  observeEvent(input$cont_trans,{
    transformacao <- input$cont_trans
    
    if (transformacao == "total") {
      Serie_Transformacao("total")
      
    } else if (transformacao == "log") {
      Serie_Transformacao("log")
      
    } else if (transformacao == "raiz quadrada") {
      Serie_Transformacao("sqrt")
     
    } else if (transformacao == "inversa") {
      Serie_Transformacao("inversa")
      
    } else if (transformacao == "box cox") {
      Serie_Transformacao("box_cox")
      
    }
    
  })
  
  
  observeEvent(input$cont_filtro,{
    tempo <- input$cont_filtro
    Granularidade(tempo)
    
    if (tempo == "semanal") {
      Serie_Tempo("semana")
      Graph_Tempo("Semana")
    } else if (tempo == "mensal") {
      Serie_Tempo("mes")
      Graph_Tempo("Mês")
    } else if (tempo == "trimestral") {
      Serie_Tempo("tri")
      Graph_Tempo("Trimestre")
    } else if (tempo == "semestral") {
      Serie_Tempo("sem")
      Graph_Tempo("Semestre")
    } else if (tempo == "anual") {
      Serie_Tempo("ano")
      Graph_Tempo("Ano")
    }
    
  })
  
  observeEvent(input$serie_escolha,{
    serie_escolhida <- input$serie_escolha
    if (serie_escolhida == "inicial"){
      Lista_Tempo(c("semanal","mensal","trimestral","semestral","anual"))
      Lista_Trans(c("total","log","raiz quadrada","inversa","box cox"))
      updatePickerInput(session, "cont_filtro", choices = Lista_Tempo(), selected = "semanal") 
      updatePickerInput(session, "cont_trans", choices = Lista_Trans(), selected = "total")
    } else{
      tempo <- Lista_Serie()[[serie_escolhida]]
      Lista_Trans(c(colnames(tempo)[2],"log","raiz quadrada","inversa","box cox"))
      updatePickerInput(session, "cont_filtro", choices = colnames(tempo)[1] , selected = colnames(tempo)[1])
      updatePickerInput(session, "cont_trans", choices = Lista_Trans(), selected = Lista_Trans()[1])
      
      
      
      
      }
    
    
    
  })
  
  
  
  Serie_Atual <- reactive({
    serie_escolhida <- input$serie_escolha
    if (serie_escolhida == "inicial"){
     
     k <- dados_filtrados_tempo(dado_filtrado(),Serie_Tempo())
     
     k <- get_transformacoes(k,Serie_Transformacao())
     
     colnames(k)[1] <- Granularidade()
     
     k} else{
       name_serie <- input$serie_escolha
       k <- Lista_Serie()[[name_serie]]
       
       k <- get_transformacoes(k,Serie_Transformacao())
       
       k
     }
  })
  
  
  
  
  output$cont <- renderPlotly({
    Graph <- NULL
    transformacao <- Serie_Transformacao()
    Graph <- autoplot(Serie_Atual(), .vars = !!sym(transformacao) ) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Número de ocorrências por ', Graph_Tempo())) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(Graph)
  })
  
  output$auto <- renderPlotly({
    Graph <- NULL
    transformacao <- Serie_Transformacao()
    Graph <- autoplot(ACF(Serie_Atual(), .vars = !!sym(transformacao))) +
      theme_pubclean() +
      xlab(Graph_Tempo()) +
      ggtitle(paste('Número de ocorrências por ', Graph_Tempo())) +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(Graph)
   })
  
  
  observeEvent(input$serie_salvar,{
    Nome_Objeto_Serie <- input$serie_nome
    
    
    #Salvando o nome da série na lista
    if ((Nome_Objeto_Serie %in% lista() == FALSE)){
    listinha <- lista()
    listinha <- c(listinha,Nome_Objeto_Serie)
    lista(listinha)
    updatePickerInput(session, "serie_escolha", choices = lista())
    
   # Salvando o objeto série
    Objeto_Serie <- Serie_Atual()
    
    if (input$cont_trans != "total"){
      Objeto_Serie <- Objeto_Serie[-2]
    
      colnames(Objeto_Serie)[2] <- paste("total")}
    
    lista_de_serie <- Lista_Serie()
    
    lista_de_serie[[Nome_Objeto_Serie]] <- Objeto_Serie
    
    Lista_Serie(NULL)
    Lista_Serie(lista_de_serie)
    
  
    }
  })
  
  output$summary <- renderTable({
   Serie_Atual()})
  
  
  
  observeEvent(input$serie_escolha_diff,{
    name_serie <- input$serie_escolha_diff
    
    k <- Lista_Serie()[[name_serie]]
    
    k<- colnames(k)[-1]
    
    Lista_Diff(k)
    
    
    updatePickerInput(session, "Col_diff", choices = Lista_Diff())
  })
  
  observe_serie_escolha_diff <- function() {
    observeEvent(input$serie_escolha_diff, {
      name_serie <- input$serie_escolha_diff
      k <- Lista_Serie()[[name_serie]]
      k <- colnames(k)[-1]
      Lista_Diff(k)
      updatePickerInput(session, "Col_diff", choices = Lista_Diff())
    })
  }
  
  observeEvent(input$serie_gerar,{
    
    shinyjs::enable("diference")
    
    novo_item <- input$serie_nome2
    
    name_serie <- input$serie_escolha_diff
    
    j <- Lista_Serie()
    
    k <- j[[name_serie]]
    
    k <- get_difference(k,input$serie_ordem, input$serie_lag,!!sym(novo_item),input$Col_diff)
    
    j[[name_serie]] <- k
    
    Lista_Serie(NULL) 
    
    Lista_Serie(j)
    
    
    observe_serie_escolha_diff()
  })
  
  
  
  observe_serie_escolha_diff2 <- function() {
    observeEvent(input$serie_escolha_teste, {
      name_serie <- input$serie_escolha_teste
      k <- Lista_Serie()[[name_serie]]
      k <- colnames(k)[-1]
      Lista_Diff(k)
      updatePickerInput(session, "Col_diff2", choices = Lista_Diff())
    })
  }
  
  observeEvent(input$serie_escolha_teste,{
    observe_serie_escolha_diff2()
  })
  
  
  output$ADF <- renderPrint({
    name_serie <- input$serie_escolha_teste
    k <- Lista_Serie()[[name_serie]]
    
    kk <- teste_adf(k,input$Col_diff2)
    
    kk
  })
  
  output$KPSS <- renderText({
    name_serie <- input$serie_escolha_teste
    k <- Lista_Serie()[[name_serie]]
    kk <- teste_kpss(k,!!sym(input$Col_diff2))
    paste("Kpss_stat: ", round(kk[1],3),"   ", "KPSS_Pvalue: ", round(kk[2],3))
    })
  
  output$KPSS_DIFF <- renderText({
    name_serie <- input$serie_escolha_teste
    k <- Lista_Serie()[[name_serie]]
    kk <- teste_kpss_numero_diff(k,!!sym(input$Col_diff2))
    paste("Número de diferenciações: ", round(kk[1],3))
  })
  
  
  output$KPSS_SAZO <- renderText({
    name_serie <- input$serie_escolha_teste
    k <- Lista_Serie()[[name_serie]]
    kk <- teste_kpss_season_diff(k,!!sym(input$Col_diff2))
    paste("Número de diferenciações: ", round(kk[1],3))
  })
  
  output$cox_stuart <- renderPrint({
    name_serie <- input$serie_escolha_teste
    k <- Lista_Serie()[[name_serie]]
    
    kk <- teste_cox_stuart(k,!!sym(input$Col_diff2))
    
    kk
  })
  
  output$kendal <- renderPrint({
    name_serie <- input$serie_escolha_teste
    k <- Lista_Serie()[[name_serie]]
    
    kk <- teste_kendall(k,!!sym(input$Col_diff2))
    
    kk
  })
  
  
  output$WAVK <- renderPrint({
    name_serie <- input$serie_escolha_teste
    k <- Lista_Serie()[[name_serie]]
    
    kk <- teste_WAVK(k,!!sym(input$Col_diff2))
    
    kk
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

}





shinyApp(ui, server)
