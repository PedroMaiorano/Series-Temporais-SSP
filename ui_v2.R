  # Ui
  ui <- fluidPage(
    useShinyjs(),
    tags$style(
      HTML(
        "
        #controls {
      position: absolute;
      top: 200px; /* Posição inicial no eixo Y */
      left: 1150px; /* Posição inicial no eixo X */
      width: 250px; /* Largura do painel */
      background-color: rgba(255, 255, 255, 0.25); /* Fundo semi-transparente */
      border: 1px solid #000000; /* Borda do painel */
      padding: 10px; /* Espaçamento interno */
    }
        .box-title {
            text-align: center;
        }
       
    .ul li.custom-tab1 {
      width: 150px;
    }
    
    .ul li.custom-tab2 {
      width: 250px;
    }'
    
       .meu-codigo {
        display: block;
        padding: 9.5px;
        margin: 0 0 10px;
        margin-top: 10px;
        font-size: 13px;
        line-height: 1.6; 
        word-break: break-all;
        word-wrap: break-word;
        white-space: pre-wrap;
        background-color: #F5F5F5;
        border: 1px solid rgba(0, 0, 0, 0.15);
        border-radius: 4px;
        font-family: Arial, sans-serif; 
      }
      .meu-codigo ul {
        margin-top: 10px; 
        list-style-type: disc; 
        padding-left: 20px; 
        margin-bottom: 10px; /* Reduz o espaçamento abaixo da lista */
      }
      .meu-codigo ul li {
        font-size: 14px; 
        margin-bottom: 5px; /* Reduz o espaçamento entre os itens da lista */
      }
      .meu-codigo p {
        margin-top: 5px; /* Reduz o espaçamento acima dos parágrafos */
        margin-bottom: 5px; /* Reduz o espaçamento abaixo dos parágrafos */
      }
        
        
      hr {border-top: 1px solid #000000;}
      .custom-button {
      width: 150px;
      height: 40px;
      margin-bottom: 15px;
      background-color: #11104d;
      color: white;
      border: 2px solid #000000;
      border-radius: 5px;
      padding: 10px;
      font-size: 15px;
      font-family: inherit;
      cursor: pointer;
      }
      
      html, body, .container.fluid {
        width: 100vw;
        min-height: 100vh;
        
      }
      
     
      
      .page-layout {
        display: grid;
        grid-template-columns: minmax(0, 1fr);
        grid-template-rows: 80px 1fr;
        grid-template-areas:
            'header'
            'main ';
        height: 100vh;
        width: 100%;
        max-width: 100vw;
        overflow-x: hidden;
      }
      
      .page-header {
        grid-area: header;
      }
      .page-main {
        grid-area: main;
        transition: opacity .5s ease;
        visibility: none;
        z-index: -1;
        opacity: 0;
      }
      
      .main-layout {
        display: grid;
        grid-template-columns: minmax(0, 1fr);
        grid-template-rows: auto minmax(0, 1fr);
        grid-template-areas:
          'sidebar'
          'main';
        height: 100%;
        width: 100%;
        padding: 20px;
        gap: 20px;
      }
      @media only screen and (min-width: 800px) {
        .main-layout {
          grid-template-columns: minmax(0, 350px) minmax(0, 1fr);
          grid-template-rows: minmax(0, 1fr);
          grid-template-areas:
            'sidebar main';
        }
      }
      
      .card-style {
        border: 1px solid #eaeaea;
        padding: 20px;
        border-radius: 5px;
        box-shadow: rgb(0 0 0 / 10%) 3px 8px 12px;
        overflow: hidden;
      }
      .no-padding {
        padding: 0;
      }
      
      .main-main {
        grid-area: main;
      }
      .main-sidebar {
        grid-area: sidebar;
      }
      
      #map-map {
        width: 100% !important;
        height: 100% !important;
      }
      
      .navbar-top {
        display: grid;
        grid-template-columns: 140px 1fr auto 1fr 150px 40px;
        grid-template-rows: minmax(0, 1fr);
        grid-template-areas:
          'logo ... menu ... dataset info';
        height: 100%;
        width: 100%;
        padding: 20px;
        gap: 20px;
        
      }
      @media only screen and (max-width: 600px) {
        .navbar-top {
          grid-template-columns: 140px 1fr 1fr 85px;
          grid-template-rows: minmax(0, 1fr) minmax(0, 1fr);
          grid-template-areas:
            'logo ...   ...     info'
            'menu menu  dataset dataset';
          padding: 0;
          gap: 0;
        }
      }
      
      .navbar-top {
        background-color: #11104d;
        }
      
      .navigation-logo {
        grid-area: logo;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .navigation-menu {
        grid-area: menu;
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 10px;
      }
      @media only screen and (max-width: 600px) {
        .navigation-menu {
          justify-content: flex-start;
        }
      }
      
      .navigation-dataset {
        grid-area: dataset;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .navigation-dataset .form-group {
        margin: 0;
      }
      
      .navigation-info {
        grid-area: info;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      
      #MAPA {height: calc(100vh - 80px) !important;}
      
      
      
      .sidebarPanel {
      background-color: #FFC0CB
      }
      
      
      .tabbable > .nav > li > a{
      background-color:#FFFFFF;
      color: black
      }
      
      body {background-color: #FFFFFF;}
      
      
      "
      )
    ),
    
    # Cabeçalho da página
    tags$div(
      # Logotipo
      tags$div(
        tags$a(
          shiny::imageOutput("image",  width = "60px", height = "60px")  # Exibe a imagem renderizada pelo servidor
        ),
        id = "logo-top",
        class = "navigation-logo"
      ),
      
      # Botões de navegação
      tags$div(
        tags$div(
          actionButton(
            inputId = "inicio",
            label = "Início",
            class = "navbar-button",
            icon = icon("home"),
            value = 1
          ),
          id = "div-navbar-inicio"
        ),
        
        tags$div(
          actionButton(
            inputId = "consulta",
            label = "Dados",
            class = "navbar-button",
            icon = icon("table")
          ),
          id = "div-navbar-como-usar"
        ),
        tags$div(
          actionButton(
            inputId = "map",
            label = "Mapa",
            class = "navbar-button",
            icon = icon("map")
          ),
          id = "div-navbar-map"
        ),
        tags$div(
          actionButton(
            inputId = "series",
            label = "Séries Temporais",
            class = "navbar-button",
            icon = icon("clock")
          ),
          id = "div-navbar-series"
        ),
        tags$div(
          actionButton(
            inputId = "decomp",
            label = "Modelo",
            class = "navbar-button",
            icon = icon("warning")
          ),
          id = "div-navbar-pred"
        ),
        
        id = "div-navbar-tabs",
        class = "navigation-menu"
      ),
      # Ícone de informações  
      tags$div(
        actionButton(
          inputId = "info",
          label = NULL,
          icon = icon("info-sign", lib = "glyphicon"),
          class = "navbar-info"
        ),
        class = "navigation-info"
      ),
      
      id = "div-navbar",
      class = "navbar-top page-header card-style"
    ),
    
    uiOutput("conteudo")
  )
