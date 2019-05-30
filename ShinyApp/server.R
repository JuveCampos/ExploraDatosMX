#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$tm <- highcharter::renderHighchart({
    
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
    legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
    
    bd_treemap_shiny <- bd_tree_completa %>%
      filter(CICLO == input$Anio_Estatal)
  
    bd_tree_completa$DESC_RAMO %>% as.factor() %>% levels() 
    rplce <- c("CNDH" =  wesanderson::wes_palettes$GrandBudapest2[1],                                 
    "Defensa Nacional" =  wesanderson::wes_palettes$GrandBudapest2[2],                    
    "Gobernación"    =  wesanderson::wes_palettes$GrandBudapest2[3],                       
    "INAI"        =  wesanderson::wes_palettes$GrandBudapest2[4],                         
    "PGR" =  wesanderson::wes_palettes$Chevalier1[1],
    "Relaciones Exteriores"                =  wesanderson::wes_palettes$Chevalier1[2],
    "Seguridad Pública"             =  wesanderson::wes_palettes$Chevalier1[3])
    
    pal <- stringr::str_replace_all(levels(as.factor(bd_treemap_shiny$DESC_RAMO)), rplce)
    
    # bd_treemap_shiny <- bd_tree_completa %>%
    #   filter(CICLO == "2016")
    # 
    # print(bd_treemap_shiny$DESC_RAMO %>% as.factor() %>% levels())  
    # 
    tm <- treemap::treemap(bd_treemap_shiny,
                           index = c("DESC_RAMO", "DESC_PP"),
                           vSize = "montos",
                           vColor = "DESC_RAMO",
                           draw = FALSE,
                           fontfamily.title  = "Poppins",
                           fontsize.labels = 1,
                           palette = pal 
    )

    hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>%
      hc_title(text = paste0(" " ))  %>%
      hc_tooltip(pointFormat = "<b>{point.name}</b><br>
                                 Presupuesto: $ {point.value:,.0f}<br>") %>% 
      hc_plotOptions(series = list(stacking = FALSE, 
                                   events = list(click = canvasClickFunction, legendItemClick = legendClickFunction)))
    
  })
  
  
  output$plot1 <- renderPlot({
    b1 <- b1_ramo_year %>%
      filter(DESC_RAMO == input$Sel_ramo) %>%
      mutate(CICLO = as.character(CICLO))
    
    fillColor <- switch(input$Sel_ramo, 
                   "Comisión Nacional de los Derechos Humanos" =  wesanderson::wes_palettes$GrandBudapest2[1],                                 
                   "Defensa Nacional" =  wesanderson::wes_palettes$GrandBudapest2[2],                    
                   "Gobernación"    =  wesanderson::wes_palettes$GrandBudapest2[3],                       
                   "Instituto Nacional de Transparencia, Acceso a la Información y Protección de Datos Personales"        =  wesanderson::wes_palettes$GrandBudapest2[4],                         
                   "Procuraduría General de la República" =  wesanderson::wes_palettes$Chevalier1[1],
                   "Relaciones Exteriores"                =  wesanderson::wes_palettes$Chevalier1[2],
                   "Seguridad Pública"             =  wesanderson::wes_palettes$Chevalier1[3]
                   )
    print(input$Sel_ramo)
    
    ggplot(b1, aes(x = CICLO, y = aprobado)) + geom_bar(stat = 'identity', fill = fillColor) + 
      labs(title = "Evolución del presupuesto", 
           x = "Año", 
           y = "Presupuesto Aprobado (millones de Pesos)") +  tema_juve
    
  })
  
  output$plot2 <- renderPlotly({
    
    b2 <- b2_tipo_gasto %>%
      filter(DESC_RAMO == input$Sel_ramo) 
    
    p <- ggplot(b2, aes(x = CICLO, y = aprobado, color = DESC_TIPOGASTO)) + geom_line()  + tema_juve + 
      theme(legend.position="bottom", 
            legend.title=element_blank(), 
            legend.box.spacing = unit(-0.5, 'cm'), 
            legend.key = element_rect(color = NA, fill = NA), 
            legend.key.size = unit(0.5, "cm"),
            legend.spacing.x = unit(0.2, 'cm')) + 
      labs(title = "Evolución del gasto en el tiempo") + 
      scale_fill_manual(values = c(wesanderson::wes_palettes$Darjeeling1[1:3]) , aesthetics = "fill") + 
      scale_x_continuous(breaks = c(2013:2019))
            
    ggplotly(p)
    
  })
  
})
