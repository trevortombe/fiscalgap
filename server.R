server<-function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    data %>% filter(r %in% unique(input$rate/100),
                    health_inflation %in% unique(input$health_inflation/100),
                    labour_prod %in% unique(input$labour_prod/100),
                    years %in% unique(input$years),
                    pop_scenario %in% unique(input$pop_scenario)) %>%
      mutate(short=factor(short,levels=c('BC','AB','SK','MB','ON','QC','NB','NS','PE','NL','PROV','FED')))
  })
  
  # Load the FON logo
  img <- readPNG("FON_logo.png") 
  logo <- rasterGrob(img) 
  
  # Generate the Plot
  output$plot1 <- renderPlot({
    g1<-ggplot(selectedData(),aes(short,same_netdebt))+
      geom_col(fill='dodgerblue3')+
      geom_hline(yintercept=0,size=1)+
      theme_minimal()+
      scale_fill_brewer(name="",palette="Set1")+
      theme(panel.grid.minor = element_blank(),
            axis.title.y = element_text(size=10),
            axis.title.x = element_text(size=10),
            axis.text.y = element_text(size=10),
            legend.position="top",
            axis.text.x = element_text(size=10),
            plot.title = element_text(face = "bold",size=14),
            plot.subtitle = element_text(size = 10, color = "gray40"),
            plot.caption = element_text(size = 8, color = "gray40",hjust=1),
            panel.grid.major.x = element_blank())+
      scale_y_continuous(label=percent)+
      labs(x="",y="Share of GDP per Year",
           title="Long-Run Fiscal Gaps in Canada",
           subtitle="Source: Estimates from Tombe (2020, CTJ)",
           caption="Displays the hypothetical fiscal adjustment required for
sustainable debt over a 75-year horizon, with and without EQ.")
    
    # Add the logo to the plot
    g2 <- ggplot(mapping = aes(x = 0:1, y = 0)) +
      theme_void() +
      annotation_custom(logo, xmin = 0, xmax = .4)
    
    # Combine the plot and the logo
    gridExtra::grid.arrange(g1, g2, heights = c(.9, .1))
  },res=100)
  
  # Generate a summary table of the data underlying the figure
  output$table <- renderTable({
    selectedData() %>%
      mutate(same_netdebt=100*same_netdebt) %>%
      select(Province=short,
             `Fiscal Gap`=same_netdebt)
  })
  
}