library(ggplot2)
library(plotly)
library(plyr)
library(shiny)
library(shinydashboardPlus)
library(shinythemes)

source("helper.R")
function(input, output, session) {
  
  
  # ############################ Observe League name ################
  # observeEvent(input$lsc_selector, {
  #   cat("League: ", input$lsc_selector, "\n")  })
  # ########## Print League Name  ################ 
  # liga_nome <- eventReactive(input$lsc_selector,{
  #   input$lsc_selector  })
  
  # output$league = renderText({
  #   liga_nome() })
  
  # # ############################ Observe Item Type ###########################################
  # observeEvent(input$item_type_lsc, {
  #   cat("Item type: ", input$item_type_lsc, "\n")})
  # # ######### Print item Type ########
  # item_type_print <- eventReactive(input$item_type_lsc, {
  #   input$item_type_lsc})
  # 
  # output$item_type_print = renderText({
  #  item_type_print() })
  
  
  
  
  
  ########################################################
  #### LSC Server Data ####
  ###################### Reactive in item_type_lsc ################
  tipo_do_item <- reactive({
    filter(subset_names, Type == input$item_type_lsc)})
  
  observeEvent(tipo_do_item(), {
    choices <- unique(tipo_do_item()$Name)
    updateSelectInput(session, "item_name_lsc", choices = choices) })
  
  observeEvent(input$item_name_lsc, {
    cat("Item name: ", input$item_name_lsc, "\n") })
  
  #Filter to link item_type_lsc to the axes in the plot.
  sub_item_type <- eventReactive(input$item_type_lsc,{
    subset(q, Type == input$item_type_lsc)}) 
  sub_item_type <- eventReactive(input$item_type_lsc,{
    if (input$item_type_lsc == "Beast"){
      Beast
    }
    else if (input$item_type_lsc == "Currency"){
      Currency
    }
    else if (input$item_type_lsc == "Incubator"){
      Incubator
    }
    else if (input$item_type_lsc == "Oil"){
      Oil
    }
    else if (input$item_type_lsc == "Fossil"){
      Fossil
    }
    else if (input$item_type_lsc == "Resonator"){
      Resonator
    }
    else if (input$item_type_lsc == "Scarab"){
      Scarab
    }
    else if (input$item_type_lsc == "HelmetEnchant"){
      HelmetEnchant
    }
    else if (input$item_type_lsc == "Map"){
      Map
    }
    else if (input$item_type_lsc == "Prophecy"){
      Prophecy
    }
    else if (input$item_type_lsc == "UniqueJewel"){
      UniqueJewel
    }
    else if (input$item_type_lsc == "UniqueFlask"){
      UniqueFlask
    }
    else if (input$item_type_lsc == "Essence"){
      Essence
    }
    else if (input$item_type_lsc == "DivinationCard"){
      DivinationCard
    }
    else if (input$item_type_lsc == "UniqueAccessory"){
      UniqueAccessory
    }
    else if (input$item_type_lsc == "UniqueArmour"){
      UniqueArmour
    }
    else if (input$item_type_lsc == "UniqueWeapon"){
      UniqueWeapon
    }
    else if (input$item_type_lsc == "BaseType"){
      BaseType
    }
    else if (input$item_type_lsc == "SkillGem"){
      SkillGem
    }
    else if (input$item_type_lsc == "UniqueMap"){
      UniqueMap
    }
    else if (input$item_type_lsc == "Vial"){
      Vial
    }
    else if (input$item_type_lsc == "Watchstone"){
      Watchstone
      
    }
    else if (input$item_type_lsc == "Fragment") {
      Fragment
    }
  })
  
  #### Main Plot LSC ####
  output$plot_lsc = renderPlotly({
    ggplotly(
      ggplot(data = sub_item_type()[sub_item_type()$League %in% input$lsc_selector,],
             aes_string(x = "Day", y = make.names(input$item_name_lsc))) +
        geom_line(aes(colour = League)) +
        geom_area(aes(colour = League), position = "identity", alpha = 0.1) +
        scale_x_continuous(breaks = seq(0, length(sub_item_type()$Day), by=5))+
        theme(legend.position = "bottom",
              axis.line = element_line(colour = "black",size = 2),
              panel.grid.minor = element_blank(),
              panel.grid.major =  element_line(color = "gray90"),
              panel.background = element_blank(),
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold")) 
    )
  })
  #########################################################
  
  #########################################################  
  #### LHC Server data ####
  ###################### Reactive in item_type_lhc ################
  tipo_do_item_lhc <- reactive({
    filter(subset_names, Type == input$item_type_lhc)})
  
  observeEvent(tipo_do_item_lhc(), {
    choices <- unique(tipo_do_item_lhc()$Name)
    updateSelectInput(session, "item_name_lhc", choices = choices) })
  
  observeEvent(input$item_name_lhc, {
    cat("Item name: ", input$item_name_lhc, "\n") })
  
  
  
  #Filter to link item_type_lhc to the axes in the plot.
  sub_item_type_lhc <- eventReactive(input$item_type_lhc,{
    subset(t, Type == input$item_type_lhc)}) 
  sub_item_type_lhc <- eventReactive(input$item_type_lhc,{
    if (input$item_type_lhc == "Beast"){
      Beast_lhc
    }
    else if (input$item_type_lhc == "Currency"){
      Currency_lhc
    }
    else if (input$item_type_lhc == "Incubator"){
      Incubator_lhc
    }
    else if (input$item_type_lhc == "Oil"){
      Oil_lhc
    }
    else if (input$item_type_lhc == "Fossil"){
      Fossil_lhc
    }
    else if (input$item_type_lhc == "Resonator"){
      Resonator_lhc
    }
    else if (input$item_type_lhc == "Scarab"){
      Scarab_lhc
    }
    else if (input$item_type_lhc == "HelmetEnchant"){
      HelmetEnchant_lhc
    }
    else if (input$item_type_lhc == "Map"){
      Map_lhc
    }
    else if (input$item_type_lhc == "Prophecy"){
      Prophecy_lhc
    }
    else if (input$item_type_lhc == "UniqueJewel"){
      UniqueJewel_lhc
    }
    else if (input$item_type_lhc == "UniqueFlask"){
      UniqueFlask_lhc
    }
    else if (input$item_type_lhc == "Essence"){
      Essence
    }
    else if (input$item_type_lhc == "DivinationCard"){
      DivinationCard_lhc
    }
    else if (input$item_type_lhc == "UniqueAccessory"){
      UniqueAccessory_lhc
    }
    else if (input$item_type_lhc == "UniqueArmour"){
      UniqueArmour_lhc
    }
    else if (input$item_type_lhc == "UniqueWeapon"){
      UniqueWeapon_lhc
    }
    else if (input$item_type_lhc == "BaseType"){
      BaseType_lhc
    }
    else if (input$item_type_lhc == "SkillGem"){
      SkillGem_lhc
    }
    else if (input$item_type_lhc == "UniqueMap"){
      UniqueMap_lhc
    }
    else if (input$item_type_lhc == "Vial"){
      Vial_lhc
    }
    else if (input$item_type_lhc == "Watchstone"){
      Watchstone_lhc
      
    }
    else if (input$item_type_lhc == "Fragment") {
      Fragment_lhc
    }
  })
  
  #### Main Plot LHC ####
  output$plot_lhc = renderPlotly({
    ggplotly(
      ggplot(data = sub_item_type_lhc()[sub_item_type_lhc()$League %in% input$lhc_selector,],
             aes_string(x = "Day", y = make.names(input$item_name_lhc))) +
        geom_line(aes(colour = League)) +
        geom_area(aes(colour = League), position = "identity", alpha = 0.1) +
        scale_x_continuous(breaks = seq(0, length(sub_item_type_lhc()$Day), by=5))+
        theme(legend.position = "bottom",
              axis.line = element_line(colour = "black",size = 2),
              panel.grid.minor = element_blank(),
              panel.grid.major =  element_line(color = "gray90"),
              panel.background = element_blank(),
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"))
    )
  })
  ########################################################
  
  ########################################################
  #### SSC Server data ####
  ###################### Reactive in item_type_SSC ################
  tipo_do_item_ssc <- reactive({
    filter(subset_names, Type == input$item_type_ssc)})
  
  observeEvent(tipo_do_item_ssc(), {
    choices <- unique(tipo_do_item_ssc()$Name)
    updateSelectInput(session, "item_name_ssc", choices = choices) })
  
  observeEvent(input$item_name_ssc, {
    cat("Item name: ", input$item_name_ssc, "\n") })
  
  #Filter to link item_type_ssc to the axes in the plot.
  sub_item_type_ssc <- eventReactive(input$item_type_ssc,{
    subset(s, Type == input$item_type_ssc)}) 
  sub_item_type_ssc <- eventReactive(input$item_type_ssc,{
    if (input$item_type_ssc == "Beast"){
      Beast_ssc
    }
    else if (input$item_type_ssc == "Currency"){
      Currency_ssc
    }
    else if (input$item_type_ssc == "Incubator"){
      Incubator_ssc
    }
    else if (input$item_type_ssc == "Oil"){
      Oil_ssc
    }
    else if (input$item_type_ssc == "Fossil"){
      Fossil_ssc
    }
    else if (input$item_type_ssc == "Resonator"){
      Resonator_ssc
    }
    else if (input$item_type_ssc == "Scarab"){
      Scarab_ssc
    }
    else if (input$item_type_ssc == "HelmetEnchant"){
      HelmetEnchant_ssc
    }
    else if (input$item_type_ssc == "Map"){
      Map_ssc
    }
    else if (input$item_type_ssc == "Prophecy"){
      Prophecy_ssc
    }
    else if (input$item_type_ssc == "UniqueJewel"){
      UniqueJewel_ssc
    }
    else if (input$item_type_ssc == "UniqueFlask"){
      UniqueFlask_ssc
    }
    else if (input$item_type_ssc == "Essence"){
      Essence
    }
    else if (input$item_type_ssc == "DivinationCard"){
      DivinationCard_ssc
    }
    else if (input$item_type_ssc == "UniqueAccessory"){
      UniqueAccessory_ssc
    }
    else if (input$item_type_ssc == "UniqueArmour"){
      UniqueArmour_ssc
    }
    else if (input$item_type_ssc == "UniqueWeapon"){
      UniqueWeapon_ssc
    }
    else if (input$item_type_ssc == "BaseType"){
      BaseType_ssc
    }
    else if (input$item_type_ssc == "SkillGem"){
      SkillGem_ssc
    }
    else if (input$item_type_ssc == "UniqueMap"){
      UniqueMap_ssc
    }
    else if (input$item_type_ssc == "Vial"){
      Vial_ssc
    }
    else if (input$item_type_ssc == "Watchstone"){
      Watchstone_ssc
      
    }
    else if (input$item_type_ssc == "Fragment") {
      Fragment_ssc
    }
  })
  
  ##### Main Plot SSC ####
  output$plot_ssc = renderPlotly({
    ggplotly(
      ggplot(data = sub_item_type_ssc()[sub_item_type_ssc()$League %in% input$ssc_selector,],
             aes_string(x = "Date", y = make.names(input$item_name_ssc))) +
        geom_line(color = "#F0736A") +
        geom_area(position = "identity", alpha = 0.1) +
        scale_x_date(date_breaks = paste(input$d_interval, "day"), date_labels = "%d/%m/%y") +
        theme(legend.position = "bottom",
              axis.line = element_line(colour = "black",size = 2),
              panel.grid.minor = element_blank(),
              panel.grid.major =  element_line(color = "gray90"),
              panel.background = element_blank(),
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"))
    )
  })
  ##########################################################
  
  # Print League and Current day of League
  output$current_date = renderText({
    paste0("The current league, <b>", current_league_name, "</b>, has passed <b>", Sys.Date() - current_league_start_date, " days.</b>")
  })
  
}