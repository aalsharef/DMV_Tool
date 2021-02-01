library(shiny)
library(shinydashboard)
library(tidyverse)
library(DescTools)
#############################################
all_1 <- read_csv("count_sheet1.csv")
all_1 <- all_1 %>% 
  select(-claim_n)
nam_cols <- colnames(all_1)
vec_1 <- as.vector(nam_cols)
df1<-  as.data.frame(nam_cols)

all_1 %>% 
  select(type) %>% 
  distinct()

#########################################


header <- dashboardHeader(title = "Injury Prediction Tool")


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overall Probability", tabName="dash_1", icon = icon("percent")),
    menuItem("Condit. Prob. (One Variable)", tabName="dash_2", icon = icon("binoculars")),
    menuItem("Condit. Prob. (Two Variables)", tabName="dash_3", icon = icon("network-wired"))))

body <- dashboardBody(
  tabItems(
    # last tab content
    tabItem(tabName = "dash_3",
            fluidRow(
              box (
                selectizeInput(inputId = "dd_1",
                               label = "Predicted Variable",
                               choices = colnames(all_1),
                               selected = "",
                               multiple = FALSE)),
              box (
                selectizeInput(inputId = "dd_2",
                               label = "Given Variable 1",
                               choices = vec_1,
                               selected = "",
                               multiple = FALSE)),
              box(
                selectizeInput(inputId = "dd_3",
                               label = "Given Variable 2",
                               choices = vec_1,
                               selected = "",
                               multiple = FALSE)),
              box (
                selectizeInput(inputId = "dd_4",
                               label = "Attribute for Given Variable 1",
                               choices = colnames(all_1),
                               selected = "",
                               multiple = FALSE)),
              box (
                selectizeInput(inputId = "dd_5",
                               label = "Attribute for Given Variable 2",
                               choices = colnames(all_1),
                               selected = "",
                               multiple = FALSE))),
            
            fluidRow(
              box(plotOutput("plot_1")),
              box(tableOutput("table_1"))),
            fluidRow(
              box(plotOutput("error_plt_tab3")),
              box(tableOutput("error_table_tab3")))),
    # Secons tab content
    tabItem(tabName = "dash_2",
            fluidRow(
              box (width = 4,
                   selectizeInput(inputId = "dd_6",
                                  label = "Predicted Variable",
                                  choices = colnames(all_1),
                                  selected = "type",
                                  multiple = FALSE)),
              box (width = 4,
                   selectizeInput(inputId = "dd_7",
                                  label = "Given Variable",
                                  choices = vec_1,
                                  selected = "",
                                  multiple = FALSE)),
              box(width = 4,
                  selectizeInput(inputId = "dd_8",
                                 label = "Attribute for Given Variable",
                                 choices = vec_1,
                                 selected = "",
                                 multiple = FALSE))),
            
            fluidRow(
              box(plotOutput("plot_2")),
              box(tableOutput("table_2"))),
            fluidRow(
              box(plotOutput("error_plt_tab2")),
              box(tableOutput("error_table_tab2")))),
    
    tabItem(tabName = "dash_1",
            fluidRow(
              box (
                selectizeInput(inputId = "dd_9",
                               label = "Predicted Variable",
                               choices = colnames(all_1),
                               selected = "type",
                               multiple = FALSE))),
            fluidRow(
              box(plotOutput("plot_3")),
              box(tableOutput("table_3"))),
            
            fluidRow(
              box(plotOutput("error_plt_tab1")),
              box(tableOutput("error_table_tab1"))))))

ui <- dashboardPage(header, sidebar, body, skin = "red")        
######################################### Server Section

server <- function(input, output, session) { 
  
  observeEvent(input$dd_1, {
    d <- df1 %>%
      filter(nam_cols != input$dd_1)
    updateSelectizeInput(session, "dd_2", choices = unique(d$nam_cols)) 
  })
  
  observeEvent(input$dd_2, {
    d <- df1 %>%
      filter(nam_cols != input$dd_1) %>% 
      filter(nam_cols != input$dd_2)
    updateSelectizeInput(session, "dd_3", choices = unique(d$nam_cols)) 
  })
  
  observeEvent(input$dd_2, {
    d <- all_1 %>%
      select(input$dd_2) %>%
      distinct()
    updateSelectizeInput(session, "dd_4", choices = d)
  })
  
  observeEvent(input$dd_3, {
    d <- all_1 %>%
      select(input$dd_3) %>%
      distinct()
    updateSelectizeInput(session, "dd_5", choices = d)
  })
  
  ##########################################
  # second tab
  
  observeEvent(input$dd_6, {
    d <- df1 %>%
      filter(nam_cols != input$dd_6)
    updateSelectizeInput(session, "dd_7", choices = unique(d$nam_cols)) 
  })
  
  observeEvent(input$dd_7, {
    d <- all_1 %>%
      select(input$dd_7) %>%
      distinct()
    updateSelectizeInput(session, "dd_8", choices = d)
  })
  
  ##########################################
  # Plot and table
  
  output$table_1 <- renderTable({
    
    my_fun <- function (var1, var2, var3, subvar1, subvar2) {
      
      vars <- c(var1, var2, var3)
      
      vars_2 <- c(subvar1, subvar2)
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]], .data[[vars[[3]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]& 
                 .data[[vars[[3]]]] == vars_2[2]) %>% 
        count(.data[[vars[[1]]]], sort = T) %>% 
        mutate(prop = n * 100/ sum(n)) %>% 
        mutate(prop = round(prop,2)) %>% 
        select(-n)
      # -----------------------------------
      # get the district value add to zero
      
      distic_1 <- all_2 %>% 
        distinct(.data[[vars[[1]]]])
      
      # -----------------------------------
      # left join
      
      join <- left_join(distic_1, all_3) %>%
        mutate(prop = if_else(is.na(prop) == T, 0, prop)) %>% 
        arrange(desc(prop))
      
      return(join)}
    my_fun (input$dd_1,input$dd_2,input$dd_3,input$dd_4,input$dd_5)
    
    
  })    
  
  output$plot_1 <- renderPlot({
    
    my_fun <- function (var1, var2, var3, subvar1, subvar2) {
      
      vars <- c(var1, var2, var3)
      
      vars_2 <- c(subvar1, subvar2)
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]], .data[[vars[[3]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]& 
                 .data[[vars[[3]]]] == vars_2[2]) %>% 
        count(.data[[vars[[1]]]], sort = T) %>% 
        mutate(prop = n * 100/ sum(n)) %>% 
        mutate(prop = round(prop,2)) %>% 
        select(-n)
      # -----------------------------------
      # get the district value add to zero
      
      distic_1 <- all_2 %>% 
        distinct(.data[[vars[[1]]]])
      
      # -----------------------------------
      # left join
      
      join <- left_join(distic_1, all_3) %>%
        mutate(prop = if_else(is.na(prop) == T, 0, prop)) %>% 
        arrange(desc(prop))
      
      a <- as.matrix(join[1])
      b <- as.matrix(join[2])
      a<- as.vector(a)
      b<- as.vector(b)
      nam <- colnames(join)
      # ----
      # data viz
      
      plt <- ggplot(join, aes(x = reorder(.data[[nam[[1]]]],.data[[nam[[2]]]]), y = .data[[nam[[2]]]])) +
        geom_col(fill = "dodgerblue4") +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() +
        geom_text(aes(label = prop), size = 5, position = position_dodge(0.9), hjust = 0) +
        xlab("") + ylab("Probability (%)")+
        theme_classic(15) 
      
      
      return(plt)}
    my_fun (input$dd_1,input$dd_2,input$dd_3,input$dd_4,input$dd_5)
    
    
    
    
    
  })
  
  ###################################################################
  ##### plot for tab2 - one predicted variable
  output$table_2 <- renderTable({
    my_fun <- function (var1, var2, subvar1) {
      
      vars <- c(var1, var2)
      
      vars_2 <- c(subvar1)
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]) %>% 
        count(.data[[vars[[1]]]], sort = T) %>% 
        mutate(prop = n * 100/ sum(n)) %>% 
        mutate(prop = round(prop,2)) %>% 
        select(-n)
      # -----------------------------------
      # get the district value add to zero
      
      distic_1 <- all_2 %>% 
        distinct(.data[[vars[[1]]]])
      
      # -----------------------------------
      # left join
      
      join <- left_join(distic_1, all_3) %>%
        mutate(prop = if_else(is.na(prop) == T, 0, prop)) %>% 
        arrange(desc(prop))
      
      a <- as.matrix(join[1])
      b <- as.matrix(join[2])
      a<- as.vector(a)
      b<- as.vector(b)
      nam <- colnames(join)
      
      
      return(join)}
    my_fun (input$dd_6,input$dd_7,input$dd_8)
    
    
  })
  
  
  output$plot_2 <- renderPlot ({
    my_fun <- function (var1, var2, subvar1) {
      
      vars <- c(var1, var2)
      
      vars_2 <- c(subvar1)
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]) %>% 
        count(.data[[vars[[1]]]], sort = T) %>% 
        mutate(prop = n * 100/ sum(n)) %>% 
        mutate(prop = round(prop,2)) %>% 
        select(-n)
      # -----------------------------------
      # get the district value add to zero
      
      distic_1 <- all_2 %>% 
        distinct(.data[[vars[[1]]]])
      
      # -----------------------------------
      # left join
      
      join <- left_join(distic_1, all_3) %>%
        mutate(prop = if_else(is.na(prop) == T, 0, prop)) %>% 
        arrange(desc(prop))
      
      a <- as.matrix(join[1])
      b <- as.matrix(join[2])
      a<- as.vector(a)
      b<- as.vector(b)
      nam <- colnames(join)
      # ----
      # data viz
      
      plt <- ggplot(join, aes(x = reorder(.data[[nam[[1]]]],.data[[nam[[2]]]]), y = .data[[nam[[2]]]])) +
        geom_col(fill = "dodgerblue4") +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() +
        geom_text(aes(label = prop), size = 5, position = position_dodge(0.9), hjust = 0) +
        xlab("") + ylab("Probability (%)")+
        theme_classic(15) 
      
      return(plt)}
    my_fun (input$dd_6,input$dd_7,input$dd_8)
    
    
  })
  ###################################################################
  ##### plot for tab1 - one predicted variable
  output$table_3 <- renderTable({
    my_fun <- function (var1) {
      
      vars <- c(var1)
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]]) %>% 
        count(.data[[vars[[1]]]], sort = T) %>% 
        mutate(prop = n * 100/ sum(n)) %>% 
        mutate(prop = round(prop,2)) %>% 
        select(-n)
      # -----------------------------------
      # get the district value add to zero
      
      distic_1 <- all_2 %>% 
        distinct(.data[[vars[[1]]]])
      
      # -----------------------------------
      # left join
      
      join <- left_join(distic_1, all_3) %>%
        mutate(prop = if_else(is.na(prop) == T, 0, prop)) %>% 
        arrange(desc(prop))
      
      a <- as.matrix(join[1])
      b <- as.matrix(join[2])
      a<- as.vector(a)
      b<- as.vector(b)
      nam <- colnames(join)
      # ----
      
      return(join)}
    my_fun (input$dd_9)
    
    
  })
  
  
  output$plot_3 <- renderPlot ({
    my_fun <- function (var1) {
      
      vars <- c(var1)
      
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]]) %>% 
        count(.data[[vars[[1]]]], sort = T) %>% 
        mutate(prop = n * 100/ sum(n)) %>% 
        mutate(prop = round(prop,2)) %>% 
        select(-n)
      # -----------------------------------
      # get the district value add to zero
      
      distic_1 <- all_2 %>% 
        distinct(.data[[vars[[1]]]])
      
      # -----------------------------------
      # left join
      
      join <- left_join(distic_1, all_3) %>%
        mutate(prop = if_else(is.na(prop) == T, 0, prop)) %>% 
        arrange(desc(prop))
      
      a <- as.matrix(join[1])
      b <- as.matrix(join[2])
      a<- as.vector(a)
      b<- as.vector(b)
      nam <- colnames(join)
      # ----
      # data viz
      
      plt <- ggplot(join, aes(x = reorder(.data[[nam[[1]]]],.data[[nam[[2]]]]), y = .data[[nam[[2]]]])) +
        geom_col(fill = "dodgerblue4") +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() +
        geom_text(aes(label = prop), size = 5, position = position_dodge(0.9), hjust = 0) +
        xlab("") + ylab("Probability (%)")+
        theme_classic(15) 
      
      
      return(plt)}
    my_fun (input$dd_9)
    
    
  })
  
  # Erro Bar Plot output for the fist tab
  
  output$error_plt_tab1 <- renderPlot ({
    my_fun <- function (var1) {
      
      vars <- c(var1)
      
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]])
      
      final <- MultinomCI(table(all_3[1]), method = "goodman")
      
      final_2 <- as.data.frame(final)
      
      dat_1<- rownames_to_column(final_2, var = "Attributes")
      
      dat_2 <- dat_1 %>% 
        mutate(est = est * 100, lwr.ci = lwr.ci *100, upr.ci = upr.ci *100) %>% 
        arrange(desc(est))
      # plot
      plt <- ggplot (dat_2, aes(x = Attributes, y = est)) +
        geom_point(size = 3, color ="dodgerblue4") +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() + 
        geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.3) +
        labs(x = "", y = "Probability (%)") +
        theme_classic(15)
      
      return(plt)}
    my_fun (input$dd_9)
  })
  
  output$error_table_tab1 <- renderTable ({
    my_fun <- function (var1) {
      
      vars <- c(var1)
      
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]])
      
      final <- MultinomCI(table(all_3[1]), method = "goodman")
      
      final_2 <- as.data.frame(final)
      
      dat_1<- rownames_to_column(final_2, var = colnames(all_3[1]))
      
      dat_2 <- dat_1 %>% 
        mutate(est = est * 100, lwr.ci = lwr.ci *100, upr.ci = upr.ci *100) %>% 
        arrange(desc(est)) %>% 
        rename(prop = est, "Lower Confidence Limit" = lwr.ci, "Upper Confidence Limit" = upr.ci)
      
      return(dat_2)}
    
    my_fun (input$dd_9)
  })
  
  # The error bar plot and tables for the second tab
  
  output$error_plt_tab2 <- renderPlot ({
    my_fun <- function (var1, var2, subvar1) {
      
      vars <- c(var1, var2)
      vars_2 <- c(subvar1)
      
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]) %>% 
        select(.data[[vars[[1]]]])
      
      
      final <- MultinomCI(table(all_3[1]), method = "goodman")
      
      final_2 <- as.data.frame(final)
      
      dat_1<- rownames_to_column(final_2, var = "Attributes")
      
      dat_2 <- dat_1 %>% 
        mutate(est = est * 100, lwr.ci = lwr.ci *100, upr.ci = upr.ci *100) %>% 
        arrange(desc(est))
      # plot
      plt <- ggplot (dat_2, aes(x = Attributes, y = est)) +
        geom_point(size = 3, color ="dodgerblue4") +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() + 
        geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.3) +
        labs(x = "", y = "Probability (%)") +
        theme_classic(15)
      
      return(plt)}
    my_fun (input$dd_6, input$dd_7, input$dd_8)
  })
  
  output$error_table_tab2 <- renderTable ({
    my_fun <- function (var1, var2, subvar1) {
      
      vars <- c(var1, var2)
      vars_2 <- c(subvar1)
      
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]) %>% 
        select(.data[[vars[[1]]]])
      
      
      final <- MultinomCI(table(all_3[1]), method = "goodman")
      
      final_2 <- as.data.frame(final)
      
      dat_1<- rownames_to_column(final_2, var = colnames(all_3[1]))
      
      dat_2 <- dat_1 %>% 
        mutate(est = est * 100, lwr.ci = lwr.ci *100, upr.ci = upr.ci *100) %>% 
        arrange(desc(est)) %>% 
        rename(prop = est, "Lower Confidence Limit" = lwr.ci, "Upper Confidence Limit" = upr.ci)
      
      return(dat_2)}
    
    my_fun (input$dd_6, input$dd_7, input$dd_8)
  })          
  ###################################################################################
  # The Error and table error for the third tab
  output$error_plt_tab3 <- renderPlot ({
    my_fun <- function (var1, var2, var3, subvar1, subvar2) {
      
      vars <- c(var1, var2, var3)
      
      vars_2 <- c(subvar1, subvar2)
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]], .data[[vars[[3]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]& 
                 .data[[vars[[3]]]] == vars_2[2]) %>%
        select(.data[[vars[[1]]]])
      
      
      final <- MultinomCI(table(all_3[1]), method = "goodman")
      
      final_2 <- as.data.frame(final)
      
      dat_1<- rownames_to_column(final_2, var = "Attributes")
      
      dat_2 <- dat_1 %>% 
        mutate(est = est * 100, lwr.ci = lwr.ci *100, upr.ci = upr.ci *100) %>% 
        arrange(desc(est))
      # plot
      plt <- ggplot (dat_2, aes(x = Attributes, y = est)) +
        geom_point(size = 3, color ="dodgerblue4") +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() + 
        geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.3) +
        labs(x = "", y = "Probability (%)") +
        theme_classic(15)
      
      return(plt)}
    my_fun (input$dd_1,input$dd_2,input$dd_3,input$dd_4,input$dd_5)
  })
  
  output$error_table_tab3 <- renderTable ({
    my_fun <- function (var1, var2, var3, subvar1, subvar2) {
      
      vars <- c(var1, var2, var3)
      
      vars_2 <- c(subvar1, subvar2)
      
      all_2 <- all_1
      all_3 <- all_2 %>%
        select(.data[[vars[[1]]]], .data[[vars[[2]]]], .data[[vars[[3]]]]) %>% 
        filter(.data[[vars[[2]]]] == vars_2[1]& 
                 .data[[vars[[3]]]] == vars_2[2]) %>%
        select(.data[[vars[[1]]]])
      
      
      final <- MultinomCI(table(all_3[1]), method = "goodman")
      
      final_2 <- as.data.frame(final)
      
      dat_1<- rownames_to_column(final_2, var = colnames(all_3[1]))
      
      dat_2 <- dat_1 %>% 
        mutate(est = est * 100, lwr.ci = lwr.ci *100, upr.ci = upr.ci *100) %>% 
        arrange(desc(est)) %>% 
        rename(prop = est, "Lower Confidence Limit" = lwr.ci, "Upper Confidence Limit" = upr.ci)
      
      return(dat_2)}
    
    my_fun (input$dd_1,input$dd_2,input$dd_3,input$dd_4,input$dd_5)
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)
