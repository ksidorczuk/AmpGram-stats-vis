library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
    
    titlePanel("AmpGram results"),
    h5("Shiny app for visualisation of statistics for the second layer of prediction. Data contains prediction
        results from a random forest trained on (19,26] length group. You can use three different plots for 
        visualisation, each is located in a seperate tab of a main panel below. You can find the description of 
        all calculated statistics in the panel on the left side and a diagram in the Description tab."),
    sidebarLayout(
        sidebarPanel(
            h5(strong("Statistics calculated for each peptide:")),
            h6(
                tags$div(
                    tags$ul(
                        tags$li(tags$b("fraction_true"), "- fraction of positive mers"),
                        tags$li(tags$b("pred_mean"), "- mean value of prediction"),
                        tags$li(tags$b("pred_median"), "- median of prediction"),
                        tags$li(tags$b("n_peptide"), "- number of mers"),
                        tags$li(tags$b("n_pos"), "- number of positive mers"),
                        tags$li(tags$b("pred_min"), "- minimum value of prediction"),
                        tags$li(tags$b("pred_max"), "- maximum value of prediction"),
                        tags$li(tags$b("longest_pos"), "- the longest stretch of mers occuring 
                            one by one and predicted as positive"),
                        tags$li(tags$b("n_pos_10"), "- number of streches comprising of at least 
                    10 mers predicted as positive"),
                        tags$li(tags$b("frac_0_0.2"), "- fraction of mers with prediction in range [0,0.2]"),
                        tags$li(tags$b("frac_0.2_0.4"), "- fraction of mers with prediction in range (0.2,0.4]"),
                        tags$li(tags$b("frac_0.4_0.6"), "- fraction of mers with prediction in range (0.4,0.6]"),
                        tags$li(tags$b("frac_0.6_0.8"), "- fraction of mers with prediction in range (0.6,0.8]"),
                        tags$li(tags$b("frac_0.8_1"), "- fraction of mers with prediction in range (0.8,1]"))
                )
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Description",
                         h5("Diagram showing how we get statistics for each peptide."),
                         #img(src='fig1.png', align = "right", width = '100%'),
                         img(src='stats.png', align = "right", width = '100%')),
                tabPanel("Violin plot",
                         h5("View the distribution of statistics and its probability density. 
                            You can select different statistics and set custom length groups."),
                         selectInput("y_violin", "Select data for y axis:",
                                     choices = colnames(data)[5:18], selected = "fraction_true",
                                     width = '150px'),
                         h5(strong("Select length ranges for x axis:")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             numericInput("gr1", label = NULL,
                                          min = 1, max = 710, value = 1, width = '80px')),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             numericInput("gr2", label = NULL,
                                          min = 1, max = 710, value = 50, width = '80px')),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             numericInput("gr3", label = NULL,
                                          min = 1, max = 710, value = 100, width = '80px')),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             numericInput("gr4", label = NULL,
                                          min = 1, max = 710, value = 200, width = '80px')),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             numericInput("gr5", label = NULL,
                                          min = 1, max = 710, value = 710, width = '80px')),
                         plotOutput("violin_plot")
                ),
                tabPanel("2d density plot",
                         h5("View the 2d distribution of statistics density. The default setting 
                         for x axis is number of mers in a protein. You can select different statistics 
                         for both y and x axises."),
                         div(style="display: inline-block;vertical-align:top; width: 200px;",
                             selectInput("y_density", "Select data for y axis:",
                                         choices = colnames(data)[5:18], selected = "fraction_true",
                                         width = '150px')),
                         div(style="display: inline-block;vertical-align:top; width: 200px;",
                             selectInput("x_density", "Select data for x axis:",
                                         choices = colnames(data)[5:18], selected = "n_peptide",
                                         width = '150px')),
                         plotOutput("density_plot")
                ),
                tabPanel("Bar plot",
                         h5("View the ranges of a given statistic in the proportion of all peptides. The upper
                         and lower parts of the plot represent peptides from the negative and positive datasets, 
                         respectively. The value ranges (represented by colors) are calculated for each statistic 
                         that can be selected below. You can change limits of the x axis to see other length ranges 
                         (max length of a peptide is 710)."),
                         selectInput("bar_plot", "Select data:",
                                     choices = colnames(data[c(5:7,10:11,14:18)]), selected = "fraction_true",
                                     width = '150px'),
                         h5(strong("Select length ranges for x axis:")),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             numericInput("start", label = NULL,
                                          min = 1, max = 700, value = 1, width = '80px')),
                         div(style="display: inline-block;vertical-align:top; width: 100px;",
                             numericInput("end", label = NULL,
                                          min = 10, max = 710, value = 200, width = '80px')),
                         plotOutput("col_plot"))
            )
        )
        
    )
)

server <- function(input, output) {
    
    data <- read.csv("layer_data.csv")
    
    len_groups <- reactive({
        c(input[["gr1"]], input[["gr2"]], input[["gr3"]], input[["gr4"]], input[["gr5"]])
    })
    
    len_limits <- reactive({
        c(input[["start"]], input[["end"]])
    })
    
    break_vals <- reactive({
        case_when(
            input[["bar_plot"]] %in% colnames(data)[c(5:7, 10:11, 14:18)] ~ 0L:5/5,
            input[["bar_plot"]] == "n_pos" ~ c(0,1,10,20,50,187),
            input[["bar_plot"]] == "longest_pos" ~ c(0,1,5,15,30,85),
            input[["bar_plot"]] == "n_pos_10" ~ c(0,0.5,1,3,5,7)
        )
    })
    
    data_v <- reactive({
        data %>% mutate(len = (n_peptide + 9),
                   len_group = cut(len, breaks = len_groups(), include.lowest = TRUE))
    })
    
    output[["violin_plot"]] <- renderPlot({
        ggplot(na.omit(data_v()), aes_string(x = "len_group", y = input[["y_violin"]], fill = "target")) +
            geom_violin()
    })
    
    output[["density_plot"]] <- renderPlot({
        ggplot(data, aes_string(x = input[["x_density"]], y = input[["y_density"]], 
                                fill = "target", color = "target")) +
            geom_point(aes(alpha = 0.01), position = "jitter") +
            stat_density2d(aes(alpha = ..level..), geom = "polygon", color = "black", size = 0.4) 
    })
    
    
    output[["col_plot"]] <- renderPlot({
        data %>% 
            mutate(value_ranges = cut(get(input[["bar_plot"]]), breaks = break_vals(), 
                             include.lowest = TRUE),
                   len = n_peptide + 9) %>% 
            group_by(target, len, value_ranges) %>% 
            summarise(n = length(get(input[["bar_plot"]]))) %>% 
            group_by(target, len) %>% 
            mutate(n_prop = n/sum(n)) %>% 
            ggplot(aes_string(x = "len", y = "n_prop", fill = "value_ranges")) +
            geom_col() +
            facet_wrap(~ target, ncol = 1) +
            theme_bw() +
            scale_x_continuous(limits = len_limits()) +
            labs(y ="proportion of peptides", x = "lenght of peptides")
        
    })
}

shinyApp(ui = ui, server = server)
