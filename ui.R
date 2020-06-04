library(ggplot2)
library(plotly)
library(plyr)
library(shiny)
library(shinydashboardPlus)
library(shinythemes)


source("helper.R")

fluidPage(
  tags$head(HTML("<title> Path of Trends</title>")),
  navbarPage(strong("League Selector"),
             theme = shinytheme('spacelab'),
             footer = dashboardFooter(
               left_text = a(img(src = "paypal.png", height = 36, width = 115), href = "https://www.paypal.me/potrends"),
               right_text = a(img(src = "patreon.png", height = 36, width = 68), href = "https://www.patreon.com/join/synet08/checkout?rid=5169918")
             ),
             #### Leagues SC Tab ####
             tabPanel("League Softcore",
                      sidebarLayout(
                        sidebarPanel(
                          # Create League check box
                          checkboxGroupInput("lsc_selector", "League",
                                             choices = league_list$League,
                                             selected = "Metamorph"
                          ),
                          selectInput("item_type_lsc", "Item Type", 
                                      choices = c("Base type"="BaseType","Beast"="Beast","Currency" = "Currency","Divination Card"="DivinationCard",
                                                  "Essence"="Essence","Fossil"="Fossil","Fragment" = "Fragment","Helmet Enchant"="HelmetEnchant","Incubator"="Incubator",
                                                  "Map"="Map","Oil"="Oil","Prophecy"="Prophecy","Resonator"="Resonator",
                                                  "Sacarb"="Scarab","Skill Gem"="SkillGem","Unique Accessory"="UniqueAccessory","Unique Armour"="UniqueArmour",
                                                  "Unique Flask"="UniqueFlask","Unique Jewel"="UniqueJewel","Unique Map"="UniqueMap",
                                                  "Unique Weapon "="UniqueWeapon","Vial"="Vial","Watchstone"="Watchstone"), selected = "DivinationCard"
                          ),
                          selectInput("item_name_lsc", "Name", choices = NULL)
                        ),
                        mainPanel(
                          plotlyOutput("plot_lsc"),
                          # textOutput("league"),
                          # textOutput("item_type_print"),
                          # textOutput("item_name_print"),
                          h5(htmlOutput("current_date"), align="center"),
                        ),
                      ),
                      # # WHERE YOUR FOOTER GOES
                      # br(),br(),br(),br(),br(),br(),
                      # hr(),
                      # a(img(src = "patreon.png", height = 36, width = 68), href = "https://www.patreon.com/join/synet08/checkout?rid=5169918"),
                      # a(img(src = "paypal.png", height = 36, width = 115), href = "https://www.paypal.me/potrends"),
             ),
             #### End League SC tab ####
             
             #### League HC tab ####
             tabPanel("League Hardcore",
                      sidebarLayout(
                        sidebarPanel(
                          # Create League check box
                          checkboxGroupInput("lhc_selector", "League",
                                             choices = league_list$LeagueHC,
                                             selected = "Metamorph HC"
                          ),
                          selectInput("item_type_lhc", "Item Type", 
                                      choices = c("Base type"="BaseType","Beast"="Beast","Currency" = "Currency","Divination Card"="DivinationCard",
                                                  "Essence"="Essence","Fossil"="Fossil","Fragment" = "Fragment","Helmet Enchant"="HelmetEnchant","Incubator"="Incubator",
                                                  "Map"="Map","Oil"="Oil","Prophecy"="Prophecy","Resonator"="Resonator",
                                                  "Sacarb"="Scarab","Skill Gem"="SkillGem","Unique Accessory"="UniqueAccessory","Unique Armour"="UniqueArmour",
                                                  "Unique Flask"="UniqueFlask","Unique Jewel"="UniqueJewel","Unique Map"="UniqueMap",
                                                  "Unique Weapon "="UniqueWeapon","Vial"="Vial","Watchstone"="Watchstone"), selected = "DivinationCard"
                          ),
                          selectInput("item_name_lhc", "Name", choices = NULL)
                        ),
                        mainPanel(
                          plotlyOutput("plot_lhc"),
                          textOutput("league_lhc"),
                          textOutput("item_type_print_lhc"),
                          textOutput("item_name_print_lhc"),
                          h5(htmlOutput("current_date_lhc"), align="center"),
                        ),
                      )
             ),
             #### End League HC tab ####
             
             #### Standard SC tab ####
             tabPanel("Standard Softcore",
                      sidebarLayout(
                        sidebarPanel(
                          # Create League check box
                          radioButtons("ssc_selector", "League",
                                       choices = "Standard",
                                       selected = "Standard"
                          ),
                          selectInput("item_type_ssc", "Item Type", 
                                      choices = c("Base type"="BaseType","Beast"="Beast","Currency" = "Currency","Divination Card"="DivinationCard",
                                                  "Essence"="Essence","Fossil"="Fossil","Fragment" = "Fragment","Helmet Enchant"="HelmetEnchant","Incubator"="Incubator",
                                                  "Map"="Map","Oil"="Oil","Prophecy"="Prophecy","Resonator"="Resonator",
                                                  "Sacarb"="Scarab","Skill Gem"="SkillGem","Unique Accessory"="UniqueAccessory","Unique Armour"="UniqueArmour",
                                                  "Unique Flask"="UniqueFlask","Unique Jewel"="UniqueJewel","Unique Map"="UniqueMap",
                                                  "Unique Weapon "="UniqueWeapon","Vial"="Vial","Watchstone"="Watchstone"), selected = "DivinationCard"
                          ),
                          selectInput("item_name_ssc", "Name", choices = NULL),
                          
                          sliderInput("d_interval",
                                      "Interval of Days (Legend X-axis)",
                                      min = 1,
                                      max = 120,
                                      value = 75)
                        ),
                        mainPanel(
                          plotlyOutput("plot_ssc"),
                          textOutput("league_ssc"),
                          textOutput("item_type_print_scc"),
                          textOutput("item_name_print_scc"),
                        ),
                      )
             ),
             #### End Standard SC tab ####
             
             navbarMenu("More",
                        tabPanel("Contact",
                                 h4("Feel free to contact me if you have any suggestion/question or whatever."),
                                 p(a("Discord: Synet#3944", href = "https://poe.ninja/")),
                                 p(a("Reddit topic", href = "https://poe.ninja/")),
                                 p(a("Forum thread", href = "https://www.pathofexile.com/forum"))
                        ),
                        tabPanel("About",
                                 h2(strong("About")),
                                 h4("Path of Trends is a tool that shows long-term price of the items in Path of Exile."),
                                 
                                 h2(strong("Acknowledgment")),
                                 h4("I would like to say a big Thank you to",
                                    a("Poe.ninja", href = "https://poe.ninja/"),
                                    "creator to process and share all the data. Without your job it would not be possible to create this tool."),
                                 p(strong(h3(("Thank You!"))), style = "text-align: center;"),
                                 
                                 br(),
                                 h6("Version: 1.0b")
                        )
             )
  )
)