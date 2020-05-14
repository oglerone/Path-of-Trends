library(shiny)
library(ggplot2)
#library(dplyr)
library(plotly)
#library(hrbrthemes)
library(shinythemes)



# Manipulating the data

  # Importing base Currency Database
    poedbLeagueSC   = read.csv("data/leagueSCdb.csv", header = T, sep = ";")
    poedbLeagueHC   = read.csv("data/leagueHCdb.csv", header = T, sep = ";")
    poedbStandardSC = read.csv("data/leagueSCdb.csv", header = T, sep = ";")
    poedbStandardHC = read.csv("data/leagueHCdb.csv", header = T, sep = ";")
    
    #Arranging Date Columns  
    poedbLeagueSC$Date = as.Date(poedbLeagueSC$Date, format = "%d/%m/%Y")
    poedbLeagueHC$Date = as.Date(poedbLeagueHC$Date, format = "%d/%m/%Y")
    poedbStandardSC$Date = as.Date(poedbStandardSC$Date, format = "%d/%m/%Y")
    poedbStandardHC$Date = as.Date(poedbStandardHC$Date, format = "%d/%m/%Y")
    
    # Transforming all NA to 0
    poedbLeagueSC[is.na(poedbLeagueSC)] <- 0
    poedbLeagueHC[is.na(poedbLeagueHC)] <- 0
    poedbStandardSC[is.na(poedbStandardSC)] <- 0
    poedbStandardHC[is.na(poedbStandardHC)] <- 0
  
  
  # Define UI ----
ui <- 
  navbarPage(strong("League Selector"),
    theme = shinytheme('spacelab'),
    
    #### Leagues SC Tab ####
    tabPanel("Leagues Softcore",
    #titlePanel("Title"),
      sidebarLayout(
        sidebarPanel(
          #h3("Price Trend"),
          # Create League check box
          checkboxGroupInput(inputId = "league_sc_selector",
            label = "League",
            choices  = unique(poedbLeagueSC$League),
            selected = "Blight"
          ),
          
          #Create Currency selector
          selectInput(inputId = 'currency_sc', 
            label = 'Currency', 
            choices = c("Ancient Orb"="Ancient.Orb",  "Ancient Reliquary Key"="Ancient.Reliquary.Key",	"Annulment Shard"="Annulment.Shard",  "Apprentice Cartographer's Seal"="Apprentice.Cartographer.s.Seal",  "Apprentice Cartographer's Sextant"="Apprentice.Cartographer.s.Sextant",	"Armourer's Scrap"="Armourer.s.Scrap",	"Blacksmith's Whetstone"="Blacksmith.s.Whetstone",	"Blessed Orb"="Blessed.Orb",	"Blessing of Chayula"="Blessing.of.Chayula",	"Blessing of Esh"="Blessing.of.Esh",	"Blessing of Tul"="Blessing.of.Tul",	"Blessing of Uul-Netol"="Blessing.of.Uul.Netol",	"Blessing of Xoph"="Blessing.of.Xoph",	"Cartographer's Chisel"="Cartographer.s.Chisel",	"Chayula's Breachstone"="Chayula.s.Breachstone",	"Chayula's Charged Breachstone"="Chayula.s.Charged.Breachstone",	"Chayula's Enriched Breachstone"="Chayula.s.Enriched.Breachstone",	"Chayula's Pure Breachstone"="Chayula.s.Pure.Breachstone",	"Chromatic Orb"="Chromatic.Orb",	"Divine Orb"="Divine.Orb",	"Divine Vessel"="Divine.Vessel",	"Eber's Key"="Eber.s.Key",	"Engineer's Orb"="Engineer.s.Orb",	"Esh's Breachstone"="Esh.s.Breachstone",	"Esh's Charged Breachstone"="Esh.s.Charged.Breachstone",	"Esh's Enriched Breachstone"="Esh.s.Enriched.Breachstone",	"Esh's Pure Breachstone"="Esh.s.Pure.Breachstone",
              "Eternal Orb"="Eternal.Orb",	"Exalted Orb"="Exalted.Orb",	"Exalted Shard"="Exalted.Shard",	"Fragment of the Chimera"="Fragment.of.the.Chimera",	"Fragment of the Hydra"="Fragment.of.the.Hydra",	"Fragment of the Minotaur"="Fragment.of.the.Minotaur",	"Fragment of the Phoenix"="Fragment.of.the.Phoenix",	"Gemcutter's Prism"="Gemcutter.s.Prism",	"Glassblower's Bauble"="Glassblower.s.Bauble",	"Harbinger's Orb"="Harbinger.s.Orb",	"Inya's Key"="Inya.s.Key",	"Jeweller's Orb"="Jeweller.s.Orb",	"Journeyman Cartographer's Seal"="Journeyman.Cartographer.s.Seal",	"Journeyman Cartographer's Sextant"="Journeyman.Cartographer.s.Sextant",	"Master Cartographer's Seal"="Master.Cartographer.s.Seal",	"Master Cartographer's Sextant"="Master.Cartographer.s.Sextant",	"Mirror of Kalandra"="Mirror.of.Kalandra",	"Mirror Shard"="Mirror.Shard",	"Mortal Grief"="Mortal.Grief",	"Mortal Hope"="Mortal.Hope",	"Mortal Ignorance"="Mortal.Ignorance",	"Mortal Rage"="Mortal.Rage",	"Offering to the Goddess"="Offering.to.the.Goddess",	"Orb of Alchemy"="Orb.of.Alchemy",
              "Orb of Alteration"="Orb.of.Alteration",	"Orb of Annulment"="Orb.of.Annulment",	"Orb of Augmentation"="Orb.of.Augmentation",	"Orb of Binding"="Orb.of.Binding",	"Orb of Chance"="Orb.of.Chance",	"Orb of Fusing"="Orb.of.Fusing",	"Orb of Horizons"="Orb.of.Horizons",	"Orb of Regret"="Orb.of.Regret",	"Orb of Scouring"="Orb.of.Scouring",	"Orb of Transmutation"="Orb.of.Transmutation",	"Perandus Coin"="Perandus.Coin",	"Portal Scroll"="Portal.Scroll",	"Regal Orb"="Regal.Orb",	"Sacrifice at Dawn"="Sacrifice.at.Dawn",	"Sacrifice at Dusk"="Sacrifice.at.Dusk",	"Sacrifice at Midnight"="Sacrifice.at.Midnight",	"Sacrifice at Noon"="Sacrifice.at.Noon",	"Scroll of Wisdom"="Scroll.of.Wisdom",	"Silver Coin"="Silver.Coin",	"Splinter of Chayula"="Splinter.of.Chayula",	"Splinter of Esh"="Splinter.of.Esh",	"Splinter of Tul"="Splinter.of.Tul",	"Splinter of Uul-Netol"="Splinter.of.Uul.Netol",	"Splinter of Xoph"="Splinter.of.Xoph",	"Stacked Deck"="Stacked.Deck",	"Timeless Eternal Emblem"="Timeless.Eternal.Emblem",
              "Timeless Eternal Empire Splinter"="Timeless.Eternal.Empire.Splinter",	"Timeless Karui Emblem"="Timeless.Karui.Emblem",	"Timeless Karui Splinter"="Timeless.Karui.Splinter",	"Timeless Maraketh Emblem"="Timeless.Maraketh.Emblem",	"Timeless Maraketh Splinter"="Timeless.Maraketh.Splinter",	"Timeless Templar Emblem"="Timeless.Templar.Emblem",	"Timeless Templar Splinter"="Timeless.Templar.Splinter",	"Timeless Vaal Emblem"="Timeless.Vaal.Emblem",	"Timeless Vaal Splinter"="Timeless.Vaal.Splinter",	"Timeworn Reliquary Key"="Timeworn.Reliquary.Key",	"Tul's Breachstone"="Tul.s.Breachstone",	"Tul's Charged Breachstone"="Tul.s.Charged.Breachstone",	"Tul's Enriched Breachstone"="Tul.s.Enriched.Breachstone",	"Tul's Pure Breachstone"="Tul.s.Pure.Breachstone",	"Uul-Netol's Breachstone"="Uul.Netol.s.Breachstone",	"Uul-Netol's Charged Breachstone"="Uul.Netol.s.Charged.Breachstone",	"Uul-Netol's Enriched Breachstone"="Uul.Netol.s.Enriched.Breachstone",	"Uul-Netol's Pure Breachstone"="Uul.Netol.s.Pure.Breachstone",	"Vaal Orb"="Vaal.Orb",	"Volkuur's Key"="Volkuur.s.Key",	"Xoph's Breachstone"="Xoph.s.Breachstone",	"Xoph's Charged Breachstone"="Xoph.s.Charged.Breachstone",	"Xoph's Enriched Breachstone"="Xoph.s.Enriched.Breachstone",	"Xoph's Pure Breachstone"="Xoph.s.Pure.Breachstone",	"Yriel's Key"="Yriel.s.Key"
            )
          )
        ),
        
        mainPanel(
          h2(strong("Paht of Exile Currency Trend: Softcore League")),
          plotlyOutput("currency_league_SC_plotly"),
          #textOutput("selected_league"),
          #textOutput("selected_currency"),
          #plotOutput("currency_plot")
        )
      )
    ),
    #### End Tab League SC #### 
    
    #### Leagues HC Tab ####
    tabPanel("Leagues Hardcore",
      sidebarLayout(
        sidebarPanel(
          # Create League check box
          checkboxGroupInput(inputId = "league_hc_selector",
            label = "League",
            choices  = unique(poedbLeagueHC$League),
            selected = "Blight"
          ),
          
          #Create Currency selector
          selectInput(inputId = 'currency_hc', 
            label = 'Currency', 
            choices = c("Ancient Orb"="Ancient.Orb",  "Ancient Reliquary Key"="Ancient.Reliquary.Key",	"Annulment Shard"="Annulment.Shard",  "Apprentice Cartographer's Seal"="Apprentice.Cartographer.s.Seal",  "Apprentice Cartographer's Sextant"="Apprentice.Cartographer.s.Sextant",	"Armourer's Scrap"="Armourer.s.Scrap",	"Blacksmith's Whetstone"="Blacksmith.s.Whetstone",	"Blessed Orb"="Blessed.Orb",	"Blessing of Chayula"="Blessing.of.Chayula",	"Blessing of Esh"="Blessing.of.Esh",	"Blessing of Tul"="Blessing.of.Tul",	"Blessing of Uul-Netol"="Blessing.of.Uul.Netol",	"Blessing of Xoph"="Blessing.of.Xoph",	"Cartographer's Chisel"="Cartographer.s.Chisel",	"Chayula's Breachstone"="Chayula.s.Breachstone",	"Chayula's Charged Breachstone"="Chayula.s.Charged.Breachstone",	"Chayula's Enriched Breachstone"="Chayula.s.Enriched.Breachstone",	"Chayula's Pure Breachstone"="Chayula.s.Pure.Breachstone",	"Chromatic Orb"="Chromatic.Orb",	"Divine Orb"="Divine.Orb",	"Divine Vessel"="Divine.Vessel",	"Eber's Key"="Eber.s.Key",	"Engineer's Orb"="Engineer.s.Orb",	"Esh's Breachstone"="Esh.s.Breachstone",	"Esh's Charged Breachstone"="Esh.s.Charged.Breachstone",	"Esh's Enriched Breachstone"="Esh.s.Enriched.Breachstone",	"Esh's Pure Breachstone"="Esh.s.Pure.Breachstone",
              "Eternal Orb"="Eternal.Orb",	"Exalted Orb"="Exalted.Orb",	"Exalted Shard"="Exalted.Shard",	"Fragment of the Chimera"="Fragment.of.the.Chimera",	"Fragment of the Hydra"="Fragment.of.the.Hydra",	"Fragment of the Minotaur"="Fragment.of.the.Minotaur",	"Fragment of the Phoenix"="Fragment.of.the.Phoenix",	"Gemcutter's Prism"="Gemcutter.s.Prism",	"Glassblower's Bauble"="Glassblower.s.Bauble",	"Harbinger's Orb"="Harbinger.s.Orb",	"Inya's Key"="Inya.s.Key",	"Jeweller's Orb"="Jeweller.s.Orb",	"Journeyman Cartographer's Seal"="Journeyman.Cartographer.s.Seal",	"Journeyman Cartographer's Sextant"="Journeyman.Cartographer.s.Sextant",	"Master Cartographer's Seal"="Master.Cartographer.s.Seal",	"Master Cartographer's Sextant"="Master.Cartographer.s.Sextant",	"Mirror of Kalandra"="Mirror.of.Kalandra",	"Mirror Shard"="Mirror.Shard",	"Mortal Grief"="Mortal.Grief",	"Mortal Hope"="Mortal.Hope",	"Mortal Ignorance"="Mortal.Ignorance",	"Mortal Rage"="Mortal.Rage",	"Offering to the Goddess"="Offering.to.the.Goddess",	"Orb of Alchemy"="Orb.of.Alchemy",
              "Orb of Alteration"="Orb.of.Alteration",	"Orb of Annulment"="Orb.of.Annulment",	"Orb of Augmentation"="Orb.of.Augmentation",	"Orb of Binding"="Orb.of.Binding",	"Orb of Chance"="Orb.of.Chance",	"Orb of Fusing"="Orb.of.Fusing",	"Orb of Horizons"="Orb.of.Horizons",	"Orb of Regret"="Orb.of.Regret",	"Orb of Scouring"="Orb.of.Scouring",	"Orb of Transmutation"="Orb.of.Transmutation",	"Perandus Coin"="Perandus.Coin",	"Portal Scroll"="Portal.Scroll",	"Regal Orb"="Regal.Orb",	"Sacrifice at Dawn"="Sacrifice.at.Dawn",	"Sacrifice at Dusk"="Sacrifice.at.Dusk",	"Sacrifice at Midnight"="Sacrifice.at.Midnight",	"Sacrifice at Noon"="Sacrifice.at.Noon",	"Scroll of Wisdom"="Scroll.of.Wisdom",	"Silver Coin"="Silver.Coin",	"Splinter of Chayula"="Splinter.of.Chayula",	"Splinter of Esh"="Splinter.of.Esh",	"Splinter of Tul"="Splinter.of.Tul",	"Splinter of Uul-Netol"="Splinter.of.Uul.Netol",	"Splinter of Xoph"="Splinter.of.Xoph",	"Stacked Deck"="Stacked.Deck",	"Timeless Eternal Emblem"="Timeless.Eternal.Emblem",
              "Timeless Eternal Empire Splinter"="Timeless.Eternal.Empire.Splinter",	"Timeless Karui Emblem"="Timeless.Karui.Emblem",	"Timeless Karui Splinter"="Timeless.Karui.Splinter",	"Timeless Maraketh Emblem"="Timeless.Maraketh.Emblem",	"Timeless Maraketh Splinter"="Timeless.Maraketh.Splinter",	"Timeless Templar Emblem"="Timeless.Templar.Emblem",	"Timeless Templar Splinter"="Timeless.Templar.Splinter",	"Timeless Vaal Emblem"="Timeless.Vaal.Emblem",	"Timeless Vaal Splinter"="Timeless.Vaal.Splinter",	"Timeworn Reliquary Key"="Timeworn.Reliquary.Key",	"Tul's Breachstone"="Tul.s.Breachstone",	"Tul's Charged Breachstone"="Tul.s.Charged.Breachstone",	"Tul's Enriched Breachstone"="Tul.s.Enriched.Breachstone",	"Tul's Pure Breachstone"="Tul.s.Pure.Breachstone",	"Uul-Netol's Breachstone"="Uul.Netol.s.Breachstone",	"Uul-Netol's Charged Breachstone"="Uul.Netol.s.Charged.Breachstone",	"Uul-Netol's Enriched Breachstone"="Uul.Netol.s.Enriched.Breachstone",	"Uul-Netol's Pure Breachstone"="Uul.Netol.s.Pure.Breachstone",	"Vaal Orb"="Vaal.Orb",	"Volkuur's Key"="Volkuur.s.Key",	"Xoph's Breachstone"="Xoph.s.Breachstone",	"Xoph's Charged Breachstone"="Xoph.s.Charged.Breachstone",	"Xoph's Enriched Breachstone"="Xoph.s.Enriched.Breachstone",	"Xoph's Pure Breachstone"="Xoph.s.Pure.Breachstone",	"Yriel's Key"="Yriel.s.Key"
            )
          )
        ),
        mainPanel(          
          h2(strong("Paht of Exile Currency Trend: Hardcore League")),
          plotlyOutput("currency_league_HC_plotly"),
        )
      )
    ),
    #### End Tab League HC #### 
    
    #### Standard SC Tab ####
    tabPanel("Standard Softcore",
      sidebarLayout(
        sidebarPanel(
          # Create League check box
          checkboxGroupInput(inputId = "standard_sc_selector",
            label = "League",
            choices  = unique(poedbStandardSC$League),
            selected = "Blight"
          ),
          
          #Create Currency selector
          selectInput(inputId = 'currency_std_sc', 
            label = 'Currency', 
            choices = c("Ancient Orb"="Ancient.Orb",  "Ancient Reliquary Key"="Ancient.Reliquary.Key",	"Annulment Shard"="Annulment.Shard",  "Apprentice Cartographer's Seal"="Apprentice.Cartographer.s.Seal",  "Apprentice Cartographer's Sextant"="Apprentice.Cartographer.s.Sextant",	"Armourer's Scrap"="Armourer.s.Scrap",	"Blacksmith's Whetstone"="Blacksmith.s.Whetstone",	"Blessed Orb"="Blessed.Orb",	"Blessing of Chayula"="Blessing.of.Chayula",	"Blessing of Esh"="Blessing.of.Esh",	"Blessing of Tul"="Blessing.of.Tul",	"Blessing of Uul-Netol"="Blessing.of.Uul.Netol",	"Blessing of Xoph"="Blessing.of.Xoph",	"Cartographer's Chisel"="Cartographer.s.Chisel",	"Chayula's Breachstone"="Chayula.s.Breachstone",	"Chayula's Charged Breachstone"="Chayula.s.Charged.Breachstone",	"Chayula's Enriched Breachstone"="Chayula.s.Enriched.Breachstone",	"Chayula's Pure Breachstone"="Chayula.s.Pure.Breachstone",	"Chromatic Orb"="Chromatic.Orb",	"Divine Orb"="Divine.Orb",	"Divine Vessel"="Divine.Vessel",	"Eber's Key"="Eber.s.Key",	"Engineer's Orb"="Engineer.s.Orb",	"Esh's Breachstone"="Esh.s.Breachstone",	"Esh's Charged Breachstone"="Esh.s.Charged.Breachstone",	"Esh's Enriched Breachstone"="Esh.s.Enriched.Breachstone",	"Esh's Pure Breachstone"="Esh.s.Pure.Breachstone",
              "Eternal Orb"="Eternal.Orb",	"Exalted Orb"="Exalted.Orb",	"Exalted Shard"="Exalted.Shard",	"Fragment of the Chimera"="Fragment.of.the.Chimera",	"Fragment of the Hydra"="Fragment.of.the.Hydra",	"Fragment of the Minotaur"="Fragment.of.the.Minotaur",	"Fragment of the Phoenix"="Fragment.of.the.Phoenix",	"Gemcutter's Prism"="Gemcutter.s.Prism",	"Glassblower's Bauble"="Glassblower.s.Bauble",	"Harbinger's Orb"="Harbinger.s.Orb",	"Inya's Key"="Inya.s.Key",	"Jeweller's Orb"="Jeweller.s.Orb",	"Journeyman Cartographer's Seal"="Journeyman.Cartographer.s.Seal",	"Journeyman Cartographer's Sextant"="Journeyman.Cartographer.s.Sextant",	"Master Cartographer's Seal"="Master.Cartographer.s.Seal",	"Master Cartographer's Sextant"="Master.Cartographer.s.Sextant",	"Mirror of Kalandra"="Mirror.of.Kalandra",	"Mirror Shard"="Mirror.Shard",	"Mortal Grief"="Mortal.Grief",	"Mortal Hope"="Mortal.Hope",	"Mortal Ignorance"="Mortal.Ignorance",	"Mortal Rage"="Mortal.Rage",	"Offering to the Goddess"="Offering.to.the.Goddess",	"Orb of Alchemy"="Orb.of.Alchemy",
              "Orb of Alteration"="Orb.of.Alteration",	"Orb of Annulment"="Orb.of.Annulment",	"Orb of Augmentation"="Orb.of.Augmentation",	"Orb of Binding"="Orb.of.Binding",	"Orb of Chance"="Orb.of.Chance",	"Orb of Fusing"="Orb.of.Fusing",	"Orb of Horizons"="Orb.of.Horizons",	"Orb of Regret"="Orb.of.Regret",	"Orb of Scouring"="Orb.of.Scouring",	"Orb of Transmutation"="Orb.of.Transmutation",	"Perandus Coin"="Perandus.Coin",	"Portal Scroll"="Portal.Scroll",	"Regal Orb"="Regal.Orb",	"Sacrifice at Dawn"="Sacrifice.at.Dawn",	"Sacrifice at Dusk"="Sacrifice.at.Dusk",	"Sacrifice at Midnight"="Sacrifice.at.Midnight",	"Sacrifice at Noon"="Sacrifice.at.Noon",	"Scroll of Wisdom"="Scroll.of.Wisdom",	"Silver Coin"="Silver.Coin",	"Splinter of Chayula"="Splinter.of.Chayula",	"Splinter of Esh"="Splinter.of.Esh",	"Splinter of Tul"="Splinter.of.Tul",	"Splinter of Uul-Netol"="Splinter.of.Uul.Netol",	"Splinter of Xoph"="Splinter.of.Xoph",	"Stacked Deck"="Stacked.Deck",	"Timeless Eternal Emblem"="Timeless.Eternal.Emblem",
              "Timeless Eternal Empire Splinter"="Timeless.Eternal.Empire.Splinter",	"Timeless Karui Emblem"="Timeless.Karui.Emblem",	"Timeless Karui Splinter"="Timeless.Karui.Splinter",	"Timeless Maraketh Emblem"="Timeless.Maraketh.Emblem",	"Timeless Maraketh Splinter"="Timeless.Maraketh.Splinter",	"Timeless Templar Emblem"="Timeless.Templar.Emblem",	"Timeless Templar Splinter"="Timeless.Templar.Splinter",	"Timeless Vaal Emblem"="Timeless.Vaal.Emblem",	"Timeless Vaal Splinter"="Timeless.Vaal.Splinter",	"Timeworn Reliquary Key"="Timeworn.Reliquary.Key",	"Tul's Breachstone"="Tul.s.Breachstone",	"Tul's Charged Breachstone"="Tul.s.Charged.Breachstone",	"Tul's Enriched Breachstone"="Tul.s.Enriched.Breachstone",	"Tul's Pure Breachstone"="Tul.s.Pure.Breachstone",	"Uul-Netol's Breachstone"="Uul.Netol.s.Breachstone",	"Uul-Netol's Charged Breachstone"="Uul.Netol.s.Charged.Breachstone",	"Uul-Netol's Enriched Breachstone"="Uul.Netol.s.Enriched.Breachstone",	"Uul-Netol's Pure Breachstone"="Uul.Netol.s.Pure.Breachstone",	"Vaal Orb"="Vaal.Orb",	"Volkuur's Key"="Volkuur.s.Key",	"Xoph's Breachstone"="Xoph.s.Breachstone",	"Xoph's Charged Breachstone"="Xoph.s.Charged.Breachstone",	"Xoph's Enriched Breachstone"="Xoph.s.Enriched.Breachstone",	"Xoph's Pure Breachstone"="Xoph.s.Pure.Breachstone",	"Yriel's Key"="Yriel.s.Key"
            )
          )
        ),
        mainPanel(
          h2(strong("Paht of Exile Currency Trend: Softcore Standard")),
          plotlyOutput("currency_standard_SC_plotly"),
        )
      )
    ),
    #### End Standard SC tab ####
    
    #### Standard HC Tab ####
    tabPanel("Standard Hardcore",
      sidebarLayout(
        sidebarPanel(
          # Create League check box
          checkboxGroupInput(inputId = "standard_hc_selector",
            label = "League",
            choices  = unique(poedbStandardHC$League),
            selected = "Blight"
          ),
          
          #Create Currency selector
          selectInput(inputId = 'currency_std_hc', 
            label = 'Currency', 
            choices = c("Ancient Orb"="Ancient.Orb",  "Ancient Reliquary Key"="Ancient.Reliquary.Key",	"Annulment Shard"="Annulment.Shard",  "Apprentice Cartographer's Seal"="Apprentice.Cartographer.s.Seal",  "Apprentice Cartographer's Sextant"="Apprentice.Cartographer.s.Sextant",	"Armourer's Scrap"="Armourer.s.Scrap",	"Blacksmith's Whetstone"="Blacksmith.s.Whetstone",	"Blessed Orb"="Blessed.Orb",	"Blessing of Chayula"="Blessing.of.Chayula",	"Blessing of Esh"="Blessing.of.Esh",	"Blessing of Tul"="Blessing.of.Tul",	"Blessing of Uul-Netol"="Blessing.of.Uul.Netol",	"Blessing of Xoph"="Blessing.of.Xoph",	"Cartographer's Chisel"="Cartographer.s.Chisel",	"Chayula's Breachstone"="Chayula.s.Breachstone",	"Chayula's Charged Breachstone"="Chayula.s.Charged.Breachstone",	"Chayula's Enriched Breachstone"="Chayula.s.Enriched.Breachstone",	"Chayula's Pure Breachstone"="Chayula.s.Pure.Breachstone",	"Chromatic Orb"="Chromatic.Orb",	"Divine Orb"="Divine.Orb",	"Divine Vessel"="Divine.Vessel",	"Eber's Key"="Eber.s.Key",	"Engineer's Orb"="Engineer.s.Orb",	"Esh's Breachstone"="Esh.s.Breachstone",	"Esh's Charged Breachstone"="Esh.s.Charged.Breachstone",	"Esh's Enriched Breachstone"="Esh.s.Enriched.Breachstone",	"Esh's Pure Breachstone"="Esh.s.Pure.Breachstone",
              "Eternal Orb"="Eternal.Orb",	"Exalted Orb"="Exalted.Orb",	"Exalted Shard"="Exalted.Shard",	"Fragment of the Chimera"="Fragment.of.the.Chimera",	"Fragment of the Hydra"="Fragment.of.the.Hydra",	"Fragment of the Minotaur"="Fragment.of.the.Minotaur",	"Fragment of the Phoenix"="Fragment.of.the.Phoenix",	"Gemcutter's Prism"="Gemcutter.s.Prism",	"Glassblower's Bauble"="Glassblower.s.Bauble",	"Harbinger's Orb"="Harbinger.s.Orb",	"Inya's Key"="Inya.s.Key",	"Jeweller's Orb"="Jeweller.s.Orb",	"Journeyman Cartographer's Seal"="Journeyman.Cartographer.s.Seal",	"Journeyman Cartographer's Sextant"="Journeyman.Cartographer.s.Sextant",	"Master Cartographer's Seal"="Master.Cartographer.s.Seal",	"Master Cartographer's Sextant"="Master.Cartographer.s.Sextant",	"Mirror of Kalandra"="Mirror.of.Kalandra",	"Mirror Shard"="Mirror.Shard",	"Mortal Grief"="Mortal.Grief",	"Mortal Hope"="Mortal.Hope",	"Mortal Ignorance"="Mortal.Ignorance",	"Mortal Rage"="Mortal.Rage",	"Offering to the Goddess"="Offering.to.the.Goddess",	"Orb of Alchemy"="Orb.of.Alchemy",
              "Orb of Alteration"="Orb.of.Alteration",	"Orb of Annulment"="Orb.of.Annulment",	"Orb of Augmentation"="Orb.of.Augmentation",	"Orb of Binding"="Orb.of.Binding",	"Orb of Chance"="Orb.of.Chance",	"Orb of Fusing"="Orb.of.Fusing",	"Orb of Horizons"="Orb.of.Horizons",	"Orb of Regret"="Orb.of.Regret",	"Orb of Scouring"="Orb.of.Scouring",	"Orb of Transmutation"="Orb.of.Transmutation",	"Perandus Coin"="Perandus.Coin",	"Portal Scroll"="Portal.Scroll",	"Regal Orb"="Regal.Orb",	"Sacrifice at Dawn"="Sacrifice.at.Dawn",	"Sacrifice at Dusk"="Sacrifice.at.Dusk",	"Sacrifice at Midnight"="Sacrifice.at.Midnight",	"Sacrifice at Noon"="Sacrifice.at.Noon",	"Scroll of Wisdom"="Scroll.of.Wisdom",	"Silver Coin"="Silver.Coin",	"Splinter of Chayula"="Splinter.of.Chayula",	"Splinter of Esh"="Splinter.of.Esh",	"Splinter of Tul"="Splinter.of.Tul",	"Splinter of Uul-Netol"="Splinter.of.Uul.Netol",	"Splinter of Xoph"="Splinter.of.Xoph",	"Stacked Deck"="Stacked.Deck",	"Timeless Eternal Emblem"="Timeless.Eternal.Emblem",
              "Timeless Eternal Empire Splinter"="Timeless.Eternal.Empire.Splinter",	"Timeless Karui Emblem"="Timeless.Karui.Emblem",	"Timeless Karui Splinter"="Timeless.Karui.Splinter",	"Timeless Maraketh Emblem"="Timeless.Maraketh.Emblem",	"Timeless Maraketh Splinter"="Timeless.Maraketh.Splinter",	"Timeless Templar Emblem"="Timeless.Templar.Emblem",	"Timeless Templar Splinter"="Timeless.Templar.Splinter",	"Timeless Vaal Emblem"="Timeless.Vaal.Emblem",	"Timeless Vaal Splinter"="Timeless.Vaal.Splinter",	"Timeworn Reliquary Key"="Timeworn.Reliquary.Key",	"Tul's Breachstone"="Tul.s.Breachstone",	"Tul's Charged Breachstone"="Tul.s.Charged.Breachstone",	"Tul's Enriched Breachstone"="Tul.s.Enriched.Breachstone",	"Tul's Pure Breachstone"="Tul.s.Pure.Breachstone",	"Uul-Netol's Breachstone"="Uul.Netol.s.Breachstone",	"Uul-Netol's Charged Breachstone"="Uul.Netol.s.Charged.Breachstone",	"Uul-Netol's Enriched Breachstone"="Uul.Netol.s.Enriched.Breachstone",	"Uul-Netol's Pure Breachstone"="Uul.Netol.s.Pure.Breachstone",	"Vaal Orb"="Vaal.Orb",	"Volkuur's Key"="Volkuur.s.Key",	"Xoph's Breachstone"="Xoph.s.Breachstone",	"Xoph's Charged Breachstone"="Xoph.s.Charged.Breachstone",	"Xoph's Enriched Breachstone"="Xoph.s.Enriched.Breachstone",	"Xoph's Pure Breachstone"="Xoph.s.Pure.Breachstone",	"Yriel's Key"="Yriel.s.Key"
            )
          )
        ),
        
        mainPanel(
          h2(strong("Paht of Exile Currency Trend: Hardcore Standard")),
          plotlyOutput("currency_standard_HC_plotly"),
          
        )
      )
    )
    #### End Standard HC tab ####
  )


# Define server logic ----
# Server logic ----
server <- function(input, output) {

  #Plot League Softcore
  output$currency_league_SC_plotly <- renderPlotly(
    ggplotly(  
        ggplot(data = poedbLeagueSC[poedbLeagueSC$League %in% input$league_sc_selector,],
          aes_string(x = "Day", y = input$currency_sc)) +
          geom_line(aes(colour = League))+
          geom_area(aes(colour = League), position = "identity", alpha = 0.1)+
          scale_x_continuous(breaks = seq(0, length(poedbLeagueSC$Day), by=5))+
          theme(legend.position = "bottom",
            axis.line = element_line(colour = "black",size = 2),
            panel.grid.minor = element_blank(),
            panel.grid.major =  element_line(color = "gray90"),
            
            panel.background = element_blank(),
            #panel.background = element_rect(fill = "#2B3F50"),
            #plot.background=element_rect(fill = "#2B3F50"),
            
            axis.title.x = element_text(face="bold"),
            axis.title.y = element_text(face="bold"),
          )
      )
  )
  
  # Plot League Hardcore
  output$currency_league_HC_plotly <- renderPlotly(
    ggplotly(  
      ggplot(data = poedbLeagueHC[poedbLeagueHC$League %in% input$league_hc_selector,],
             aes_string(x = "Day", y = input$currency_hc)) +
        geom_line(aes(colour = League))+
        geom_area(aes(colour = League), position = "identity", alpha = 0.1)+
        scale_x_continuous(breaks = seq(0, length(poedbLeagueHC$Day), by=5))+
        theme(legend.position = "bottom",
              axis.line = element_line(colour = "black",size = 2),
              panel.grid.minor = element_blank(),
              panel.grid.major =  element_line(color = "gray90"),
              
              panel.background = element_blank(),
              #panel.background = element_rect(fill = "#2B3F50"),
              #plot.background=element_rect(fill = "#2B3F50"),
              
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"),
        )
    )
  )
  
  # Plot Standard Softcore
  output$currency_standard_SC_plotly <- renderPlotly(
    ggplotly(  
      ggplot(data = poedbStandardSC[poedbStandardSC$League %in% input$standard_sc_selector,],
             aes_string(x = "Day", y = input$currency_std_sc)) +
        geom_line(aes(colour = League))+
        geom_area(aes(colour = League), position = "identity", alpha = 0.1)+
        scale_x_continuous(breaks = seq(0, length(poedbStandardSC$Day), by=5))+
        theme(legend.position = "bottom",
              axis.line = element_line(colour = "black",size = 2),
              panel.grid.minor = element_blank(),
              panel.grid.major =  element_line(color = "gray90"),
              
              panel.background = element_blank(),
              #panel.background = element_rect(fill = "#2B3F50"),
              #plot.background=element_rect(fill = "#2B3F50"),
              
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"),
        )
    )
  )
  

  output$currency_standard_HC_plotly <- renderPlotly(
    ggplotly(  
      ggplot(data = poedbStandardHC[poedbStandardHC$League %in% input$standard_hc_selector,],
             aes_string(x = "Day", y = input$currency_std_sc)) +
        geom_line(aes(colour = League))+
        geom_area(aes(colour = League), position = "identity", alpha = 0.1)+
        scale_x_continuous(breaks = seq(0, length(poedbStandardHC$Day), by=5))+
        theme(legend.position = "bottom",
              axis.line = element_line(colour = "black",size = 2),
              panel.grid.minor = element_blank(),
              panel.grid.major =  element_line(color = "gray90"),
              
              panel.background = element_blank(),
              #panel.background = element_rect(fill = "#2B3F50"),
              #plot.background=element_rect(fill = "#2B3F50"),
              
              axis.title.x = element_text(face="bold"),
              axis.title.y = element_text(face="bold"),
        )
    )
  )

  
}

# Run the app ----
shinyApp(ui = ui, server = server)

  
