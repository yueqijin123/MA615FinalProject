#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

knitr::opts_chunk$set(echo = TRUE)
pacman::p_load("tidyverse","sf","dplyr","shiny","shinydashboard","shinypanels","leaflet","htmlwidgets","gtExtras","gt","ggplot2",
               "readxl","gapminder", "ggforce", "gh", "globals", "openintro", "profvis", 
               "RSQLite", "shiny", "shinycssloaders", "shinyFeedback", 
               "shinythemes", "testthat", "thematic", "tidyverse", "vroom", 
               "waiter", "xml2", "zeallot","DT","readxl","treemap","treemapify","rsconnect")


#population_data <- read_excel('/Users/charlenej/Desktop/MA615FinalProject/Tonga/data/tongapopulation.xlsx')
#literacyrates_data <- read_excel('/Users/charlenej/Desktop/MA615FinalProject/Tonga/data/tongaliteracyrates.xlsx')
#Populationbyage <- read_excel('/Users/charlenej/Desktop/MA615FinalProject/Tonga/data/Proportion of population by age group1950-2050.xlsx')
#Lifescan <- read_excel('/Users/charlenej/Desktop/MA615FinalProject/Tonga/data/LifeExpectancy.xlsx')
#BirthRate <- read_excel('/Users/charlenej/Desktop/MA615FinalProject/Tonga/data/BirthRate.xlsx')
#Urban <- read_excel('/Users/charlenej/Desktop/MA615FinalProject/Tonga/data/Tongaurban.xlsx')
#gdp_data <- read_excel('/Users/charlenej/Desktop/MA615FinalProject/Tonga/data/GDP.xlsx')

population_data <- read_excel("tongapopulation.xlsx")
literacyrates_data <- read_excel("tongaliteracyrates.xlsx")
Populationbyage <- read_excel("Proportion of population by age group1950-2050.xlsx")
Lifescan <- read_excel("LifeExpectancy.xlsx")
BirthRate <- read_excel("BirthRate.xlsx")
Urban <- read_excel("Tongaurban.xlsx")
gdp_data <- read_excel("GDP.xlsx")


ui <- fluidPage(
  
  titlePanel("Tonga Island: A Hidden gem in South Pacific"),
  
  navbarPage("", id = "mainNav",
             tabPanel("Research Objects", h2("Research Objects"),
                      HTML("There is the research objectives focused on Tonga Island<br><br>
                      General Description of Tonga Island:<br>
                      1.Map Creation: Develop a map of Tonga Island and another showing its location in the world. Utilize mapping packages in R to create these visualizations.<br>
                      2.Key Facts Compilation and Brief Description: Gather essential facts about Tonga Island, such as its geography, climate, political structure, economy, and culture.<br><br>
                      Key Demographics:<br>
                      Research and present data on the population scare, birth rate, age struction, lifescan, education, literacy rate, and urban-rural split.<br><br>
                      Comparative Analysis:<br>
                      Compare Tonga Island with other island states in the same region (with Fiji,Samoa and Vanuatu) in terms of economic performance, environmental challenges.<br><br>
                      SWOT Analysis:<br>
                      1.Strengths: Identify Tonga Island’s key strengths, such as natural resources, cultural heritage, or strategic location.<br>
                      2.Weaknesses: Discuss challenges like vulnerability to natural disasters, economic dependencies, or infrastructural limitations.<br>
                      3.Opportunities: Explore potential areas for growth and development, such as tourism, sustainable energy, or digital innovation.<br>
                      4.Threats: Analyze external risks, including climate change impacts, global economic trends, or geopolitical issues."),
                      
                      img(src = "https://upload.wikimedia.org/wikipedia/commons/9/9a/Flag_of_Tonga.svg ", style = "max-width: 100%; height: auto;")
                      
             ),
             tabPanel("Tonga Island Introduction",
                      conditionalPanel(
                        condition = "input.mainNav == 'Tonga Island Introduction'",
                        navlistPanel(
                          tabPanel("Map of Tonga Island State", h2("Tonga Island Map Description"),
                                   p("The map shows part of Tonga Island, specifically an area around the capital city, Nuku'alofa. This section of the island has a coastline that curves inward, creating a sheltered area on the western side. The eastern side of the island appears to be more exposed to the open ocean. There are several places marked on the map, such as Houma, Nukunuku, Tofua, Ha'ateiho, and Vaini on the western side, and Lapaha and Tatakamotonga on the eastern side. The southern tip of the island has a location labeled Fua'amotu, which is close to an airport."),
                                   leafletOutput("mapTonga")
                                   ),
                          
                          tabPanel("World Map Showing Tonga Island State Location", h2("World Map of Tonga Island Description"),
                                   p("Tonga is a Polynesian sovereign state and archipelago in the southern Pacific Ocean. It is situated about a third of the way between New Zealand and Hawaii. The kingdom of Tonga is spread over approximately 170 islands, many of which are uninhabited and covered with tropical forests, with line-ups of white beaches and coral reefs."),
                                   leafletOutput("mapWorld")
                                   ),
                          tabPanel("Key Facts about Tonga Island State", h2("Key Facts"),
                                   img(src = "https://encrypted-tbn3.gstatic.com/licensed-image?q=tbn:ANd9GcQ0am_4FKAKuwDsHApCLtD-7rST2hEdn1oNUZhRRTPbvRWbr-e-d-FoPXsGQeVOW08fGfu5tmWZjBgqcPuvVl2QFuu_y_eYAR2f_XGWSA", style = "max-width: 100%; height: auto;"),
                                   HTML("Geographical Location and Overview: <br>
                                        Tonga Island State, commonly referred to as Tonga, is an island nation located in the South Pacific Ocean. It consists of about 170 islands spread over an area of approximately 700,000 square kilometers, of which only about 36 are inhabited. The main islands of Tonga include Tongatapu, Vava'u, and the Ha'apai group.<br><br>
                                        Population and Culture:<br>
                                        The population of Tonga is around 100,000, with most residing on the main island of Tongatapu. Tongans have a rich cultural heritage and traditions, including traditional dances, music, and carving. Christianity has a profound influence on the local culture, with religious activities and churches playing a significant role in social life.<br><br>
                                        Economic Situation:<br>
                                        Tonga's economy mainly relies on agriculture, fishing, and tourism. Major export products include coconuts, bananas, and vanilla. In recent years, tourism has become an essential pillar of the economy, attracting many visitors seeking unique cultural experiences and natural beauty. Additionally, remittances from Tongans living abroad are a significant part of Tonga's economy.<br><br>
                                        Natural Environment and Climate:<br>
                                        Tonga is known for its spectacular beaches, coral reefs, and tropical rainforests. These natural landscapes provide diverse habitats for wildlife. The climate in Tonga is a tropical maritime climate, warm and humid throughout the year, influenced by the southeast trade winds.<br><br>
                                        Political System:<br>
                                        Tonga is a constitutional monarchy with a king. Over the past few decades, Tonga has undergone a gradual democratization of its political system. The legislative body includes a parliament elected by the people and nobles appointed by the king. Political reforms aim to increase government transparency and public participation.<br><br>"),
                                   img(src = "https://encrypted-tbn0.gstatic.com/licensed-image?q=tbn:ANd9GcT9izjzRleEOHfFQVg4fIB5VX9H-KpdNriOxtnGgx0zoNVsEAjCpitZYansy4rHR8clGoulpF_5bsgF_ZqkNTWwRizP7ZdBbBtfTHG84IQ", style = "max-width: 100%; height: auto;")
                                 ),
                          
                          tabPanel("Brief Description of Tonga Island State", h2("Brief Description"),
                                   HTML("Tonga's history is steeped in ancient Polynesian culture and mythology. It's one of the few places in the Pacific to have never fully lost its indigenous governance to colonial powers. This rich history is evident in its archaeological sites, oral traditions, and cultural festivals."),
                                   img(src = "https://media-cldnry.s-nbcnews.com/image/upload/t_fit-1240w,f_auto,q_auto:best/newscms/2014_30/584956/tonga-stone-structure.jpg", style = "max-width: 100%; height: auto;"),
                                   p("A population of around 100,000 people predominantly inhabits the main islands. Tongan, a Polynesian language, is widely spoken and maintains a strong presence alongside English."),
                                   img(src = "https://cdn.britannica.com/69/194769-050-98C4770D/men-Tongan-one-kailo-dances.jpg", style = "max-width: 100%; height: auto;")
                                   
                                   )
                        )
                      )
             ),
            
              tabPanel("Key Demographics",  
                      conditionalPanel(
                      condition = "input.mainNav == 'Key Demographics'",
                        navlistPanel(
                          
                          tabPanel("Population Scale and Birth Rate",h2("Population Scale and Birht Rate"),
                                  sliderInput("yearRange", "Select Year Range (Population Scale):",
                                               min = min(population_data$Year),
                                               max = max(population_data$Year),
                                               value = c(min(population_data$Year), max(population_data$Year)),
                                               step = 1),
                                  plotOutput("populationPlot") ,
                                  HTML("We can conclude that :<br>
                                  1. The average annual population growth rate is approximately 0.75%.<br>
                                  2. The smallest recorded population number during the period was 67,428 in the year 1960.<br>
                                  3. The largest recorded population number was 107,611, which occurred in the year 2011.<br><br>
                                  From this plot, we can conclude that Tonga's population has been growing over the observed period. The growth appears to be quite steady without any drastic increases or decreases. However, there seems to be a plateau or a slight decrease in population size towards the end of the timeline, which could suggest a slowing of population growth in recent years."),
                                
                                  plotOutput("BirthratePlot"),
                                  HTML("We can see that Tonga has experienced a significant decline in its birth rate over the past six decades. The line decreases sharply from around 1960 until the late 1970s, indicating a rapid decline in birth rates during that period. Following this, there appears to be a period of fluctuation before the decline continues at a more gradual pace.")
                                   ),
                  
                          tabPanel("Age Structure and Lifescan", h2("Age Structure and Lifescan"),
                                   p("This section provides visualizations of the age group distribution and average lifespan in Tonga Island over years."),
                                   plotOutput("AgePlot") ,
                                   HTML("From this plot, we can observe several key trends:<br>
                                   1. The proportion of the population aged 0-14 has been decreasing over time, indicated by the downward trend of the blue line.<br>
                                   2. Conversely, the proportion of the population within the age groups 60+, 65+, and 80+ has been on an upward trajectory, as shown by the ascending red, orange, and purple lines, respectively.<br>
                                   3. The age group 15-64, represented by the green line, shows an initial increase followed by a plateau, suggesting a stable proportion of the population in this age bracket."),
                                   HTML("<br>
                                        "),
                                   plotOutput("LifescanPlot"),
                                   HTML("The life expectancy in Tonga has been relatively high over the years and shows an upward trend. But this increase appears to be gradual, without any drastic changes.")
                                   ),
                      
                          tabPanel("Literacy Rate", h2("Literacy Rate"),
                                  plotOutput("LiteracyRatesPlot"),
                                  HTML("The plot displays literacy rate by sex in Tonga, specifically for the age group of 15 and above. The male literacy rate is indicated as 99.4%, while the female literacy rate is slightly higher at 99.5%.<br><br>
                                        We can conclude that Tonga has an exceptionally high literacy rate among its population, with very little difference between genders. Both young men and women have almost universal access to education or the ability to read and write. This reflects a successful educational system with respect to literacy and gender parity in education within this age group.")       
                                  ),
                          
                          tabPanel("Urban and Rural Distribution", h2("Urban and Rural Distribution"),
                          plotOutput("UrbanPlot"),
                          HTML("The rural population appears to be stable or slightly increasing, without significant fluctuations over the years. The urban population is much lower than the rural population, and it also shows a stable pattern without significant increases or decreases.")
                                  ),
                        )
                      )
                    ),
                      
                      
             tabPanel("Comparative Analysis with Fiji,Samoa and Vanuatu",
                      #p("Choose a few SIDS from the Pacific region for comparison, such as Fiji, Samoa, or Vanuatu. Since these countries have similarities in size, population, or economic structure to make the comparison meaningful."),
                      conditionalPanel(
                        condition = "input.mainNav == 'Comparative Analysis with Fiji,Samoa and Vanuatu'",
                        navlistPanel(
                          tabPanel("Economic Indicators", 
                
                                   plotOutput("GDPPlot"),
                                   HTML("The chart provides the following insights:<br>
                                   1.Fiji's GDP generally increased over the years, although there is a noticeable dip towards the end of the chart.<br>
                                   2.The GDPs of Samoa, Tonga, and Vanuatu remained relatively stable and close to each other throughout the observed period.<br>
                                   3.Fiji's GDP was significantly higher than the other three countries throughout the time frame.<br>
                                   4.There were no significant downturns in the GDPs of all four countries from 2010 to around 2022, suggesting economic stability in these Pacific Island nations during the observed period.<br>"),
                                   ),
                          tabPanel("Environmental Challenges", 
                                   HTML("Tonga<br>
                                        1.Climate Change Impact: Tonga faces rising sea levels and increased temperatures, which are significant threats to its low-lying areas.<br>
                                        2.Natural Disasters: The country is prone to frequent cyclones and occasional tsunamis, posing serious risks to its infrastructure and population.<br><br>
                                        Fiji<br>
                                        1.Climate Change Impact: Fiji is grappling with rising sea levels and more frequent cyclones, which disrupts its ecological balance and community livelihoods.<br>
                                        2.Natural Disasters: The island faces both cyclones and earthquakes, necessitating robust disaster preparedness and response mechanisms.<br><br>
                                        Samoa<br>
                                        1.Climate Change Impact: Samoa deals with changing weather patterns and increased temperatures, impacting its agriculture and marine life.<br>
                                        2.Natural Disasters: Cyclones and tsunamis are common in Samoa, requiring continuous preparedness and alert systems.<br><br>
                                        Vanuatu<br>
                                        1.Climate Change Impact: Vanuatu faces rising sea levels and an increased frequency of cyclones, threatening its coastal communities and ecosystems.<br>
                                        2.Natural Disasters: The country is prone to volcanic eruptions and cyclones, making disaster response a crucial aspect of governance."),
                                   img(src = "https://www.un.org/sustainabledevelopment/wp-content/uploads/sites/6/2016/05/05-19-2016Environment.jpg", style = "max-width: 100%; height: auto;")
                        
                                   )
                      )
                      )
             ),
             tabPanel("SWOT Analysis of Tonga Island State",
                      conditionalPanel(
                        condition = "input.mainNav == 'SWOT Analysis of Tonga Island State'",
                        navlistPanel(
                          tabPanel("Strength", h2("Strength"),
                                   HTML("1.Natural Resources: Tonga Island State boasts rich natural resources, including beautiful beaches and coral reefs, beneficial for tourism.<br>
                                   2.Cultural Heritage: Unique culture and traditions can attract tourists interested in these aspects.<br>
                                   3.Geographical Location: Centrally located in the Pacific Ocean, making it conveniently accessible for tourists from neighboring countries.")
                                   ),
                          tabPanel("Weakness", h2("Weakness"),
                                   HTML("1.Economic Development Level: As a small island nation, its economic scale is limited, possibly lacking sufficient infrastructure and investment.<br>
                                        2.Vulnerability to Natural Disasters: Prone to threats like tropical storms and rising sea levels, especially for low-lying islands.<br>
                                        3.Limited Resources: Limited resources might make it challenging to support large-scale economic development.")
                                   ),
                          tabPanel("Opportunity", h2("Opportunity"),
                                   HTML("1.Tourism Development: Attracting more international tourists through the development of eco-tourism and cultural tourism.<br>
                                   2.Renewable Energy: Utilizing renewable energy sources like solar and wind energy to reduce dependence on external resources.<br>
                                   3.International Cooperation: Improving global influence through partnerships with other countries and international organizations.")
                                   ),
                          tabPanel("Threat", h2("Threat"),
                                   HTML("1.Climate Change: Global climate change, impacting sea levels and weather patterns, could severely affect Tonga's natural environment and lifestyle.<br>
                                        2.Economic Dependence: Over-reliance on tourism can lead to economic vulnerability, especially during global economic or health crises.<br>
                                        3.Political Instability: Regional political instability might affect investments and the tourism industry.")
                                   )
                        )
                      )
             ),
             tabPanel("Welcome to Tonga", 
                      HTML("In Tonga, you have the opportunity to immerse yourself in the unique culture, including participating in traditional kava ceremonies and enjoying our traditional dances and music.<br><br> 
                           Additionally, Tonga's natural landscapes are truly breathtaking. You can explore Tonga's beautiful beaches, coral reefs, and tropical rainforests. For those who love adventure, Tonga also offers a variety of activities such as diving, hiking, and whale watching.<br><br> 
                           No matter where you are from in the world, Tonga Island welcomes you!"),
                      img(src = "https://lindseyland.files.wordpress.com/2011/06/dsc00103.jpg", style = "max-width: 100%; height: auto;")
                      )
  ),
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = "orange", border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
# Population Scale and BirthRate Rate
    output$populationPlot <- renderPlot({
      filtered_data <- subset(population_data, Year >= input$yearRange[1] & Year <= input$yearRange[2])
      ggplot(filtered_data, aes(x = Year, y = Population)) +
        geom_col() +
        theme_minimal() +
        labs(title = "Population Scale in Tonga Over Years",
             x = "Year", 
             y = "Population")+
             geom_col(fill = "purple")+
        theme(panel.grid = element_blank(), 
              plot.title = element_text(hjust = 0.5, size = 15),   # 居中并调整标题大小
              axis.text.x = element_text(size = 12),    # 调整x轴值的大小
              axis.text.y = element_text(size = 12, vjust = 3),    # 调整y轴值的大小和垂直位置
              axis.title.x = element_text(size = 14),   # 调整x轴标签的大小
              axis.title.y = element_text(size = 14))   # 调整y轴标签的大小
    })
    
    output$BirthratePlot <- renderPlot({
    #filtered_data <- subset(BirthRate, Year >= input$BirthYear[1] & Year <= input$BirthYear[2])
    ggplot(BirthRate, aes(x = Year, y = BirthRate)) +
      geom_line() +
      labs(title = "Birth Rate (per 1,000 people) in Tonga in 1960-2021",
           x = "Year",
           y = "BirthRate") +
      geom_line(color = "purple") +  # 指定折线颜色为紫色
      theme_minimal()+
        theme(
          # panel.grid = element_blank(),             # 移除网格线
          plot.title = element_text(hjust = 0.5, size = 15),   # 居中并调整标题大小
          axis.text.x = element_text(size = 12),    # 调整x轴值的大小
          axis.text.y = element_text(size = 12, vjust = 3),    # 调整y轴值的大小和垂直位置
          axis.title.x = element_text(size = 14),   # 调整x轴标签的大小
          axis.title.y = element_text(size = 14))   # 调整y轴标签的大小 
      
    })
  
# Age Structure and Lifescan 
    
output$AgePlot <- renderPlot({     
    # 绘制不同颜色的折线图
    ggplot(Populationbyage, aes(x = Year)) +
      geom_line(aes(y = `0-14`, color = "Aged 0-14")) +
      geom_line(aes(y = `15-64`, color = "Aged 15-64")) +
      geom_line(aes(y = `60+`, color = "Aged 60+")) +
      geom_line(aes(y = `65+`, color = "Aged 65+")) +
      geom_line(aes(y = `80+`, color = "Aged 80+")) +
      labs(title = "Proportion of population by age group in Tonga (1950-2050)",
           x = "Year",
           y = "Population Percentage",
           color = "Age Group") +
      scale_color_manual(values = c("Aged 0-14" = "blue",
                                    "Aged 15-64" = "green",
                                    "Aged 60+" = "red",
                                    "Aged 65+" = "orange",
                                    "Aged 80+" = "purple")) +
      theme_minimal()+
      theme(
        # panel.grid = element_blank(),             # 移除网格线
        plot.title = element_text(hjust = 0.5, size = 15),   # 居中并调整标题大小
        axis.text.x = element_text(size = 12),    # 调整x轴值的大小
        axis.text.y = element_text(size = 12, vjust = 3),    # 调整y轴值的大小和垂直位置
        axis.title.x = element_text(size = 14),   # 调整x轴标签的大小
        axis.title.y = element_text(size = 14))   # 调整y轴标签的大小 

})   

output$LifescanPlot <- renderPlot({
  filtered_data <- subset(Lifescan, Year >= input$yearRange[1] & Year <= input$yearRange[2])
  ggplot(filtered_data, aes(x = Year, y = Lifescan)) +
    geom_col() +
    theme_minimal() +
    labs(title = "Lifescan in Tonga Over Years",
         x = "Year", 
         y = "Lifescan")+
    geom_col(fill = "purple")+
    theme(#panel.grid = element_blank(), 
          plot.title = element_text(hjust = 0.5, size = 15),   # 居中并调整标题大小
          axis.text.x = element_text(size = 12),    # 调整x轴值的大小
          axis.text.y = element_text(size = 12, vjust = 3),    # 调整y轴值的大小和垂直位置
          axis.title.x = element_text(size = 14),   # 调整x轴标签的大小
          axis.title.y = element_text(size = 14))   # 调整y轴标签的大小
})


    
"Education and Literacy Rates"

# Adult literacy rate, by sex (% of people ages 15 and above)
# 筛选2021年的数据，并且筛选出指定的指标
output$LiteracyRatesPlot <- renderPlot({ 
  filtered_data <- literacyrates_data[literacyrates_data$Year == 2021 & 
                                        literacyrates_data$`Indicator Name` %in% c("Literacy rate, adult female (% of females ages 15 and above))",
                                                                                   "Literacy rate, adult male (% of males ages 15 and above)"),]
  # 创建一个用于绘图的数据框架
  plot_data <- data.frame(
    Sex = c("Female", "Male"),
    LiteracyRate = c(99.5, 99.4)
  )
  
  # 绘制横向条形图并在条形上添加数值，同时调整条形的宽度和图形的边际
  ggplot(plot_data, aes(x = Sex, y = LiteracyRate, fill = Sex)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.1), width = 0.3) + # 调整条形图间距
    geom_text(aes(label = LiteracyRate), vjust = -0.3, color = "black") +
    scale_fill_manual(values = c("purple", "orange")) +
    coord_flip() +
    labs(title = "Literacy rate by sex in 2021 (% of ages 15 and above)",
         x = "Sex",
         y = "Literacy Rate (%)") +
    theme_minimal() +
    theme(
          panel.grid = element_blank(),             # 移除网格线
               plot.title = element_text(hjust = 0.5, size = 15),   # 居中并调整标题大小
                axis.text.x = element_text(size = 12),    # 调整x轴值的大小
                axis.text.y = element_text(size = 12, vjust = 3),    # 调整y轴值的大小和垂直位置
                axis.title.x = element_text(size = 14),   # 调整x轴标签的大小
                axis.title.y = element_text(size = 14))   # 调整y轴标签的大小 
})

   
"Urban and Rural Distribution" 
output$UrbanPlot <- renderPlot({     
ggplot(Urban, aes(x = Year, y = Value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Urban and Rural Distribution in Tonga (1991-2023)",
       x = "Year",
       y = "Value",
       fill = "Area") +
  scale_fill_manual(values = c("Urban" = "purple", "Rural" = "orange")) +
  theme_minimal()+
    theme(
      #panel.grid = element_blank(),             # 移除网格线
      plot.title = element_text(hjust = 0.5, size = 15),   # 居中并调整标题大小
      axis.text.x = element_text(size = 12),    # 调整x轴值的大小
      axis.text.y = element_text(size = 12, vjust = 3),    # 调整y轴值的大小和垂直位置
      axis.title.x = element_text(size = 14),   # 调整x轴标签的大小
      axis.title.y = element_text(size = 14))   # 调整y轴标签的大小 
})

output$mapTonga <- renderLeaflet({
  leaflet() %>%
    setView(lng = -175.1982, lat = -21.1790, zoom = 10) %>%
    addTiles()
})

# 创建展示汤加岛在世界地图上位置的地图
output$mapWorld <- renderLeaflet({
  leaflet() %>%
    setView(lng = -175.1982, lat = -21.1790, zoom = 2) %>%
    addTiles() %>%
    addMarkers(lng = -175.1982, lat = -21.1790, popup = "Tonga Island State")
})

output$GDPPlot <- renderPlot({
  
  # 将数据从宽格式转换为长格式，适合ggplot2使用
  gdp_data_long <- tidyr::gather(gdp_data, "Country", "GDP", -Year)
  
  # 绘制折线图
  ggplot(gdp_data_long, aes(x = Year, y = GDP, color = Country, group = Country)) +
    geom_line() +
    theme_minimal() +
    labs(title = "GDP of Tonga, Fiji, Samoa and Vanuatu over Years",
         x = "Year",
         y = "GDP")+
    theme(
      #panel.grid = element_blank(),             # 移除网格线
      plot.title = element_text(hjust = 0.5, size = 15),   # 居中并调整标题大小
      axis.text.x = element_text(size = 12),    # 调整x轴值的大小
      axis.text.y = element_text(size = 12, vjust = 3),    # 调整y轴值的大小和垂直位置
      axis.title.x = element_text(size = 14),   # 调整x轴标签的大小
      axis.title.y = element_text(size = 14))   # 调整y轴标签的大小 
})

}

# Run the application 
shinyApp(ui = ui, server = server)
