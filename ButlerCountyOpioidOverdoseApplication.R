#Produces an app for visualizing the data of the opioid epidemic provided by the Butler County Coroner 

#Load needed packages
library(shiny)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(extrafont)
library(ggmap)
library(shinydashboard)
library(lubridate)

#Load in the cleaned and manipulated data
load(file="coronerAppFinal2.Rdata")

#add time variable

full_data$month.year<-format(as.Date(full_data$D.O.D.), "%Y-%m")

#Assigning color pallete to each vector for later use
drug_colors <- as.character(categories$Colors)
names(drug_colors) <- categories$DrugName

substance_colors <- as.character(unique(categories$Colors))
names(substance_colors) <- unique(categories$Category)
substance_colors[19] <- "#11a05b"

city_colors <-unique(categories$Colors)
names(city_colors) <- unique(city_township.d$`CITY/TWP OF DEATH`)
city_colors[6] <- "#ff9900"

categories$DrugName <- as.character(categories$DrugName)

#Create function to make the slider labels categorical rather than numberic
#slider input source: https://stackoverflow.com/questions/40872964/issue-changing-shiny-slider-to-represent-categorical-string-features-with-custom
JScode <-
  "$(function() {
setTimeout(function(){
var vals = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'];
for (i = 1; i >= vals.length; i++) {
var val = (1,12);
vals.push(val);
}
$('#month').data('ionRangeSlider').update({'values':vals})
}, 12)})"


### Define UI for application
ui <- dashboardPage(skin = "blue", #stylize the app
                    dashboardHeader(),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        menuItem("Home", tabName = "home"),
                        menuItem("How to use the app", tabName = "how"),
                        menuItem("Drugs Found in Overdose Decedent",
                                 menuSubItem("Demographics", tabName = "substance"),
                                 menuSubItem("Annual Trends", tabName = "time"),
                                 menuSubItem("Annual Category Trends", tabName = "time2")),
                        menuItem("Location Associated With Overdoses",
                                 menuSubItem("Address of Overdose Incidents", tabName = "county"),
                                 menuSubItem("Trends by Cities & Townships", tabName = "map"),
                                 menuSubItem("Location of Incident vs Death", tabName = "facet"))
                      )
                    ),
                    #tags$head(tags$script(HTML(JScode))
                    #Tabbing the panels source: https://shiny.rstudio.com/articles/layout-guide.html
                    dashboardBody(
                      tabItems(
                        tabItem(tabName ="home",
                                fillPage(imageOutput("home", width = "100%", height = "100%"))),
                        
                        tabItem(tabName =  "how",
                                fillPage(imageOutput("how", width = "100%", height = "100%"))),
                        
                        tabItem(tabName = "substance",
                                fluidRow(
                                  box(selectizeInput(inputId= "sex", label = h4("Click in the box to select gender(s): "), 
                                                     choices = c("Male", "Female"), selected = "Male", multiple = TRUE), 
                                      status = "primary", width = 4),
                                  box(selectizeInput(inputId= "race", label = h4("Click in the box to select race/ethnicity(s): "),
                                                     choices = c("White", "Black", "Hispanic", "Asian"),
                                                     multiple = TRUE, selected = "White"),  
                                      status = "primary", width = 4),
                                  box(selectizeInput(inputId= "age", label = h4("Click in the box to select age range(s): "),
                                                     choices = c("Under 21", "21-30", "31-40", "41-50", "51-60", "61-80", "Over 80"), 
                                                     multiple = TRUE, selected = c("31-40", "41-50")),
                                      status = "primary", width = 4),
                                  plotOutput(outputId = "substance", width='80%', height = "620px")
                                )),
                        
                        tabItem(tabName = "time", 
                                fluidRow(
                                  box(selectizeInput(inputId= "drug", label = h4("Click in the box to select drug(s): "),
                                                     choices = unique(arrange(YEAR_DATA,-count)$substance), multiple = TRUE, 
                                                     selected = c("Heroin", "Cocaine", "Fentanyl","Carfentanil"),
                                                     options = list(maxItems = 6)),
                                      status = "primary"),
                                  box(title="The drugs are colored by their respective categories.", width = 4,
                                      tableOutput('timeTable'),collapsible = TRUE,collapsed = TRUE,status = "primary"),
                                  plotOutput(outputId = "time", width = '80%', height = "620px")#,
                                  # box(,width=3)
                                  
                                  
                                )),
                        tabItem(tabName = "time2", 
                                fluidRow(
                                  box(selectizeInput(inputId= "category", label = h4("Click in the box to select category(s): "),
                                                     choices = unique(arrange(YEAR_DATA2,-count)$Category), multiple = TRUE, 
                                                     selected = c("Other Opioids", "Fentanyl","Fentanyl Analogs"),
                                                     options = list(maxItems = 6)),
                                      status = "primary"),
                                  # box(h4("2018 data is January-August, inclusive."), width = 4),
                                  plotOutput(outputId = "time2", width = '80%', height = "630px")
                                  
                                  
                                )),
                        tabItem(tabName = "county", 
                                fluidRow(
                                  box(sliderInput("year", h4("Press play to animate the graph over the years:"),
                                                  min = 2013,
                                                  max = 2018,
                                                  value = 2013,
                                                  step = 1,
                                                  sep = "", 
                                                  animate = animationOptions(interval = 2000,loop = FALSE, playButton = icon("play"))),
                                      status = "primary"),
                                  
                                  plotOutput(outputId = "county", width = "60%", height = "620px")
                                )),
                        
                        
                        tabItem(tabName = "map", 
                                fluidRow(
                                  box(selectizeInput(inputId = "city", 
                                                     label= h4("Click in the box to select a city or township to highlight:"), 
                                                     choices = unique(city_township.i$`CITY/TWP/CO OF INCIDENT`), multiple = TRUE, 
                                                     selected = c("Hamilton", "Middletown"), options = list(maxItems = 4)), 
                                      status = "primary"),
                                  #box(h4("2018 data is January-August, inclusive."), width = 4),
                                  plotOutput(outputId = "map", height = "620px",width = "80%")
                                  
                                )),
                        
                        tabItem(tabName = "facet",
                                fluidRow(
                                  box(selectizeInput(inputId= "type", label = h4("Select Location of:"),
                                                     choices = c("Incident", "Death"), 
                                                     selected = "Incident"), status = "primary", width = 4),
                                  box(selectizeInput(inputId = "munic", label= h4("Click in the box to select a city or township to get on more information:"), 
                                                     choices = unique(city_township.d$`CITY/TWP OF DEATH`), multiple = TRUE, 
                                                     selected = c("Hamilton", "Fairfield", "West Chester", "Middletown"), options = list(maxItems = 4)), 
                                      status = "primary", width = 4),
                                  
                                  box(h4("The location of the presumed incident is the location of overdose, which may not be the same as the death."), width = 4),
                                  
                                  plotOutput(outputId = "facet", height = "620px", width = "100%")              ))
                      )
                      
                    )
)


server <- function(input, output) {
  output$home<-renderImage({
    list(src="home.png", width = '100%')},deleteFile=FALSE)
  output$how<-renderImage({
    list(src="how to use the app.png", width = '100%')},deleteFile=FALSE)
  output$substance <-
    
    renderPlot({
      #Filter data according to the input selections for plot 1
      DEM_DATA<-DEM_DATA %>%
        filter(SEX %in% c(input$sex), RACE %in% c(input$race), GROUPING %in% c(input$age)) %>% 
        group_by(SEX,substance) %>% 
        na.omit() %>%
        summarise(count = sum(count))
      
      
      DEM_DATA <- DEM_DATA %>%
        filter(count > 9)
      
      
      #Create a bar chart
      # re-ordering bars source: https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
      
      p1<-ggplot() +
        geom_bar(data = DEM_DATA, aes(x = reorder(substance, count),y=count,fill=SEX,label=count),
                 stat = "identity", alpha = 0.7,position = position_stack(reverse = TRUE)) +
        scale_fill_manual(values=c("Male"="#0794f2","Female"="#d32a2a"))+
        geom_text(aes(x=substance,y=count,label=count),
                  position = position_stack(vjust = 0.5),
                  data=DEM_DATA,color="black",fontface="bold",size=5.5)+
        theme_minimal() +
        theme(#axis.text.x=element_text(hjust =1, size = 18), 
          #text=element_text(family="Times"), 
          plot.title=element_text(size = 30, hjust = 0.5),  
          plot.subtitle = element_text(size = 20, hjust = 0.5),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 17),
          axis.title.y = element_text(size = 17),
          legend.title = element_text(size=15),
          legend.text = element_text(size=15)) +  #Changing fonts source: https://stackoverflow.com/questions/27689222/changing-fonts-for-graphs-in-r
        labs(x = "", y ="Number of times drug traced in decedent\nMore than 1 drug can be found in the deceased.", title = "Drugs Found in Decedent (Traced 10 or more times) ", 
             subtitle = "2018 data for Fentanyl Analog Compounds are January-August, inclusive.",
             fill="Sex") +
        coord_flip()
      # ylim(0, 450)
      
      #Plot p1
      p1
    },width = 1000,height=590)
  
  output$timeTable <- renderTable({
    substance_data <- data.frame(YEAR_DATA %>%
                                   filter(substance %in% c(input$drug)))
    substance_text<- data.frame(substance_data %>% 
                                  group_by(substance) %>%
                                  filter(year == max(year)))
    substance_category <-left_join(substance_text,categories,by=c("substance"="DrugName")) %>%
      rename(Drug = substance) %>%
      select(Drug,Category)
    substance_category
  })
  output$time <-
    renderPlot({
      #Filter the data according to the inputs
      substance_data <- data.frame(YEAR_DATA %>%
                                     filter(substance %in% c(input$drug)))
      substance_text<- data.frame(substance_data %>% 
                                    group_by(substance) %>%
                                    filter(year == max(year)))
      substance_category <- as.character(unlist(left_join(substance_text,categories,by=c("substance"="DrugName")) %>%
                                                  select(Category)))
      substance_color <- as.character(unlist(left_join(substance_text,categories,by=c("substance"="DrugName")) %>%
                                               select(Colors)))
      
      #Create the plot
      p2<-ggplot() +
        geom_line(aes(x = year, y = count, group = substance), data = YEAR_DATA, color = "light gray", size = 0.8, alpha = 0.7) +
        geom_line(data = substance_data, aes(x = year, y = count, group = substance, color = substance),  size = 1.5, alpha = 0.7) +
        theme_minimal() +
        theme(#text=element_text(family="Times"), 
          plot.title=element_text(size = 30, hjust = 0.5),  
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          axis.text.y = element_text(size = 18), 
          plot.caption = element_text(size = 18, hjust = 0.5),
          plot.subtitle = element_text(size=20,hjust=0.5),
          legend.position = "none"
        ) +
        scale_x_continuous(limits = c(2013,2018.5))+
        scale_color_manual("Drugs", values =drug_colors[input$drug])+#,
        #breaks = unique(substance_color),
        #labels=unique(substance_category)) + #palette
        labs(caption = "Note: More than 1 drug can be found in the deceased.", y = "Number of times drug traced in decedent", 
             title = "Drugs Found in Overdose Decedent", x="",
             subtitle = "2018 data for Fentanyl Analog Compounds are January-August, inclusive.")  +
        geom_text(data = substance_text, aes(x = year, y = count, color = substance, label = substance),
                  size = 7, nudge_y = 2, nudge_x = .3)
      #Plot p2
      p2
    },width = 1000,height=590)
  output$time2 <-
    renderPlot({
      #Filter the data according to the inputs
      substance_data <- data.frame(YEAR_DATA2 %>%
                                     filter(Category %in% c(input$category)) )
      substance_text<- data.frame(substance_data %>% 
                                    group_by(Category) %>%
                                    filter(year == max(year)))
      
      
      #Create the plot
      p2<-ggplot() +
        geom_line(aes(x = year, y = count, group = Category), data = YEAR_DATA2, color = "light gray", size = 0.8, alpha = 0.7) +
        geom_line(data = substance_data, aes(x = year, y = count, group = Category, color = Category),  size = 1.5, alpha = 0.7) +
        theme_minimal() +
        theme(#text=element_text(family="Times"), 
          plot.title=element_text(size = 30, hjust = 0.5),  
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 18), 
          axis.text.y = element_text(size = 18), 
          plot.caption = element_text(size = 18, hjust = 0.5),
          plot.subtitle = element_text(size=20,hjust=0.5),
          legend.position = "none") +
        scale_x_continuous(limits = c(2013,2018.5))+
        scale_color_manual("Drug Categories", values =substance_colors[substance_text$Category])+#substance_colors[substance_text$Category] 
        labs(caption = "Note: More than 1 drug can be found in the deceased.", y = "Number of times drug found in decedent", 
             title = "Drug Categories Found in Overdose Decedent", x="",
             subtitle = "2018 data for Fentanyl Analog Compounds are January-August, inclusive.")  +
        geom_text(data = substance_text, aes(x = year, y = count, color = Category, label = Category),
                  size = 6, nudge_y = 2, nudge_x = .35)
      #Plot p2
      p2
    },width = 1000,height = 580
    )
  output$map <-
    renderPlot({
      #Filter data according to input
      color_city_township.i <- city_township.i %>% 
        filter(`CITY/TWP/CO OF INCIDENT` %in% input$city)
      
      #Create the line plot
      text_city_township<-color_city_township.i %>%
        select(`CITY/TWP/CO OF INCIDENT`, YEAR) %>%
        group_by(`CITY/TWP/CO OF INCIDENT`, YEAR) %>%
        summarise(total=n())
      
      text_city_township<-text_city_township %>% 
        group_by(`CITY/TWP/CO OF INCIDENT`) %>% 
        summarize(lastYR = max(YEAR), lastY = total[YEAR==lastYR])
      
      
      p4<-ggplot() +
        geom_line(data = city_township.i, aes(x = YEAR,group =`CITY/TWP/CO OF INCIDENT`), stat = "count", color = "light gray", size = 0.4, alpha = 0.8) +
        geom_line(data = color_city_township.i, aes(x = YEAR,group =`CITY/TWP/CO OF INCIDENT`, color = `CITY/TWP/CO OF INCIDENT`, label = `CITY/TWP/CO OF INCIDENT`),
                  stat = "count", size = 1.5, alpha = 0.8) +
        
        theme_minimal() +
        labs(x = "", y ="", title = "Count of Overdose Incidents by City/Township") +
        theme(#text=element_text(family="Times"), 
          plot.title=element_text(size = 30, hjust = 0.5),  
          axis.text.x = element_text(size = 18), 
          axis.text.y = element_text(size = 18),  
          legend.position = "none") +
        scale_color_manual("Drugs", values =city_colors[input$city]) +
        scale_x_continuous(limits = c(2013,2018.5))+
        scale_y_continuous(position = "left") +
        geom_text(data = text_city_township, aes(x = 2018.3, y = lastY, color = `CITY/TWP/CO OF INCIDENT`, 
                                                 label = `CITY/TWP/CO OF INCIDENT`),
                  size = 6)
      #Plot p4
      p4
    },width = 900,height=580)
  output$county <-
    renderPlot({
      #Make the values of the month match with the slider options
      full_data$Death_Month<-Death_Month<-ifelse(full_data$Death_Month == "January", 0,
                                                 ifelse(full_data$Death_Month == "February", 1,
                                                        ifelse(full_data$Death_Month == "March", 2,
                                                               ifelse(full_data$Death_Month == "April", 3,
                                                                      ifelse(full_data$Death_Month == "May", 4,
                                                                             ifelse(full_data$Death_Month == "June", 5,
                                                                                    ifelse(full_data$Death_Month == "July", 6,
                                                                                           ifelse(full_data$Death_Month == "August", 7,
                                                                                                  ifelse(full_data$Death_Month == "September", 8,
                                                                                                         ifelse(full_data$Death_Month == "October", 9,
                                                                                                                ifelse(full_data$Death_Month == "November", 10,
                                                                                                                       ifelse(full_data$Death_Month == "December", 11, full_data$Death_Month
                                                                                                                       ))))))))))))
      full_data<-as.data.frame(full_data)
      full_data$Death_Month <- as.numeric(full_data$Death_Month)
      #Filter the data according to the inputs
      full_data <-full_data %>% 
        filter(Death_Year %in% input$year)
      #, Death_Month == input$month)
      
      
      
      plot_data<- full_data %>% 
        filter(lat > 39.2, lat < 39.7, lon < (-84.3))
      
      cities<-data.frame(1, 1)
      cities<-as.data.frame(rbind(cities,
                                  c(39.398869, -84.558800),
                                  c(39.515751,-84.402283),
                                  c(39.335480,-84.559669),
                                  c(39.312278,-84.650503),
                                  c(39.506996,-84.745232),
                                  c(39.345131,-84.395699)))
      cities<-cities[-1,]
      cities$City <- c("Hamilton", "Middletown", "Fairfield", "Ross", "Oxford", "West Chester")
      
      colnames(cities)<-c("lat", "long", "City")
      
      
      p5<-ggplot() +
        
        geom_polygon(data=townships, aes(x=long,y=lat,group=group), fill = "white", color = "black", values = townships$NAME) +
        geom_polygon(data = corp, aes(x=long,y=lat,group=group), fill = "white", color = "black") +
        geom_point(aes(x=lon, y = lat), color = "#007299", size = 5, alpha = 0.4, data = plot_data) +
        
        scale_fill_gradient(low = "#6495ed", high = "#000080") +
        theme_minimal() +
        scale_y_continuous(labels = c("", "", "", "", ""), limits = c(39.25, 39.6)) +
        scale_x_continuous(labels = c("", "", "", "", "", "", ""), limits = c(-84.85, -84.3)) +
        labs(x = "", y ="", title = paste("Address of Overdose Incident:", input$year),
             subtitle = "Darker points represent a higher frequency.") +
        theme(#text=element_text(family="Times"), 
          plot.title=element_text(size = 30, hjust = 0.5),  legend.position = "none", axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(), 
          plot.subtitle=element_text(size = 20, hjust = 0.5),
          panel.border = element_blank())
      #Plot p5
      p5
      
    }
    
    )
  output$facet <-
    renderPlot({
      if(input$type == "Death") {
        location<-city_township.d %>% 
          select(`CITY/TWP OF DEATH`) %>% 
          group_by(`CITY/TWP OF DEATH`) %>% 
          summarise(total=n())
        
        location<-location %>% 
          filter(total > 4)
        
        p7<-ggplot(location, aes(x=reorder(`CITY/TWP OF DEATH`, total), y = total)) +
          geom_bar(stat = "identity", fill = "#FF7F50") +
          theme_minimal() +
          coord_flip() +
          labs(x = "", y = "", 
               title = "Location of Overdose Death",
               subtitle = "All years of data are represented in the graphs."
          ) +
          theme(axis.text.x=element_blank(),
                #text=element_text(family="Times"), 
                axis.text.y = element_text(size = 15), 
                plot.title = element_text(size = 30, hjust = 0.5),
                plot.subtitle = element_text(size = 20, hjust = 0.5),
                plot.caption = element_text(size = 17))+
          geom_text(data =location, aes(x = `CITY/TWP OF DEATH`, y = total, label = total), color = "black", fontface = "bold", hjust = -.09, size =5) 
        
        place<-city_township.d %>%
          filter(`CITY/TWP OF DEATH` %in% input$munic) %>%
          select(`PLACE OF DEATH`, `CITY/TWP OF DEATH`) %>%
          group_by(`PLACE OF DEATH`, `CITY/TWP OF DEATH`) %>%
          summarise(total=n())%>%
          filter(total > 1) %>%
          arrange(-total)
        
        p8<-ggplot(place, aes(x=reorder(`PLACE OF DEATH`, -total), y = total), fill = `PLACE OF DEATH`) +
          geom_bar(stat = "identity",fill = "#B2DFEE") +
          theme_minimal() +
          labs(x = "", y = "", title = paste0("Place of Opioid Overdose Death")) +
          theme(strip.text.x = element_text(size = 14, face = "bold"),axis.text.x=element_text(size = 16, angle = 30, hjust = 1),#text=element_text(family="Times"), 
                axis.text.y = element_text(size = 16), title = element_text(size = 17)) +
          facet_wrap(~`CITY/TWP OF DEATH`, scales = "free", ncol = 2) +
          guides(fill = "none") +
          geom_text(data = place, aes(x = `PLACE OF DEATH`, y = total, label = total),vjust = .25, size = 6, fontface = "bold") 
      }
      if(input$type == "Incident") {
        location<-city_township.i %>% 
          select(`CITY/TWP/CO OF INCIDENT`) %>% 
          group_by(`CITY/TWP/CO OF INCIDENT`) %>% 
          summarise(total=n())
        
        location<-location %>% 
          filter(total > 4)
        
        p7<-ggplot(location, aes(x=reorder(`CITY/TWP/CO OF INCIDENT`, total), y = total)) +
          geom_bar(stat = "identity", fill = "#dc3912", alpha = 0.8, color = "#dc3912") +
          theme_minimal() +
          coord_flip() +
          labs(x = "", y = "", title = "Location of Overdose Incident",
               subtitle = "All years of data are represented in the graphs.",
               caption = "Municipalities with events less than 5 have been filtered out of the data.") +
          theme(axis.text.x=element_blank(),
                #text=element_text(family="Times"), 
                axis.text.y = element_text(size = 15), 
                plot.title = element_text(size = 30, hjust = 0.5),
                plot.subtitle = element_text(size = 20, hjust = 0.5),
                plot.caption = element_text(size = 17)) +
          geom_text(data =location, aes(x = `CITY/TWP/CO OF INCIDENT`, y = total, label = total), color = "black", fontface = "bold", hjust = -.09, size =5)
        
        
        place<-city_township.i %>%
          filter(`CITY/TWP/CO OF INCIDENT` %in% input$munic) %>%
          select(`PLACE OF INCIDENT`, `CITY/TWP/CO OF INCIDENT` ) %>%
          group_by(`PLACE OF INCIDENT`, `CITY/TWP/CO OF INCIDENT` ) %>%
          summarise(total=n()) %>%
          filter(total > 1)%>%
          arrange(`CITY/TWP/CO OF INCIDENT`,-total)
        
        
        p8<-ggplot(place, aes(x=reorder(`PLACE OF INCIDENT`,-total), y = total, fill = `PLACE OF INCIDENT`)) +
          geom_bar(stat = "identity", fill = "#0099c6", alpha = 0.8, color = "#0099c6") +
          theme_minimal() +
          labs(x = "", y = "", title = "Place of Overdose") +
          theme(strip.text.x = element_text(size = 15, face = "bold"),
                axis.text.x=element_text(size = 16, angle = 30, hjust = 1),
                #text=element_text(family="Times"), 
                axis.text.y = element_blank(), 
                plot.title = element_text(size = 25, hjust = 0.5)) +
          facet_wrap(~`CITY/TWP/CO OF INCIDENT`, scales = "free", ncol = 2) +
          guides(fill ="none") +
          geom_text(data = place, aes(x = `PLACE OF INCIDENT`, y = total, label = total), vjust = .25, size = 5, fontface = "bold")
        
      }
      grid.arrange(p7, p8, ncol = 2, widths = c(1.5, 1.5))
      
    },width = 1280,height=580)
  
  
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)


