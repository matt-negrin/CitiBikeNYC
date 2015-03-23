library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(car)
library(stargazer)
library(lubridate)
library(grid)
library(RcppEigen)

#load datasets
nabes <- readRDS("data/nabes.rds")

# ui.R
shinyUI(navbarPage(theme=shinytheme("flatly"), "Citibiking NYC",
        tabPanel("Background",
        fluidPage(
            fluidRow(
                column(width=8, offset=2,
                h1("Project Updates", align="center"),
                hr(),
                h3("Second Post", align="left"),
                h5(tags$em("2/22/2015")), 
                p("Since my last post, I have pumped three additional months of data into my analysis. The dataset now spans July, August, September, and October 2014. All of the charts on the \"Visualizations\" tab have been updated accordingly. I recommend playing around with the \"Interactive Over Time\" chart; the newly added data makes searching for Citibike and weather trends over time far more interesting. Take advantage of the date widget to narrow in on different time periods that seem intriguing to you."),
                p("The \"Analysis\" tab has also been completely overhauled with a new predictive regression model and a section on neighborhood impact. For background on motivation and reasoning for the study, see my first post below. Thank you for checking out my page!"),
                p("All analysis posted here is done in R and can be viewed on Github. This site was built using", tags$span(tags$a("Shiny", href="http://shiny.rstudio.com/", target="_blank")),"."),
                tags$em("Matt"),
                hr(),
                h3("First Post", align="left"),
                h5(tags$em("2/16/2015")), 
                p("Since beginning my Citibike subscription in June of 2013, I have taken about 100 Citibike trips, mostly around the East Village. (If you're a subscriber, you can log in to the Citibike website to see a list of each of your trips). Fortunately for someone who enjoys working with data, Citibike makes their usage history easily accessible. A quick Google search will yield scores of great analyses on Citibike usage patterns around NYC. I take a unique perspective on these studies and examine each NYC neighborhood's relationship with Citibike more closely."),
                p("Why do I focus on neighborhoods? Like many other early-on subscribers, I have experienced the frustration of packed stations during peak \'return\' times as well as empty stations during peak \'departure\' times. I have always been intrigued by the idea that I am somehow disadvantaged against using Citibike during certain times of the day because my \'home\' stations are located in the East Village compared to my friends who live in Midtown East or Midtown West."),
                p("While Citibike has improved its rebalancing efforts significantly over the past 2 years, Citibike usage still seems to ebb and flow towards different neighborhoods at various points throughout the day and on various days throughout the month. I pose that NYC neighborhoods have specific attributes about them, be it physical location, general demographic of user, spending patterns, etc. that are relatively systematic. Ultimately, I seek to understand if the residents of each NYC neighborhood react to environmental factors in methodically similar ways that influence how their neighborhood, on the whole, utilizes Citibike."),
                p("To examine these claims, I will begin by studying the impact of weather on bike usage."),
                p("All analysis posted here is done in R and can be viewed on Github. This site was built using", tags$span(tags$a("Shiny", href="http://shiny.rstudio.com/", target="_blank")),"."),
                tags$em("Matt"),
                hr()
            )
        ),
        br(),
        br(),
        br()
        )),
        tabPanel("The Data",
        fluidPage(column(width=9, offset=1,
        tabsetPanel(type = "pills",
            tabPanel("Sources",
            hr(),
            fluidRow(
                column(width=5, offset=0,
                h3("Citibike Data", align="left"),
                h5(tags$a("http://www.citibikenyc.com/system-data", href="http://www.citibikenyc.com/system-data", target="_blank")),
                p("Citibike has two great datasets to work with. The first contains historical trip data and the second is a live station feed. One of my first data projects used the live feed to project the flow of citibike station capacity onto a map of Manhattan for a 24-hour period in July 2014. You can see the video in the \"Time Lapse\" tab above."),
                p("For this project, I am using historical bike usage data, made available in compiled CSV files. Each CSV contains records of every Citibike trip taken in a given month. Each line is a different trip (point A to point B) and contains the following information:"),
                tags$ul(
                tags$li("Trip Duration (Seconds)"),
                tags$li("Start Time & Date"),
                tags$li("Stop Time & Date"),
                tags$li("Start Station Name"),
                tags$li("End Station Name"),
                tags$li("Station ID"),
                tags$li("Station Latitude & Longitude"),
                tags$li("Bike ID"),
                tags$li("User Type (Customer = 24-hour pass or 7-day pass user; Subscriber = Annual Member)"),
                tags$li("Gender (0 = Unknown; 1 = Male; 2 = Female)"),
                tags$li("Year of Birth")
                ),
                p("My current analysis looks at trips from July through October 2014.")
                ),
                column(width=5, offset=1,
                h3("Weather Data", align="left"),
                h5(tags$a("http://weatherspark.com/download/documentation", href="http://weatherspark.com/download/documentation", target="_blank")),
                p("I spent a good deal of time searching for quality weather data for NYC. In doing so, I have learned a lot about how weather data is stored in the US, specifically in NY. For NYC, the local weather centers that store accurate historical data are JFK, LaGuardia, Newark, and Central Park."), 
                p("Initially, I used a free data set for weather (an R package called weatherData). Unfortunately, while the package was great in terms of ease-of-use, weatherData only provides daily weather data. After playing with the daily data, I decided that daily weather summaries would not be granular enough for my analysis. My personal decision to ride a Citibike tends to be relatively impulsive and is definitely more impacted by the weather at the moment than the summary for the day."),
                p("I found a great service, Weather Spark, that offers hourly weather data for the Central Park weather station. Through Weather Spark, I was able to purchase a year's (2014) worth of data for $10. After purchasing, Weather Spark emailed me a CSV file with hourly data. From the data sent to me, I took out the following:"),
                tags$ul(
                tags$li("Date & Time"),
                tags$li("Cloud Coverage"),
                tags$li("Dew Point"),
                tags$li("Humidity Fraction"),
                tags$li("Precipitation Amount"),
                tags$li("Snow Depth"),
                tags$li("Temperature"),
                tags$li("Visibility"),
                tags$li("Weather Description"),
                tags$li("Wind Gust"),
                tags$li("Wind Speed")
                ),
                br(),
                br(),
                br(),
                br(),
                br()    
                )
            )),
            tabPanel("Cleaning and Addition",
                hr(),
                fluidPage(
                fluidRow(
                h3("Manipulation", align="left"),
                p("From the Citibike dataset, some manipulation was necessary to generate important data items. In addition to reformatting various data types, I also created new, useful metrics from the dataset provided. The scripts I wrote are posted on Github (clean1.R, clean2.R, and clean4.R). After all manipulation, I have the following data items to work with:"),
                fluidRow(
                    column(3,
                tags$ul(
                tags$li("Start Zip Code"),
                tags$li("End Zip Code"),
                tags$li("Start Neighborhood"),
                tags$li("Bike ID"),
                tags$li("Gender"),
                tags$li("Age"),
                tags$li("Trip ID"),
                tags$li("Start Address"))),
                    column(4,
                tags$ul(
                tags$li("End Address"),
                tags$li("Start Time"),
                tags$li("End Time"),
                tags$li("Trip Duration (Seconds)"),
                tags$li("Trip Distance (Miles)"),
                tags$li("Starting Latitude & Longitude"),
                tags$li("Ending Latitude & Longitude"),
                tags$li("Temperature (F)"))),
                    column(3,
                tags$ul(
                tags$li("Cloud Cover"),
                tags$li("Dew Point"),
                tags$li("Humidity"),
                tags$li("Precipitation"),
                tags$li("Snow"),
                tags$li("Windspeed"),
                tags$li("Weather Description")))
                ),
                h3("Generating Street Addresses", align="left"),
                p("I wanted to have the street address for each Citibike station so that I could extract zip codes, which I anticipated would be a good method of determining neighborhood designations. I used a Google package called 'ggmap' that can generate street addresses off of latitude and longitude coordinates. The ggmap package limits casual users to 2500 API requests per 24-hour period, so running a loop through the ~1 million trip start and end location points was not a viable option. To circumvent the issue, I created a look-up table of Citibike station addresses based off of the unique latitude-longitude combinations in the start-of-trip-address/ end-of-trip-address. Once I had a lookup table, I could join that data back into my primary dataset."),
                h3("Generating Trip Distanace", align="left"),
                p("While Citibike does offer the time duration of the trip, one thing they do not collect is actual distance travelled. While we do know where the bike started, where the bike ended, and how long it took to get there, we do not know what path the bike took. As a proxy, I used the 'Imap' R package to generate the distance (in miles) between two sets of coordinates. Similar to how I treated street addresses, I created a lookup table to shorten total calculation time. While straight-line distance between two points is not a perfect metric for distance travelled (it ignores citibike users that took out bikes and returned to their start station), it is a decent proxy for distance travelled in scenarios where the user is going to a specific destination (i.e. commuting)."),
                h3("Dataset Limitations", align="left"),
                p("The raw Citibike dataset contains 3,709,206 observations, or about 3.75 million trips in the months of July-October 2014. For my analysis, I narrowed down the dataset, both as a means of optimizing my ability to use the demographic data available to me and as a means of lessening potential exogeneity. Ultimately, my focus is the NYC neighborhood. While looking at tourist Citibike behavior would be fascinating, here I intend to examine the residents of NYC and their daily habits. For that reason, I limit my dataset to include only 'Subscribers' that also had a listed gender and a listed age below 80 years (limiting the age below 80 is specifically intended to reduce response bias)."),
                br(),
                br(),
                br()
                )
                )
            ),
                tabPanel("Drawing a Map",
                hr(),
                fluidPage(fluidRow(
                h3("What is a NYC Neighborhood?", align="left"),
                p("Creating proper and clearly defined NYC neighborhoods ended up being a more difficult task than I anticipated. As I began the process, I realized the importance that these neighborhood divisions could have in my analysis, as my stated goal is to determine behavioral similarities within a given neighborhood compared to its counterparts. Therefore, where I (subjectively) decide to draw boundary lines has the potential to result in relatively severe influences on my outcomes."),
                br(),
                fluidRow(
                    imageOutput("plot22"), align="center"),
                    br(), 
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    p("As any New Yorker can attest to, the potential degree of granularity when determining a Manhattan neighborhood is actually quite high. For example, a", span(a("Wikipedia page",href="http://en.wikipedia.org/wiki/List_of_Manhattan_neighborhoods",target="_blank")), "dedicated to NYC neighborhoods guides me to four broad categories: Uptown Manhattan, Downtown Manhattan, Midtown Manhattan, and West Side. Since Citibike does not currently exist north of 60th street, it was easy to begin by crossing out all of Uptown Manhattan. From there, Wikipedia lists nearly 60 sub-neighborhoods that comprise the remaining three categories. This list was clearly too broad."),
                    p("I then tested out boundaries based on zip code, as this was a measurable characteristic of every Citibike station and a consistent method for grouping addresses into different categories. However, when I pulled the list of unique zip codes out of the addresses, I found a startling 33 different zip codes in NYC that contain a Citibike station in the Island of Manhattan and nearby Brooklyn. At this point, I determined that my knowledge of NYC was sound enough to make educated, objective groups of zip codes as a means of creating 10 distinct neighborhoods. Using this method, I generated the list of neighborhoods below. I am sure that these could be divided in a more efficient fashion and that I am insulting some New Yorkers for lumping their neighborhood in with others; however, for right now, this list will suffice."),
                br(),
                br(),
                br(),
                br(),
                br()
                )))
        )))),        
        tabPanel("Visualizations",
        fluidPage(fluidRow(column(width=8, offset=1,
        tabsetPanel(type = "pills",
        tabPanel("Comparisons",
            hr(),
            p("Take a look at how certain neighborhoods may react differently to the rain relative to the other neighborhoods. Here, I notice that the East Village's relative bike usage shoots up when it rains. Comparatively, Midtown West usage seems to shrink during rainy hours. Perhaps this implies that the East Village is more immune to the impact of rain than Midtown West."),
            fluidRow(
                column(6, imageOutput("plot13")),
                column(6, imageOutput("plot14"))),
            br(),
            br(),
            p("Here we compare the number of trips taken throughout the period in humidity buckets to the number of hours in the period in humidity buckets. For the most part, the number of trips seem to be consistent with the trend in humidity. However, what is interesting is the large spike in the number of hours in the period with a high humidity percentage. Within that humidity bucket, we don't see a similar number of trips being taken. This may insinuate that humidity could impact whether or not Citibike users take out bikes."),
            fluidRow(
                column(6, imageOutput("plot15")),
                column(6, imageOutput("plot20"))),
            br(),
            br(),
            br(),
            br(),
            p("Similar to the charts above, the chart on the left displays the number of trips taken in the period broken into wind speed buckets, and the chart on the right displays the number of hours in the period broken into windspeed buckets."),
            fluidRow(
                column(6, imageOutput("plot16")),
                column(6, imageOutput("plot21"))),
            br(),
            br(),
            br()
            ),
        tabPanel("Interactive Over Time",
            hr(),
            fluidRow(column(12,
            p("Use the interactive charts below to spot trends in neighborhood citibike usage alongside weather patterns for the day. Use the date widget to adjust the x-axis. (Date range is currently limited to July through October, 2014)."),
            dateRangeInput("dates", label = "", start="07-01-2014", end="10-30-2014", format="mm-dd-yyyy"),
            sidebarLayout(
                sidebarPanel(
                        selectInput("yvalue2", label = em(h5("Citibike Measure (Top Chart):")), 
                                    choices = list("Number of Trips" = "trips", "Avg Trip Duration (Minutes)" = "tripMin", "Total Trip Distance (Miles)" = "totDist"), 
                                    selected = "trips"),
                        checkboxGroupInput('nabe', em(h5('Neighborhoods to Chart:')), nabes, selected=nabes[1]),
                        selectInput("yvalue1", label = em(h5("Weather (Bottom Chart):")), 
                                    choices = list("Temperature (F)"="tempF", "Wind Speed"="windSpeed", "Precipitation"="precip"), 
                                    selected = "TempF")
                ),
                mainPanel(
                        plotOutput("plot2", height = 415, width = 900),
                        plotOutput("plot1", heigh= 165, width = 900))
                ))
        )),
        tabPanel("Interactive Scatterplot",
            hr(), 
            p("Compare the relationship between temperature (in Fahrenheit) and the number of trips taken per hour. A 'loess' line is overlayed on each chart to give an idea of the trend."),
            fluidRow(
            column(6,selectInput("yvalue4","", 
                    choices = nabes, 
                    selected = nabes[1], width="100%"),
               plotOutput("plot4")
            ),
            column(6,selectInput("yvalue5", label = "", 
                    choices = nabes, 
                    selected = nabes[2], width="100%"),
               plotOutput("plot5")
            )),
            br(),
            br(),
            br(),
            br(),
            br()
            ))))))
        ,
                tabPanel("Analysis",
            fluidPage(column(width=9, offset=1,
            tabsetPanel(type = "pills",
            tabPanel("The Impact of Time",
                hr(),
                fluidPage(
                p("Before considering the influence of weather factors on the number of Citibike trips NYC residents are taking, it is important to consider the impact that the time of day alone has on the number of trips taken. The reasoning here is straightforward. Let's assume that people are more likely to take out a Citibike in the afternoon than they are in the morning. Afternoons will also tend to have higher temperatures than the morning. Because both Citibike usage and temperature are correlated with time of day, it would be difficult to attribute behavior to one factor or the other."),
                p("Before making any methodology decisions based on the above assumptions, I charted out all of the hourly-aggregated trips taken in the four month period (July-October, 2014). The below chart plots the hour of the day on the X-axis (i.e. \"15\" = \"3:00PM\") and the corresponding number of trips taken in that given hour on the Y-axis."),
                p("To highlight the trends I noticed, I added in two lines: one for the average workday number of trips per hour and one for the average weekend/holiday number of trips per hour."),
                br(),
                br(),
                fluidRow(plotOutput("plot23")),
                br(),
                br(),
                p("As anticipated, there seems to be a clear trend for how many Citibike trips are taken in NYC in a given hour of the day. To add conciseness to our analysis, I removed all weekend/holidays and focus just on the workday. This should prevent any exogeneity that would come from subscribers' systematic behavior changes based on type of day (workday vs. holiday/weekend)."),
                br(),
                br(),
                fluidRow(plotOutput("plot24")),
                br(),
                br(),
                p("From the chart above, I determine that the best way to avoid the influence of \"hour of day\" is to only examine the impact of weather factors within a given hour. While doing so does not necessariy offer the full picture of how weather factors affect the number of trips taken at any given hour in NYC, it does successfully isolate the decision-making of the subscribers; we are left with an intriguingly wide variation in number of trips that is unexplained by time of day."), 
                p("Looking at 8PM, for example, why is the variation so large on a typical workday? What else, other than time of day, could help us predict these outcomes? I seek to address these questions."),
                br(),
                br(),

                br(),
                br(),
                br()
                )),
            tabPanel("Regression Analysis",
                hr(),
                fluidPage(
                p("Given that we will be performing intra-hour analysis (as determined in the previous tab), we can use the slider bar below to compare the variation in number of trips taken in any given hour to the variation in temperature that existed in that hour. I added in colors and icons to give additional factor dimensions (humidity and rain, respectively) to the chart. Note that rain is included as a binary variable (i.e. the only possible outcomes are 'raining' or 'clear')."),
                p("Take a look at which hour of the day you would find most interesting to study. Keep in mind that the axes scale dynamically as you adjust each hour. Some relationships may appear stronger than they actually are."),
                br(),
                fluidRow(column(8, offset=3, sliderInput("hourSlide", label = h5("Hour of Day", align="left"), min = 0, 
                    max = 23, value = 8))),
                fluidRow(column(8, offset=2, plotOutput("plot25"))),
                br(),
                br(),
                p("As I scroll through each hour, I am intrigued by the morning time frames. While most hours show a moderately positive relationship between temperature and number of trips taken, the morning commute hours contain a nice range of temperatures. Additionally, looking ahead to my neighborhood analysis, I assume that the starting bike location of a morning trip is most likely that user's home neighborhood. For the model, I go ahead with 8AM - 9AM."),
                p("I run a standard linear regression analysis using a selection from the weather factors I have available to me: temperature, wind speed, humidity, and rain. I quickly run a VIF test to check for collinearity among the explanatory variables. Without running this test, I run the risk of misappropriating sources of variation to similarly correlated inputs. Fortunately, none of the values resulting from the VIF are greater than 2, implying that the explanatory variables are not collineated and that I can continue with the regression."),
                br(),
                fluidRow(tags$code("> vif(fit)")),
                tags$table(style="width:50%",
                tags$tr(
                    tags$th(tags$code("Temp")),
                    tags$th(tags$code("Wind Speed")),
                    tags$th(tags$code("Humidity")),
                    tags$th(tags$code("Rain"))),
                tags$tr(
                    tags$td(tags$code("1.178733")),
                    tags$td(tags$code("1.252637")),
                    tags$td(tags$code("1.228104")),
                    tags$td(tags$code("1.393645")))
                ),
                br(),
                br(),
                p("Take a look at the regression output below; the outcome variable is the number of trips taken between 8AM and 9AM on a given day. I include different combinations of factors to reveal the marginal influence of each factor. Ultimately the best fit model is the model with all four factors included, with an R^2 of .59, an adjusted R^2 of .57, and an overall significance at the 1 percent level. In other words, about 64% of the variation in the number of trips taken at 7PM can be explained by these four weather factors."),
                br(),
                fluidRow(column(9, offset=3, uiOutput("modeltable"))),
                br(),
                p("Our model demonstrates significance for two of the four explanatory variables (Temperature and Wind Speed are not significant) at minimally the 5 percent level. From the output above, we derive the following formula to represent our model:"),
                fluidRow(column(11,offset=1, tags$code("y = 2351 - 0.4*[temperature] - 15*[wind speed] - 408*[humidity] - 1301*[rain] + e"))),
                br(),
                p("Model Interpretations:"),
                tags$ul(
                tags$li(em("Between 8AM and 9AM in NYC, we anticipate male subscribers to take approximatley 408 less Citibike trips when the relative humidity percentage is greater than 80% than when the relative humidity percentage is less than 80%.")),
                tags$li(em("Between 8AM and 9AM in NYC, we anticipate male subscribers to take approximately 1301 less Citibike trips when the weather is \"raining\" than when the weather is \"clear\" in a given hour."))),
                br(),
                br(),
                br(),
                br()
                )),
            tabPanel("Neighborhood Impact",
                hr(),
                fluidPage(
                p("Let's take another look at our linear model we can use to predict the number of trips taken in NYC between 8AM and 9AM based on weather conditions:"),
                br(),
                fluidRow(column(11,offset=1, tags$code("y = 2351 - 0.4*[temperature] - 15*[wind speed] - 408*[humidity] - 1301*[rain] + e"))), 
                br(),
                p("I would like to see if NYC neighborhoods, when segmented out, respond similarly to our set of weather factors during morning commute hours. Are certain neighborhoods driving the model coefficients relative to other neighborhoods? Are certain neighborhoods more immune (or more reactive) to weather factors relative to their peers?"),
                p("To answer the above, I grouped out the hourly trips into 10 unique neighborhoods (see \"The Data\" tab for neighborhood zoning methodology). Then, I ran our linear model for each neighborhood's unique set of data, resulting in 10 unique neighborhood models that predict the number of trips taken between 8AM and 9AM in each given neighborhood. For consistency, I used the same weather factors as we used in the NYC model."),
                p("The bar charts below illustrate how three of our our weather factors faired in each neighborhood. The charts on the left are \"absolute\" charts - these show the actual coefficients from each neighborhood's model. The charts on the right are \"percent impact\" charts - these divide the coefficient for that particular weather factor by the mean number of trips taken in that neighborhood throughout the hour. The percent impact charts allow the model coefficients to be compared across neighborhoods."), 
                fluidRow(
                    column(4, offset=0, imageOutput("plot26a")),
                    column(4, offset=2, imageOutput("plot26"))
                    ),
                p("To highlight an example, let's look at the Chelsea/ Flatiron model above. When the relative humidity percentage is greater than 80% between 8AM and 9AM, we predict that about 36 less Citibike trips will be taken in Chelsea/ Flatiron (than when the relative humidity is less than 80%). In the Brooklyn model, we can infer that only about 20 less Citibike trips will be taken when relative humidity is greater than 80%. When using the percent impact charts, however, we see that these coefficients are in fact relatively similar in terms of degree of influence. Comparing the two neighborhoods, we would actually anticipate a high-humidity environment to have a greater impact on Brooklyn's trip-taking behavior than Chelsea's."),
                p("Without any reference point, the neighorhood coefficients are difficult to compare.  By taking into account the number of trips that we would expect Brooklyn or Chelsea/ Flatiron to take at 7PM (the mean number of trips), we can infer to what extent these coefficients influence a neighborhood's trip-taking behavior."),
                p("As an important note, the percent impact charts let you compare values across neighborhoods, but they do not necessarily imply that one factor is more effective than another in terms of influencing trip-taking behavior."),
                fluidRow(
                    column(4, offset=0, imageOutput("plot27a")),
                    column(4, offset=2, imageOutput("plot27"))
                    ),
                p("As we saw in the city-wide model, temperature does not appear to have much of an impact on whether or not subscribers use Citibike in the morning. Though perhaps a bit disappointing, this outcome is easy to rationalize. For the duration of our four month period, the temperature at 8AM ranged from about 40 to 80 degrees. The subscribers who ride Citibike every morning to work are likely in a set routine and are fairly resilient to temperature changes at this scale. I would anticipate that temperature may have a greater impact in the afternoon (we can see a mildly positive relationship between temperature and number of trips at 7PM, for example, using the scatterplot on the \"Regression Analysis\" tab). I would also anticipate that temperature would have a greater impact if our sample dataset included winter months."),
                br(),                
                fluidRow(
                    column(4, offset=0, imageOutput("plot28a")),
                    column(4, offset=2, imageOutput("plot28"))
                    ),
                p("In my opinion, the impact of rain yielded some of the most interesting results in the neighborhood analysis. First of all, the absolute impact of rain on Citibike behavior is significant. Some neighborhoods would anticipate over 100 less trips taken on a given rainy morning than on a clear one. Keep in mind, these numbers exclude trips taken by females. At face value, this appears obvious. However, these numbers tell us that Citibike riders are changing their commuting behavior in a real and significant way when it rains. How are these subscribers commuting instead of riding Citibike? Our current dataset doesn't tell us this, but we can assume it would include some mix of subway, bus, and taxi."),
                p("With this perspective, examining the percent impact of rain on each neighborhood becomes more interesting. Relatively speaking, which neighborhoods are most reactive to rain during the morning commute? According to our model, Brooklyn, Greenwich & West Village, the Lower East Side, and the East Village seem to be the least impacted by a rainy day. In other words, in these neighborhoods, Citibike subscribers are more likley to deal with rainy conditions and ride a bicycle on a given morning if it's raining outside compared to Citibike subscribers who live in Gramercy, Murray Hill, TriBeCa, or Midtown. Follow up analysis should explore potential reasons behind these variations."),
                br(),
                br(),
                br(),
                br(),
                br()
                )),
            tabPanel("Takeaways",
                fluidPage(
                br(),
                br(),
                column(12, offset=0, p(strong(em("What did we learn?")))),
                p("A portion of NYC residents subscribe to Citibike and use their subscription to commute to and from work each day. There is a very clear relationship between hour of the day and number of Citibike trips taken in NYC. Citibike appears to have interwoven itself into the daily commuting routines of some of its subscribers."),    
                p("After removing potential sources of exogeneity, our city-wide regression model demonstrated evidence, at a statistically significant level, that there is a strong, negative influence of both rain and humidity on the number of Citibike trips taken by males between 8AM and 9AM on workdays."),
                p("Applying our linear model to neighborhood subsets, we see that neighborhoods react relatively similarly to two significant weather factors: humidity and rain. However, we do notice that certain neighborhoods appear to be more \"resilient\" than others to these weather factors. For example, the East Village did not reduce its trip numbers during intensely humid hours to the same degree that other neighborhoods did. Additionally, rain appeared to impact Citibike behavior in Gramercy/ Murray Hill and TriBeCa more than in Brooklyn and the LES."),
                p("Do these models offer conclusive evidence? No; the neighborhood models' fits are actually relatively low compared to the city-wide model. However, the neighborhood anlaysis does present interesting findings for follow-up analysis."),
                br(),
                column(12, offset=0, p(strong(em("What did this analysis do well?")))),
                p("I think that this project makes two unique contirbutions in particular to the wealth of Citibike analysis already being done:"),    
                tags$ul(
                tags$li("Incorporating hourly weather data (instead of daily weather data, as seems to be commonly used in other Citibike studies) introduces a more granular and realistic perspective on the impact that weather has on how subscribers use Citibike. Being a Citibike user myself, my decision whether or not to use the service is often an impulsive one. In the context of commuting, I would assume one cares more about whether it is raining at that very moment rather than the forecast for the day, for example. Utilizing hourly data, however, quickly introduced issues with how Citibike usage trends throughout the day. By acknowleding and sourcing this trend, I successfully limited its influence by examining intra-hour data only."),
                tags$li("A second unique contribution is the concept of neighborhood-specific behaviors in relation to Citibike. I think that my treatment of the neighborhood zoning (through zip codes) was an interesting but ultimately flawed method. Nonetheless, I think that there are neighborhood-specific stories yet to be discovered. It would be fascinating to include additional neighborhood attributes into visual and predictive analyses, such as mean income, household type, proximity to public transportation, demographic, etc.")),
                br(),
                column(12, offset=0, p(strong(em("What are some issues with the analysis?")))),
                p("There are a number of assumptions that I made throughout all parts of the analysis that could have influenced my end conclusions. I will list a few of the more significant ones here:"),
                tags$ul(
                tags$li("I narrowed down the population dataset in a few important ways, mostly as a means of ensuring clean data and limiting exogeneity. To be specific, I removed all trips taken by users that were not subscribers, above the age of 80, and female. For the age limitation, I noticed a small set of unrealistic ages (i.e. 150) that I wanted to remove. Additionally, this parameter removed all subscribers who did not list an age when signing up. For the gender limitation, I included female trips in the \"Visualizations\" tab, however discluded them for the linear regression. I figured that using a more homogenous population would yield more reliable results."),
                tags$li("Though my neighborhood zoning methodology is unique, I think there needs to be more granularity in the number of neighborhoods included as well as more thoughtful boundary lines. Certain neighborhoods spanned far larger portions of land than others. Additionally, my desire to limit the number of neighborhoods I was working with early on in my analysis led to some odd combinations (i.e. Chelsea/ Flatiron). My choices here could have had potentially massive impacts on my end conclusions. A more intelligently designed method would cluster zones based on characteristics other than zip code, such as access to public transportation and demographic."),
                tags$li("I included both rain and humidity as binary variables as a means of overcoming non-linear relationships with the outcome variable (number of trips). While I do not think that this is a bad workaround per say, there are certainly more sophisticated methods of handling non-linear relationships in the regression model that may be more appropriate for this dataset. These are changes I hope to make in future iterations of the analysis.")),    
                br(),
                column(12, offset=0, p(strong(em("What next?")))),
                p("With the conclusions drawn here as a foundation, the next steps in my analysis would examine other hours of the day outside of 8AM-9AM. As we saw in the \"Impact of Time\" tab, the variation within each hour of the day can vary significantly. I am interested in seeing which weather factors play a role in explaining those variations."),
                p("Furthermore, while my predictive model is relatively reliable (particularly at the city-wide level), it is clear that there are other factors at work that influence Citibike behavior. Sourcing and including more independent explanatory variables in the model is important to improving its usefulness."),
                p("At the neighborhood level, I would be particularly interested in comparing behavior between hours. For example, do residents living in neighborhoods with heavier 20-something populations use Citibike more heavily between 12AM and 3AM than residents living in neighborhoods with 40-something populations?"),
                p("Finally, while adding in 3 additional months of data to my analysis was crucial to building a successful model, more data would be preferrable. Including even greater variations in temperatures, rain days, and even snow would add more reliability to the predictiveness of the model."),    
                br(),
                br()
                ))
        )))),
        tabPanel("Time Lapse",
        fluidPage(
        fluidRow(
        column(width=8, offset=2,
            h1("Flow of Citibikes", align="center"),
        hr(),
        column(12, offset=0, p("The video below projects the flow of Citibike station capacity onto a map of Manhattan for a 24-hour period (July 9, 2014) using Citibike's live station feed. Each circle is a different Citibike station. The size of the circle implies how 'full' the station is, or what percentage of its docks are unavailable. If the dock is completely full, the station circle will turn red. The time lapse begins at midnight. You'll notice a flury of movement during rush hour as residents move between predominantly residential neighborhoods and predominantly commercial neighborhoods.")),
        br(),
        column(8, offset=0,tags$iframe(width="800", height="450", src="https://www.youtube.com/embed/QxiTnqGxnZg",
                            frameborder="0", allowfullscreen="true")))),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br()
        ))
))