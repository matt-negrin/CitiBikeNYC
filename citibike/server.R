library(shiny)
library(shinythemes)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(car)
library(stargazer)
library(grid)
library(RcppEigen)

#load datasets
dfDay <- readRDS("data/dfDay.rds")
weathOnly <- readRDS("data/weathOnly.rds")
nabes <- readRDS("data/nabes.rds")
Palette1 <- readRDS("data/Palette1.rds")
Palette2 <- readRDS("data/Palette2.rds")
dfCity <- readRDS("data/dfCity.rds")
dfCityCom <- readRDS("data/dfCityCom.rds")
dfCityCom8 <- readRDS("data/dfCityCom8.rds")
dfNabe <- readRDS("data/dfNabe.rds")
f1 <- readRDS("data/f1.rds")
f2 <- readRDS("data/f2.rds")
f3 <- readRDS("data/f3.rds")
f4 <- readRDS("data/f4.rds")
f5 <- readRDS("data/f5.rds")
coeffs <- readRDS("data/coeffs.rds")
coeffperc <- readRDS("data/coeffperc.rds")

shinyServer(
        function(input, output) {
                dataInput <- reactive({
                        filter(dfDay, neighborhood==toupper(input$nabe[1]) | 
                                neighborhood==toupper(input$nabe[2]) |
                                neighborhood==toupper(input$nabe[3]) |
                                neighborhood==toupper(input$nabe[4]) |
                                neighborhood==toupper(input$nabe[5]) |
                                neighborhood==toupper(input$nabe[6]) |
                                neighborhood==toupper(input$nabe[7]) |
                                neighborhood==toupper(input$nabe[8]) |
                                neighborhood==toupper(input$nabe[9]) |
                                neighborhood==toupper(input$nabe[10]))
                })
                
                
                output$plot1 <- renderPlot({
                        a <- input$yvalue1
                        b <- ifelse(a=="tempF", 6, ifelse(a=="windSpeed", 8, 4))
                        c <- nth(weathOnly, b)
                        d <- min(c)*.95
                        ggplot(weathOnly, aes_string(x="dateDay", y=input$yvalue1)) + 
                                geom_line(stat="identity", size=1.5, fill="steelblue", colour="steelblue") + xlim(input$dates[1], input$dates[2]) +
                                theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5), axis.text.y=element_text(angle=50, size=14, vjust=0.5), axis.title.y = element_text(angle=90), axis.title.x = element_blank(), legend.position="none")},
                width=800,
                height=155
                )
                
                output$plot2 <- renderPlot({
                                ggplot(dataInput(), aes_string(x="dateDay", y=input$yvalue2, color="neighborhood")) + 
                                        geom_line(size=1.5) + xlim(input$dates[1], input$dates[2]) + 
                                        xlab("Date") + 
                                        theme(axis.text.x=element_text(angle=0, size=14, vjust=0.5), axis.text.y=element_text(angle=50, size=14, vjust=0.5), axis.text.y=element_text(angle=50, size=14, vjust=0.5), axis.title.x = element_blank(), legend.position="top", legend.text.align=0, legend.direction="horizontal", legend.title=element_blank()) + guides(col = guide_legend(nrow = 2))},
                width=800,
                height=395
                )
                

                dataInput10 <- reactive({as.character(input$yvalue3)})

                output$plot3 <- renderPlot({
                        m <- input$yvalue3
                        n <- ifelse(m=="trips", 4, ifelse(m=="tripMin", 6, 7))
                        yval <- nth(dfHr, n)
                        statval <- range(boxplot(yval, plot=FALSE)$stats*c(.8, 1.5))
                        dfHr %>%
                                ggplot(aes_string(x="neighborhood", y=input$yvalue3)) + geom_boxplot(aes(color=neighborhood), outlier.size=NA) + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size=rel(1), angle=30), legend.position="none") + coord_cartesian(ylim = statval) + geom_hline(yintercept=mean(yval), colour="steelblue", size=1)  + geom_hline(yintercept=mean(yval)-sd(yval), colour="red", size=.5)  + geom_hline(yintercept=mean(yval)+sd(yval), colour="red", size=.5)
                })
                
                dataInput2 <- reactive({
                        filter(dfDay, neighborhood==toupper(input$yvalue4[1]) | 
                                       neighborhood==toupper(input$yvalue4[2]) |
                                       neighborhood==toupper(input$yvalue4[3]) |
                                       neighborhood==toupper(input$yvalue4[4]) |
                                       neighborhood==toupper(input$yvalue4[5]) |
                                       neighborhood==toupper(input$yvalue4[6]) |
                                       neighborhood==toupper(input$yvalue4[7]) |
                                       neighborhood==toupper(input$yvalue4[8]) |
                                       neighborhood==toupper(input$yvalue4[9]))
                })
                
                dataInput3 <- reactive({
                        filter(dfDay, neighborhood==toupper(input$yvalue5[1]) | 
                                       neighborhood==toupper(input$yvalue5[2]) |
                                       neighborhood==toupper(input$yvalue5[3]) |
                                       neighborhood==toupper(input$yvalue5[4]) |
                                       neighborhood==toupper(input$yvalue5[5]) |
                                       neighborhood==toupper(input$yvalue5[6]) |
                                       neighborhood==toupper(input$yvalue5[7]) |
                                       neighborhood==toupper(input$yvalue5[8]) |
                                       neighborhood==toupper(input$yvalue5[9]))
                })
                
                output$plot4 <- renderPlot({
                        ggplot(dataInput2(), aes_string(x="tempF", y="trips")) + geom_point(aes(color=factor(weathDesc)), size=3) + stat_smooth(method="lm", colour="blue") + scale_colour_manual(values=Palette1 ) + facet_wrap(~neighborhood, ncol=1) + theme(legend.position="none") + labs(y="Number of Trips per Day", x="Temperature")
                })
                
                output$plot5 <- renderPlot({
                        ggplot(dataInput3(), aes_string(x="tempF", y="trips")) + geom_point(aes(color=factor(weathDesc)), size=3) + stat_smooth(method="lm", colour="blue") + scale_colour_manual(values=Palette1 ) + facet_wrap(~neighborhood, ncol=1) + theme(legend.position="none") + labs(y="",x="Temperature")
                })

                output$plot7 <- renderPlot({
                        ggplot(mornHr, aes(tempF, tripMin)) + geom_point(position = position_jitter(width=.3), aes(color=weathDesc)) + scale_colour_manual(values=Palette2) + ylim(0,30) + stat_smooth(method="lm") + labs(y = "Avg Trip Duration (Minutes)", x = "Temperature (F)")
                })
                
                output$plot8 <- renderPlot({
                        ggplot(mornHr, aes(tempF, tripMin)) + geom_point(position = position_jitter(width=.3), aes(color=neighborhood)) + stat_smooth(method="lm") + ylim(0,40) + facet_wrap( ~ neighborhood) + theme(legend.position="none")
                  })

                output$plot9 <- renderPlot({
                        ggplot(mornHr, aes(tempF, tripMin)) + geom_point(position = position_jitter(width=.3), color="orangered1") + ylim(5,25) + geom_smooth(aes(group=neighborhood, colour=neighborhood), size=1, method="lm", fullrange=TRUE) + xlab("Temperature (F)") + ylab("Trip Duration (Minutes)")
                  })

                output$plot10 <- renderPlot({
                        n <- input$slider1
                        ggplot(dfHr, aes(tripMin)) + geom_histogram(color="red", fill="steelblue", binwidth=n) + xlim(0,30) + ylab("") + xlab("Average Trip Duration per Hour (Minutes)") + geom_density()
                        })

                output$plot11 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot11.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 11"),
                        width = 420,
                        height = 420)
                        }
                        , deleteFile = FALSE)
                
                output$plot12 <- renderPlot({
                        ggplot(weathOnly, aes(x=windSpeed)) + geom_histogram(binwidth=7/32, fill="orangered4",colour="black")  + scale_x_continuous(limits=c(0,7)) + scale_y_continuous(name="", labels = comma)
                        })

                output$plot13 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot13.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 13"),
                        width = 425,
                        height = 425)
                        }
                        , deleteFile = FALSE)  

                output$plot14 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot14.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 14"),
                        width = 425,
                        height = 425)
                        }
                        , deleteFile = FALSE)

                output$plot15 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot15.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 15"),
                        width = 410,
                        height = 410)
                        }
                        , deleteFile = FALSE)

                output$plot16 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot16.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 16"),
                        width = 410,
                        height = 410)
                        }
                        , deleteFile = FALSE)

                output$plot17 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot17.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 17"),
                        width = 420,
                        height = 420)
                        }
                        , deleteFile = FALSE)

                output$plot18 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot18.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 18"),
                        width = 380,
                        height = 380)
                        }
                        , deleteFile = FALSE)    

                output$plot19 <- renderImage({
                        filename <- normalizePath(file.path('images',
                              'plot19.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 19"),
                        width = 420,
                        height = 420)
                        }
                        , deleteFile = FALSE)

                output$plot20 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot20.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 20"),
                        width = 410,
                        height = 410)
                        }
                        , deleteFile = FALSE)

                output$plot21 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot21.jpeg'))
                        list(src = filename,
                        alt = paste("Plot 21"),
                        width = 410,
                        height = 410)
                        }
                        , deleteFile = FALSE)      

                output$plot22 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'image1.png'))
                        list(src = filename,
                        alt = paste("Image 1"),
                        width = 500,
                        height = 500)
                        }
                        , deleteFile = FALSE)

                output$plot23 <- renderPlot({
                        ggplot(dfCity, aes(hr, trips)) + geom_point(aes(color=startWday), size=4) + geom_line(x=dfCity$hr, y=dfCity$tripsWday, alpha=.4, size=1.3) + geom_line(x=dfCity$hr, y=dfCity$tripsWknd, alpha=.4, size=1.3)  + labs(x="Hour of Day", y="Number of Trips Taken per Hour", colour="Day of Week") + theme(axis.ticks.x=element_line(size=.5)) + scale_x_continuous(breaks=0:23, limits=c(0,23))
                        })

                output$plot24 <- renderPlot({
                        ggplot(dfCityCom, aes(hr, trips)) + geom_point(aes(color=startWday), size=4) + geom_line(x=dfCityCom$hr, y=dfCityCom$tripsWdayV2, alpha=.4, size=1.3) + labs(x="Hour of Day", y="Number of Trips Taken per Hour", colour="Day of Week") + theme(axis.ticks.x=element_line(size=.5)) + scale_x_continuous(breaks=0:23, limits=c(0,23))
                        })

                output$plot25 <- renderPlot({
                    dfCityCom %>%
                    filter(hour(dateTime) == input$hourSlide) %>% 
                    ggplot(aes(tempF, trips)) + geom_point(aes(colour=humidity*10, shape=weathDesc), size=5) + scale_colour_gradient(low = "gray", high = "red", space = "Lab", guide = "colourbar") + labs(y="Number of Trips Taken", x="Temperature (F)", shape="Rain (Binary)", colour="Humidity")
                    })

                output$modeltable <- renderUI({
                    HTML(stargazer(f1, f2, f3, f4, f5, type="html", style="aer", covariate.labels=c("Intercept", "Temperature (F)", "Wind Speed", "Humidity", "Rain (Dummy)"), dep.var.labels="Estimated Number of Trips Taken per Hour", model.numbers=TRUE, ci=FALSE, df=FALSE, intercept.bottom=FALSE))
                    })

                output$plot26 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot26.jpeg'))
                        list(src = filename,
                        alt = paste("plot26"),
                        width = 400,
                        height = 400)
                    }
                        , deleteFile = FALSE)

                output$plot26a <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot26a.jpeg'))
                        list(src = filename,
                        alt = paste("plot26a"),
                        width = 400,
                        height = 400)
                        }
                        , deleteFile = FALSE)

                output$plot27 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot27.jpeg'))
                        list(src = filename,
                        alt = paste("plot27"),
                        width = 400,
                        height = 400)
                        }
                        , deleteFile = FALSE)

                output$plot27a <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot27a.jpeg'))
                        list(src = filename,
                        alt = paste("plot27a"),
                        width = 400,
                        height = 400)
                        }
                        , deleteFile = FALSE)

                output$plot28 <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot28.jpeg'))
                        list(src = filename,
                        alt = paste("plot28"),
                        width = 400,
                        height = 400)
                        }
                        , deleteFile = FALSE)

                output$plot28a <- renderImage({
                        filename <- normalizePath(file.path('images/',
                              'plot28a.jpeg'))
                        list(src = filename,
                        alt = paste("plot28a"),
                        width = 400,
                        height = 400)
                        }
                        , deleteFile = FALSE)


        })