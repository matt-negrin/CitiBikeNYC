#FINAL CLEAN FILE. PURPOSE IS TO CREATE DATASETS, MODELS, AND IMAGES TO BE USED ON THE SITE. SAVE ALL OBJECTS AS RDS FILES IN /DATA/ SUBDIRECTORY. ALL IMAGES (STATIC CHARTS) WILL SIT IN /IMAGES/ SUBDIRECTORY.

##create datasets

cleanWeath <- mutate(cleanWeath, humidity=10*humidity)
finJul <- mutate(finJul, humidity = 10*humidity)

#Creating weather-nly dataset for over time chart
weathOnly <- group_by(cleanWeath, dateDay)
weathOnly <- summarize(weathOnly, dew=mean(dew, na.rm=TRUE), humidity=mean(humidity, na.rm=TRUE), precip=mean(precip, na.rm=TRUE), snow=mean(snow, na.rm=TRUE), tempF=mean(tempF, na.rm=TRUE), visib=mean(visib, na.rm=TRUE), windSpeed=mean(windSpeed, na.rm=TRUE))
weathOnly <- mutate(weathOnly, windSpeed = ifelse(is.na(windSpeed),0,windSpeed))

##getting rid of bad data first. primary dataset to work off of now becomes df
df <- finJul        %>%
        filter(userType == "Subscriber") %>% 
        filter(age < 80) %>%
        filter(gender=="male") %>%
        filter(neighborhood != "NEW JERSEY") %>%
        mutate(weathDesc = ifelse(weathFlag==0,"clear", 
                        ifelse(weathFlag==1,"clear",
                        ifelse(weathFlag==2,"clear",
                        ifelse(weathFlag==3,"rain",
                        ifelse(weathFlag==4,"rain",
                        ifelse(weathFlag==5,"rain","clear"))))))) %>%
        mutate(weathDesc = as.factor(weathDesc))


#group df into just hourly buckets (no neighborhoods)
dfCity <- group_by(df, dateTime)
dfCity <- summarise(dfCity, hr=mean(hr, na.rm=TRUE), age=mean(age, na.rm=TRUE), tripMin=mean(tripMin, na.rm=TRUE), tripDist=mean(tripDist, na.rm=TRUE), trips=n(), cloudCover=mean(cloudCover, na.rm=TRUE), dew=mean(dew, na.rm=TRUE), humidity=mean(humidity, na.rm=TRUE), precip=mean(precip, na.rm=TRUE), tempF=mean(tempF, na.rm=TRUE), visib=mean(visib, na.rm=TRUE), windSpeed=mean(windSpeed, na.rm=TRUE), weathFlag=median(weathFlag, na.rm=TRUE))

#adding back in factor variables
dfCity <- dfCity %>%
        mutate(weathDesc = ifelse(weathFlag==0,"clear", 
                        ifelse(weathFlag==1,"clear",
                        ifelse(weathFlag==2,"clear",
                        ifelse(weathFlag==3,"rain",
                        ifelse(weathFlag==4,"rain",
                        ifelse(weathFlag==5,"rain","clear"))))))) %>%
        mutate(weathDesc = as.factor(weathDesc)) %>%
	mutate(startWday = wday(dateTime, label=TRUE)) %>%
	mutate(startNday = day(dateTime))

#creating average lines for graph to show trip trends on weekends v weekdays
dfCityWday <- dfCity %>%
		filter(startWday=="Mon" | startWday=="Tues" | startWday=="Wed" | startWday=="Thurs" | startWday=="Fri") %>%
		group_by(hr)
dfCityWday <- summarise(dfCityWday, tripsWday=mean(trips))
dfCityWknd <- dfCity %>% 
		filter(startWday=="Sat" | startWday=="Sun") %>%
		group_by(hr)
dfCityWknd <- summarise(dfCityWknd, tripsWknd=mean(trips))
dfCity <- join(dfCity, dfCityWday, by="hr")
dfCity <- join(dfCity, dfCityWknd, by="hr")
rm(dfCityWday, dfCityWknd)

#limiting down dfCity to just commute days- remove weekends/holidays
dfCityCom <- dfCity %>%
        filter(wday(dateTime, label=TRUE) != "Sat" & wday(dateTime, label=TRUE) != "Sun") %>%
        filter(as.Date(dateTime) != "2014-07-04" | as.Date(dateTime) != "2014-09-01" | as.Date(dateTime) != "2014-07-04") %>%
        select(dateTime, hr, startWday, trips, humidity, tempF, windSpeed, weathDesc)
dfCityCom$humidityDummy <- as.factor(ifelse(dfCityCom$humidity<8,"Less than 80%","Greater than 80%"))
dfCityCom$humidityDummy <- relevel(dfCityCom$humidityDummy, "Less than 80%")
dfComWday <- group_by(dfCityCom, hr)
dfComWday <- summarise(dfComWday, tripsWdayV2=mean(trips))
dfCityCom <- join(dfCityCom, dfComWday, by="hr")
rm(dfComWday)


#creating neighborhood broken out by hour
dfNabe <- group_by(df, dateTime, neighborhood)
dfNabe <- summarise(dfNabe, hr=mean(hr, na.rm=TRUE), trips=n(), humidity=mean(humidity, na.rm=TRUE), tempF=mean(tempF, na.rm=TRUE), windSpeed=mean(windSpeed, na.rm=TRUE), weathFlag=median(weathFlag, na.rm=TRUE))
                
#adding factor variables back in and narrowing out time frame
dfNabe <- dfNabe %>%
        mutate(weathDesc = ifelse(weathFlag==0,"clear", 
                        ifelse(weathFlag==1,"clear",
                        ifelse(weathFlag==2,"clear",
                        ifelse(weathFlag==3,"rain",
                        ifelse(weathFlag==4,"rain",
                        ifelse(weathFlag==5,"rain","clear"))))))) %>%
        mutate(startWday = wday(dateTime, label=TRUE)) %>%
        mutate(startNday = day(dateTime)) %>%
        filter(wday(dateTime, label=TRUE) != "Sat" | wday(dateTime, label=TRUE) != "Sun") %>%
        filter(as.Date(dateTime) != "2014-07-04" | as.Date(dateTime) != "2014-09-01" | as.Date(dateTime) != "2014-07-04") %>%
        select(neighborhood, hr, startWday, trips, humidity, tempF, windSpeed, weathDesc)

#creating neighborhood broken out by day
dfDay <- group_by(df, dateDay, neighborhood)
dfDay <- summarize(dfDay, trips=n(), age = mean(age, na.rm=TRUE), tripMin = mean(tripMin, na.rm=TRUE), tripDist = mean(tripDist, na.rm=TRUE), totDist=sum(tripDist, na.rm=TRUE), tripVel = mean(tripVel, na.rm=TRUE), dew=mean(dew, na.rm=TRUE), humidity=mean(humidity, na.rm=TRUE), precip=mean(precip, na.rm=TRUE), snow=mean(snow, na.rm=TRUE), tempF=mean(tempF, na.rm=TRUE), visib=mean(visib, na.rm=TRUE), windSpeed=mean(windSpeed, na.rm=TRUE), weathFlag=median(weathFlag, na.rm=TRUE))

#adding factor variables back in
dfDay <- dfDay %>%
        mutate(weathDesc = ifelse(weathFlag==0,"clear", 
                        ifelse(weathFlag==1,"clear",
                        ifelse(weathFlag==2,"clear",
                        ifelse(weathFlag==3,"rain",
                        ifelse(weathFlag==4,"rain",
                        ifelse(weathFlag==5,"rain","clear"))))))) %>%
        select(neighborhood, trips, humidity, tempF, windSpeed, weathDesc)


#choose an hour from hour-by-hour chart and pick one with significance
dfCityCom8 <- filter(dfCityCom, hour(dateTime) == 8)

#choose a linear model that best fits the data
f1 <- lm(trips ~ tempF, data=dfCityCom8)
f2 <- lm(trips ~ tempF + windSpeed, data=dfCityCom8)
f3 <- lm(trips ~ tempF + windSpeed + humidityDummy, data=dfCityCom8)
f4 <- lm(trips ~ tempF + windSpeed + weathDesc, data=dfCityCom8)
f5 <- lm(trips ~ tempF + windSpeed + humidityDummy + weathDesc, data=dfCityCom8)

#let's run the model through each of the neighborhoods for 7PM
coeffs <- data.frame()
output <- as.numeric()
for (i in 1:length(nabes)) {
        nf <- dfNabe %>% 
                filter(neighborhood==toupper(nabes[i]) & hour(dateTime) == 8)
        nf$humidityDummy <- as.factor(ifelse(nf$humidity<8,"Less than 80%","Greater than 80%"))
        nf$humidityDummy <- relevel(nf$humidityDummy, "Less than 80%")
        mod <- lm(trips ~ tempF + windSpeed + humidityDummy + weathDesc, data=nf)
        output <- as.numeric(mod$coefficients[2:5])
        intrcpt <- as.numeric(mod$coefficients[1])
        output[5] <- summary(mod)$r.squared
        coeffs <- rbind(coeffs,output)
}
coeffs[6] <- nabes
rm(mod,i,output, intrcpt)
colnames(coeffs) <- c("Temperature", "WindSpeed", "Humidity", "Rain", "ModelFit", "Neighborhood")
rownames(coeffs) <- nabes

coeffperc <- data.frame()
output <- as.numeric()
for (i in 1:length(nabes)) {
        nf <- dfNabe %>%
                filter(neighborhood==toupper(nabes[i]), hour(dateTime) == 8)
        nf$humidityDummy <- as.factor(ifelse(nf$humidity<8,"Less than 80%","Greater than 80%"))
        nf$humidityDummy <- relevel(nf$humidityDummy, "Less than 80%")
        mod <- lm(trips ~ tempF + windSpeed + humidityDummy + weathDesc, data=nf)
        output <- as.numeric(mod$coefficients[2:5])
        output <- round(100*(output/mean(nf$trips)),digits=1)
        output[5] <- summary(mod)$r.squared
        coeffperc <- rbind(coeffperc,output)
}
coeffperc[6] <- nabes
rm(mod,i,output)
colnames(coeffperc) <- c("Temperature", "WindSpeed", "Humidity", "Rain", "ModelFit", "Neighborhood")
rownames(coeffperc) <- nabes


#creating color palettes
Palette1 <- c("orange","magenta","red","blue","blue","blue")
Palette2 <- c("orangered1", "yellow4", "blueviolet", "deepskyblue", "seagreen1", "dodgerblue1")


##saving data sets to .rds files for Shiny App
saveRDS(dfDay, file="~/r/CitiBikeNYC/citibike/data/dfDay.rds")
saveRDS(weathOnly, file="~/r/CitiBikeNYC/citibike/data/weathOnly.rds")
saveRDS(nabes, file="~/r/CitiBikeNYC/citibike/data/nabes.rds")
saveRDS(Palette1, file="~/r/CitiBikeNYC/citibike/data/Palette1.rds")
saveRDS(Palette2, file="~/r/CitiBikeNYC/citibike/data/Palette2.rds")
saveRDS(dfCity, file="~/r/CitiBikeNYC/citibike/data/dfCity.rds")
saveRDS(dfCityCom, file="~/r/CitiBikeNYC/citibike/data/dfCityCom.rds")
saveRDS(dfCityCom8, file="~/r/CitiBikeNYC/citibike/data/dfCityCom8.rds")
saveRDS(dfNabe, file="~/r/CitiBikeNYC/citibike/data/dfNabe.rds")
saveRDS(f1, file="~/r/CitiBikeNYC/citibike/data/f1.rds")
saveRDS(f2, file="~/r/CitiBikeNYC/citibike/data/f2.rds")
saveRDS(f3, file="~/r/CitiBikeNYC/citibike/data/f3.rds")
saveRDS(f4, file="~/r/CitiBikeNYC/citibike/data/f4.rds")
saveRDS(f5, file="~/r/CitiBikeNYC/citibike/data/f5.rds")
saveRDS(coeffs, file="~/r/CitiBikeNYC/citibike/data/coeffs.rds")
saveRDS(coeffperc, file="~/r/CitiBikeNYC/citibike/data/coeffperc.rds")




##creting jpegs out of charts for faster load time (rather than rendering every chart)
jpeg('~/r/CitiBikeNYC/citibike/images/plot11.jpeg')
        ggplot(finJul,  aes(x = factor(""), fill = startWday) ) + geom_bar() + coord_polar(theta="y") + scale_x_discrete("") + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank()) + guides(fill = guide_legend(nrow = 1)) + scale_y_continuous(name="", labels = comma) + ggtitle("Total # of Trips by Day of Week, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot17.jpeg')
        ggplot(finJul,  aes(x = factor(""), fill = gender)) + geom_bar() + coord_polar(theta="y") + scale_x_discrete("") + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(), panel.margin=unit(-0.5, "line")) + guides(col = guide_legend(nrow = 1)) + scale_y_continuous(name="", labels = comma) + ggtitle("Total # Trips by Gender, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot18.jpeg')
        ggplot(cleanWeath,  aes(x = factor(""), fill = weathDesc) ) + geom_bar() + coord_polar(theta="y") + scale_x_discrete("") + theme(legend.position="bottom", legend.title=element_blank(), panel.margin=unit(-0.5, "line")) + scale_y_continuous(name="", labels = comma) + ggtitle("Total # Hours by Weather Type, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot19.jpeg')
        ggplot(cleanWeath,  aes(x = factor(""), fill = weathDesc) ) + geom_bar() + coord_polar(theta="y") + scale_x_discrete("") + theme(legend.position="bottom", legend.title=element_blank(), panel.margin=unit(-0.5, "line")) + scale_y_continuous(name="", labels = comma) + ggtitle("Total # Hours by Weather Type, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot13.jpeg')
        finJul %>%
        filter(weathDesc=="rain") %>%
        ggplot(aes(neighborhood)) + geom_bar(aes(fill=gender), position="stack", colour="black") + scale_x_discrete("") + theme(axis.text.x=element_text(angle=90, size=11)) + scale_y_continuous(name="", labels = comma) + ggtitle("Number of Trips during \'Rain\', Jul-Oct 2014")   
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot14.jpeg')
        finJul %>%
        filter(weathDesc=="clear") %>%
        ggplot(aes(neighborhood)) + geom_bar(aes(fill=gender), position="stack", colour="black") + scale_x_discrete("") + theme(axis.text.x=element_text(angle=90, size=11)) + scale_y_continuous(name="", labels = comma) + ggtitle("Number of Trips during \'Clear\', Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot15.jpeg')
        ggplot(finJul, aes(x=humidity, fill=gender)) + geom_histogram(binwidth=.04, fill="cornflowerblue", colour="black") + scale_y_continuous(name="", labels = comma) + ggtitle("Number of Trips by Humidity, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot16.jpeg')
        ggplot(finJul, aes(x=windSpeed)) + geom_histogram(binwidth=11/32, fill="cornflowerblue", colour="black") + scale_x_continuous(limits=c(0,7)) + scale_y_continuous(name="", labels = comma) + ggtitle("Number of Trips by WindSpeed, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot20.jpeg')
        ggplot(weathOnly, aes(x=humidity)) + geom_histogram(binwidth=.015, fill="orangered4", colour="black") + scale_y_continuous(name="", labels = comma) + ggtitle("Number of Hours by Humidity, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot21.jpeg')
        ggplot(weathOnly, aes(x=windSpeed)) + geom_histogram(binwidth=8/32, fill="orangered4",colour="black")  + scale_x_continuous(limits=c(0,6)) + scale_y_continuous(name="", labels = comma) + ggtitle("Number of Hours by WindSpeed, Jul-Oct 2014")
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot26.jpeg')
	ggplot(coeffperc, aes(Neighborhood, Humidity)) + geom_bar(stat="identity", position="dodge", aes(fill=Humidity)) + scale_fill_gradient(low = "red3", high = "white", space = "Lab", limits=c(-20,-5), guide = "colourbar") + xlab("") + ylab("") + ggtitle("Humidity - Percent Marginal Impact on # of Trips") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1, size=12)) + geom_text(aes(y=Humidity+1, label = paste(round(Humidity,digits=1),"%",sep="")), fontface=2, size = 4)
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot26a.jpeg')
	ggplot(coeffs, aes(Neighborhood, Humidity)) + geom_bar(stat="identity", position="dodge", aes(fill=Humidity)) + scale_fill_gradient(low = "red3", high = "white", space = "Lab", limits=c(-40,0), guide = "colourbar") + xlab("") + ylab("") + ggtitle("Humidity - Absolute Marginal Impact on # of Trips") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(y=Humidity+2, label = round(Humidity,digits=1)), fontface=2, size = 4)
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot27.jpeg')
	ggplot(coeffperc, aes(Neighborhood, Temperature)) + geom_bar(stat="identity", position="dodge", aes(fill=Temperature)) + scale_fill_gradient(low = "red3", high = "green4", space = "Lab", limits=c(-.35,.5), guide = "colourbar") + xlab("") + ylab("") + ggtitle("Temperature - Percent Marginal Impact on # of Trips") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(y=Temperature-.05, label = paste(round(Temperature,digits=1),"%",sep="")), fontface=2, size = 4)
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot27a.jpeg')
	ggplot(coeffs, aes(Neighborhood, Temperature)) + geom_bar(stat="identity", position="dodge", aes(fill=Temperature)) + scale_fill_gradient(low = "red3", high = "green4", space = "Lab", limits=c(-.9,.5), guide = "colourbar") + xlab("") + ylab("") + ggtitle("Temperature - Absolute Marginal Impact on # of Trips") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(y=Temperature-.05, label = round(Temperature,digits=1)), fontface=2, size = 4)
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot28.jpeg')
	ggplot(coeffperc, aes(Neighborhood, Rain)) + geom_bar(stat="identity", position="dodge", aes(fill=Rain)) + scale_fill_gradient(low = "red3", high = "white", space = "Lab", limits=c(-68,-50), guide = "colourbar") + xlab("") + ylab("") + ggtitle("Rain - Percent Marginal Impact on # of Trips") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(y=Rain+10, label = paste(round(Rain,digits=1),"%",sep="")), fontface=2, size = 4)
dev.off()

jpeg('~/r/CitiBikeNYC/citibike/images/plot28a.jpeg')
	ggplot(coeffs, aes(Neighborhood, Rain)) + geom_bar(stat="identity", position="dodge", aes(fill=Rain)) + scale_fill_gradient(low = "red3", high = "white", space = "Lab", limits=c(-180,5), guide = "colourbar") + xlab("") + ylab("") + ggtitle("Rain - Absolute Marginal Impact on # of Trips") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(y=Rain+10, label = round(Rain,digits=1)), fontface=2, size = 4)
dev.off()


