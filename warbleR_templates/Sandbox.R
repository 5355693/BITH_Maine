install.packages("warbleR")
library(warbleR)
library(ggplot2)

dir.create(file.path(getwd(),"warbleR_templates"))
setwd(file.path(getwd(),"warbleR_templates"))
getwd()

## Query to find BITH recordings:
Cat.bick <- querxc(qword = "Catharus bicknelli", download = FALSE)

## Limit to recordings from Mansfield:
Cat.bick <- Cat.bick[grep("Mt Mansfield, Vermont|Stowe", Cat.bick$Locality, ignore.case = FALSE),]

## Download this subset of recordings (from Mansfield or Stowe)
querxc(X = Cat.bick)

## Convert mp3 to wav (make sure you are in the right working directory, 
## as the function takes no arguments):
mp32wav()

## Used the interactive mode to find a BITH call in the file, zoom in so it was
## the only call on the screen, and then save the screen as a WAV file.
## "/Users/johnlloyd/GitHub/BITH_Maine/warbleR_templates/Catharus-bicknelli-160920_8.4375-10.3125.wav"
viewSpec(clip=list.files(getwd())[5], interactive = TRUE, frq.lim = c(1,15), spec.col = gray.3())

## Turn this into a template:
bithTemplate1 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/warbleR_templates/Catharus-bicknelli-160920_8.4375-10.3125.wav",
                                 frq.lim = c(2,8), name = "call")
bithTemplate1 
plot(bithTemplate1)
cscores <-corMatch(survey = list.files(getwd())[3], templates = bithTemplate1,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cscores 
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)

## This detects many, but not all calls, and it seems like reducing the score threshold
## would help.
templateCutoff(bithTemplate1)[1] <- c(call = 0.28)

## What about trying this on a real survey?
viewSpec(clip = "S4A01556_20170605_202700.wav",interactive = FALSE)
cscores <-corMatch(survey = "S4A01556_20170605_202700.wav", templates = bithTemplate1,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cscores
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)

## Finds a lot of false positives, where part of a SWTH song is registering as a detection.

## Let's try a different survey:
templateCutoff(bithTemplate1)[1] <- c(call = 0.4)
cscores <-corMatch(survey = "S4A01556_20170608_042600.wav", templates = bithTemplate1,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cscores
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects@detections
showPeaks(cdetects, id = 3)


## The bith template finds a lot of MAWA by accident. Curious to see what happens with a real MAWA template:
mawaTemplate1 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/warbleR_templates/MAWA.wav",
                                 frq.lim = c(1,8), name = "MAWAsong")

cscores <-corMatch(survey = "S4A01556_20170608_042600.wav", templates = mawaTemplate1,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects@detections
showPeaks(cdetects)

## Let's try a BITH song
bithTemplate1 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHsong1.wav",
                                 name = "s1")
bithTemplate2 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHsong2.wav",
                                 name = "s2", frq.lim = c(2.5,9)) ##frq.lim settings can greatly affect results! 
bithTemplate3 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHsong3.wav",
                                 name = "s3")
bithTemplate4 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHCall1.wav",
                                 name = "c1", frq.lim = c(3,5.5))
bithTemplate5 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHsong5.wav", 
                                 name = "s4")
bithTemplate6 <- makeCorTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/BITHsong4.wav",
                                 name = "s5")
bithTemps <- combineCorTemplates(bithTemplate1,bithTemplate4)
templateCutoff(bithTemps)
templateCutoff(bithTemps)[1:4] <- c(s1 = 0.28, s2 = 0.28, s3 = 0.28, c1 = 0.1)

cscores <-corMatch(survey = "S4A01574_20160604_210803.wav", templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
#cdetects@detections
#showPeaks(cdetects, what = "peaks")
plot(cdetects)
## Templates 2 and 3 are not useful, but Template 1 would work if it was set to a
## score cutoff of ~0.25
par(mfrow = c(1,3))
plot(bithTemplate1)
plot(bithTemplate2)
plot(bithTemplate3) # I think this template fails because it has lots of bg noise

#Interestingly, the "Call" template picks up many songs, too!

#Try this again with the 2 useful templates and cutoffs adjusted
bithTemps <- combineCorTemplates(bithTemplate1, bithTemplate4)
templateCutoff(bithTemps)
templateCutoff(bithTemps)[1:2] <- c(s1 = 0.25, c1 = 0.1)
cscores <-corMatch(survey = "S4A01574_20160604_210803.wav", templates = bithTemps,
                   parallel = T, show.prog = TRUE, cor.method = "pearson")
cdetects <- findPeaks(score.obj = cscores, parallel = T)
cdetects
plot(cdetects)

## I had to change the dbUploadTemplate function because it wasn't working. 
## It was trying to INSERT a "NULL" into the pkTemplateID column. That column
## auto-increments, so works only if the field is left empty. The only way
## I could figure out how to do this was to change the SQL INSERT command 
## so that it didn't refer at all to pkTemplateID, which effectivel left it blank
## and able to auto-increment after the INSERT completed.

dbUploadTemplate(templates = bithTemps,
                 uid = "root",
                 pwd = "RSW4xN!!",
                 db.name = "noh",
                 analyst = 1, 
                 locationID = "2",
                 date.recorded = "2017/09/07",
                 recording.equip = "Unknown",
                 species.code = "BITH",
                 type = "COR")

dbDownloadCardRecorderID(db.name = "noh", uid = "root", pwd = "RSW4xN!!")

## Binary point matching
bithTemplate1 <- makeBinTemplate(clip = "/Users/johnlloyd/GitHub/BITH_Maine/warbleR_templates/BITHsong.wav",
                                 name = "song")

bscores <- binMatch(survey = "S4A01556_20170608_042600.wav", templates = bithTemplate1,
                    parallel = T, show.prog = TRUE)

bdetects <- findPeaks(bscores)
showPeaks(bdetects,id = 150)

###

ch <- odbcConnect(dsn="noh", uid = "root", pwd = "RSW4xN!!")
sqlTables(ch)
ch


