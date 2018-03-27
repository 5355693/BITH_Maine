install.packages("monitoR")
library(monitoR)
data("survey")
survey

#View the spectogram of the recording:
viewSpec(survey)

#Play the sound in R, after having installed the 
#command-line wav player SoundExchange (aka SoX):
#"brew install sox" from bash.
#The player itself is named 'play'.
setWavPlayer("play")
play(survey)

#Add clips of BTNW & OVEN:
data(btnw)
data(oven)
viewSpec(btnw)

#As Wave objects, these recordings can be used directly in the template matching functions.
#Users will typically work with wav files, however, as opposed to mp3 files, or Wave objects. To be
#consistent with this typical approach, we will save the two clips and the survey as wav files.
#We’ll include a made-up date in the name for the survey file, since that is one way to determine “absolute”
#times for detections

btnw.fp <- file.path(tempdir(), "btnw.wav")
oven.fp <- file.path(tempdir(), "oven.wav")
survey.fp <- file.path(tempdir(), "survey2010-12-31_120000_EST.wav")
writeWave(btnw, btnw.fp) #This is recreating a Wave file, as would be downloaded from the recorder.
writeWave(oven, oven.fp)
writeWave(survey, survey.fp)

#Spectogram cross correlation.
## The easiest way to create a correlation template is to call makeCorTemplate with only the 'clip'
## argument specified (here I added the "name" argument because the resulting template is put into 
## a list and automatically given the name "A", which isn't informative.The makeCorTemplate function 
## displays a spectrogram of the recording and shows the cells included in the template in transparent 
## purple:
wct1 <- makeCorTemplate(btnw.fp, name = "w1")

#Review the template:
wct1

## This template is functional, but it does not take advantage of all the options available in 
## makeCorTemplate.Setting time and frequency limits is one of simplest, and may be necessary when 
## making a template from a long recording. We’ll make a new template that uses the t.lim and frq.lim 
## arguments to focus on a particular section within the song.
wct2 <- makeCorTemplate(btnw.fp, t.lim = c(1.5, 2.1), frq.lim = c(4.2, 5.6), name = "w2")
templateNames(wct2) <- "NewName" #you can also change the name of the template after the fact.

#Ovenbird templates:
oct1 <- makeCorTemplate(oven.fp, t.lim = c(1,4), frq.lim = c(1,11), name = "o1")
#Another useful step is to adjust the number of points included with the 'dens' argument:
oct2 <- makeCorTemplate(oven.fp, t.lim = c(1,4), frq.lim = c(1,11), dens = 0.1, name = "o2")
oct1;oct2 #notice that oct2 has ~10% of the number of points included as oct1.

## monitoR is designed to use template lists that contain multiple templates. 
## Templates can be combined together in a single list with combineCorTemplates.
ctemps <- combineCorTemplates(wct1, wct2, oct1, oct2)
ctemps
templateNames(ctemps)[2] <- "w2" #change the name back to match the tutorial :)
ctemps
plot(ctemps)

## We can now use our templates to look for black-throated green warbler and ovenbird songs within
## the survey recording. This is a three-step process: 
## 1) we first calculate correlation score between the templates and each time bin in the survey, 
## 2) then identify “peaks” in the scores, and 
## 3) finally determine which, if any, score peaks exceed the score cutoff. 
## Correlation scores are calculated with corMatch.

cscores <-corMatch(survey = survey.fp, templates = ctemps, parallel = T, show.prog = TRUE,
                   cor.method = "pearson")
cscores

##  To make detections, we need to look within the correlation scores returned by
## corMatch for peaks, and then identify those peaks with values above the score cutoff as detections.
## These two steps are carried out with findPeaks.

cdetects <- findPeaks(score.obj = cscores, parallel = T)

## From the n.peaks column, we can see there are from five to 38 peaks per template, and from the
## n.detections column, we can see that the templates resulted in from one to three detections:
cdetects

## The detections can be extracted with the getDetections function:
getDetections(cdetects)

## If you only wanted a single template, use 'which.one':
getDetections(detection.obj = cdetects, which.one = "w1")

## Whether or not a peak qualifies as a detection depends a parameter called the score cutoff, which can
## be set separately for each template. If we look at our template list again, we can see the score cutoffs
## given in the last column of the summary.
ctemps

## They are currently set to the default value of 0.4. No reason to think that this value should
## work for all or even any templates. Instead, score cutoffs need to be determined based on template
## performance, typically with a subset of the survey or surveys that will ultimately be searched for
## matching sounds. Here we just have one, very short, survey, so we’ll use the entire survey to determine
## what the score cutoff should be set to for each template. We can see a graphical summary of the
## results with the generic plot function.
plot(cdetects)

## Both Ovenbird templates work well. The BTNW templates perform differently, though, with the second
## template apparently better than the first. The peaks for the second template are higher during actual
## songs and lower during non-singing periods. It seems like reducing the frequency over which the template
## occurs helps.For narrow-banded songs, this would be a good strategy.

## We can also see that reducing the score cutoff would reduce errors of omission - for example, the peak
## for the faint song of the Ovenbird is missed with a 0.4 cutoff, but would be detected at a 0.2 cutoff.

## To change the cutoff without re-running the previous steps:
templateCutoff(ctemps)
templateCutoff(ctemps)[1:4] <- c(w1 = 0.2, w2 = 0.3, o1 = 0.2, o2 = 0.2)
#or, more efficiently: templateCutoff(ctemps) <- c(w2 = 0.3, default = 0.2)

#the 'cdetects' file is automatically updated!
cdetects

## Since the template w1 is nearly useless, and template o1 and o2 nearly identical, we might want to
## drop w1 and either o1 or o2 from our results.
cdetects <- cdetects[c("w2","o2")]
cdetects

showPeaks(detection.obj = cdetects, which.one = 'w2', point = T, scorelim = c(0,1),
          verify = TRUE, what = "detections", player = "play")

viewSpec(survey, interactive = TRUE, annotate = TRUE)


