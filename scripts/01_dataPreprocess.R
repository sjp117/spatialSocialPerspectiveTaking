# import packages ----
library(tidyverse)
library(circular)

# helper functions ----

# function for angular distance
# calculate distance between angles with sign (- is left of reference)
# input: a = response angle in degrees
#        b = angle to compare against (reference angle)
# output: signed angle in degrees
distanceAng <- function (a, b) {
    phi = abs(b - a) %% 360
    distance = ifelse(phi > 180, 360 - phi, phi)
    sign = ifelse((a - b >= 0 & a - b <= 180) | (a - b <= -180 & a - b >= -360), 1, -1)
    result = sign * distance
    return(result)
}

# import data ----

# importing Qualtrics data involve removing second, extraneous, row

## import online data ----

# read only the first row and saves it with a name "header"
header01 <- read.csv("data/onlineData20220211.csv", header = F, nrows = 1)

# read the whole data excluding the first three rows and names it "data01"
data01 <- read.csv("data/onlineData20220211.csv", header = F, skip = 7)

# assign column names to "data01" using the header data
colnames(data01) <- header01

## import in-person data ----

# read only the first row and saves it with a name "header"
header02 <- read.csv("data/inPersonData_20220204.csv", header = F, nrows = 1)

# read the whole data excluding the first three rows and names it "data01"
data02 <- read.csv("data/inPersonData_20220204.csv", header = F, skip = 8)

# assign column names to "data01" using the header data
colnames(data02) <- header02

# select valid samples ----

# select samples who:
# 1) consented to participate
# 2) finished the task
# 3) are not test samples
#       - id numbers <= 9000 (online) and <= 1000 (in-person) are test samples

dat1 <- data01 %>%
    filter(ConsentForm == 1, Progress == 100, id > 9000)
dat2 <- data02 %>%
    filter(ConsentForm == 1, Progress == 100, id > 9000)

# labeling participation location ----

dat1$location <- "Online"
dat2$location <- "In-Person"

# select column ----

# select columns of interest, ignore irrelevant variables

dat1 <- dat1 %>%
    select(id, ResponseId, duration = `Duration (in seconds)`, ConsentForm, location,
           id, Progress, starts_with("D0", ignore.case = F), keyChoice, handChoice,
           starts_with("PT"), starts_with("SO"), contains("Rate"),
           starts_with("bigFiveF_"), c(starts_with("FB_"), ends_with("_R"), -ends_with("_DO")),
           starts_with("SAQ"), starts_with("ASQ"), -contains("BT", ignore.case = F),
           -c(ends_with("Click Count"), ends_with("Last Click"), ends_with("First Click"), ends_with("Date")))

dat2 <- dat2 %>%
    select(id, ResponseId, duration = `Duration (in seconds)`, ConsentForm, location,
           id, Progress, starts_with("D0", ignore.case = F), keyChoice, handChoice,
           starts_with("PT"), starts_with("SO"), contains("Rate"),
           starts_with("bigFiveF_"), c(starts_with("FB_"), ends_with("_R"), -ends_with("_DO")),
           starts_with("SAQ"), starts_with("ASQ"), -contains("BT", ignore.case = F),
           -c(ends_with("Click Count"), ends_with("Last Click"), ends_with("First Click"), ends_with("Date")))

# combine online & in-person data ----

combDat <- rbind(dat1, dat2)

# create new variable for gender
combDat$gender <-
    case_when(combDat$D03 == 1 ~ "Male", combDat$D03 == 2 ~ "Female") %>%
    factor()

# find repeating id numbers
n_occur <- data.frame(table(combDat$id))
combDat[combDat$id %in% n_occur$Var1[n_occur$Freq > 1],][1]

combDat[266,]$id <- 10065
combDat[270,]$id <- 20015
combDat[261,]$id <- 20068

# remove problematic sample
combDat <- combDat %>%
    filter(id != 93685)

# scoring tasks ----

## 3D task ----

### encode correct response ----

# eval correct response for PTLFR (correct == front; 2)
combDat <- combDat %>%
    mutate(
        PTLFR01_C = if_else(PTLFR01 == 2, 1, 0),
        PTLFR02_C = if_else(PTLFR02 == 2, 1, 0),
        PTLFR03_C = if_else(PTLFR03 == 2, 1, 0),
        PTLFR04_C = if_else(PTLFR04 == 2, 1, 0),
        PTLFR05_C = if_else(PTLFR05 == 2, 1, 0)
    )

# eval correct response for PTLLR (correct == left; 1)
combDat <- combDat %>%
    mutate(
        PTLLR01_C = if_else(PTLLR01 == 1, 1, 0),
        PTLLR02_C = if_else(PTLLR02 == 1, 1, 0),
        PTLLR03_C = if_else(PTLLR03 == 1, 1, 0),
        PTLLR04_C = if_else(PTLLR04 == 1, 1, 0),
        PTLLR05_C = if_else(PTLLR05 == 1, 1, 0)
    )

# eval correct response for PTLRR (correct == right; 3)
combDat <- combDat %>%
    mutate(
        PTLRR01_C = if_else(PTLRR01 == 3, 1, 0),
        PTLRR02_C = if_else(PTLRR02 == 3, 1, 0),
        PTLRR03_C = if_else(PTLRR03 == 3, 1, 0),
        PTLRR04_C = if_else(PTLRR04 == 3, 1, 0),
        PTLRR05_C = if_else(PTLRR05 == 3, 1, 0)
    )

# eval correct response for PTRFR (correct == front; 2)
combDat <- combDat %>%
    mutate(
        PTRFR01_C = if_else(PTRFR01 == 2, 1, 0),
        PTRFR02_C = if_else(PTRFR02 == 2, 1, 0),
        PTRFR03_C = if_else(PTRFR03 == 2, 1, 0),
        PTRFR04_C = if_else(PTRFR04 == 2, 1, 0),
        PTRFR05_C = if_else(PTRFR05 == 2, 1, 0)
    )

# eval correct response for PTRLR (correct == left; 1)
combDat <- combDat %>%
    mutate(
        PTRLR01_C = if_else(PTRLR01 == 1, 1, 0),
        PTRLR02_C = if_else(PTRLR02 == 1, 1, 0),
        PTRLR03_C = if_else(PTRLR03 == 1, 1, 0),
        PTRLR04_C = if_else(PTRLR04 == 1, 1, 0),
        PTRLR05_C = if_else(PTRLR05 == 1, 1, 0)
    )

# eval correct response for PTRRR (correct == right; 3)
combDat <- combDat %>%
    mutate(
        PTRRR01_C = if_else(PTRRR01 == 3, 1, 0),
        PTRRR02_C = if_else(PTRRR02 == 3, 1, 0),
        PTRRR03_C = if_else(PTRRR03 == 3, 1, 0),
        PTRRR04_C = if_else(PTRRR04 == 3, 1, 0),
        PTRRR05_C = if_else(PTRRR05 == 3, 1, 0)
    )

# eval correct response for PTSFR (correct == front; 2)
combDat <- combDat %>%
    mutate(
        PTSFR01_C = if_else(PTSFR01 == 2, 1, 0),
        PTSFR02_C = if_else(PTSFR02 == 2, 1, 0),
        PTSFR03_C = if_else(PTSFR03 == 2, 1, 0),
        PTSFR04_C = if_else(PTSFR04 == 2, 1, 0),
        PTSFR05_C = if_else(PTSFR05 == 2, 1, 0)
    )

# eval correct response for PTSLR (correct == left; 1)
combDat <- combDat %>%
    mutate(
        PTSLR01_C = if_else(PTSLR01 == 1, 1, 0),
        PTSLR02_C = if_else(PTSLR02 == 1, 1, 0),
        PTSLR03_C = if_else(PTSLR03 == 1, 1, 0),
        PTSLR04_C = if_else(PTSLR04 == 1, 1, 0),
        PTSLR05_C = if_else(PTSLR05 == 1, 1, 0)
    )

# eval correct response for PTSRR (correct == right; 3)
combDat <- combDat %>%
    mutate(
        PTSRR01_C = if_else(PTSRR01 == 3, 1, 0),
        PTSRR02_C = if_else(PTSRR02 == 3, 1, 0),
        PTSRR03_C = if_else(PTSRR03 == 3, 1, 0),
        PTSRR04_C = if_else(PTSRR04 == 3, 1, 0),
        PTSRR05_C = if_else(PTSRR05 == 3, 1, 0)
    )

### calculate score ----

# Calc PT left score
combDat$PTLtotal <- combDat %>%
    select(ends_with("_C") & starts_with("PTL")) %>%
    rowSums(., na.rm = T)

# Calc PT right score
combDat$PTRtotal <- combDat %>%
    select(ends_with("_C") & starts_with("PTR")) %>%
    rowSums(., na.rm = T)

# Calc PT other score
combDat$PTOtotal <- combDat$PTLtotal + combDat$PTRtotal

# Calc PT self score
combDat$PTStotal <- combDat %>%
    select(ends_with("_C") & starts_with("PTS")) %>%
    rowSums(., na.rm = T)

### calculate median response time ----

combDat %>%
    select(contains("Submit") & starts_with("PT") & -contains("PTS")) %>%
    apply(.,1,median)

# Calc PT other median RT
combDat$PTOrtMedian <- combDat %>%
    select(contains("Submit") & starts_with("PT") & -contains("PTS")) %>%
    apply(., 1, median, na.rm = T)

# Calc PT self median RT
combDat$PTSrtMedian <- combDat %>%
    select(contains("Submit") & starts_with("PTS")) %>%
    apply(., 1, median, na.rm = T)

## SO task ----

### calc response angle ----

# change the origin of the coordinate to zero 
combDat <- combDat %>%
    mutate(SO_Inst2_1_xC = SO_Inst2_1_x - 100,
           SO_Inst2_1_yC = -(SO_Inst2_1_y - 100),
           SO_01_SR_1_xC = SO_01_SR_1_x - 100,
           SO_01_SR_1_yC = -(SO_01_SR_1_y - 100),
           SO_02_SR_1_xC = SO_02_SR_1_x - 100,
           SO_02_SR_1_yC = -(SO_02_SR_1_y - 100),
           SO_03_SR_1_xC = SO_03_SR_1_x - 100,
           SO_03_SR_1_yC = -(SO_03_SR_1_y - 100),
           SO_04_SR_1_xC = SO_04_SR_1_x - 100,
           SO_04_SR_1_yC = -(SO_04_SR_1_y - 100),
           SO_05_SR_1_xC = SO_05_SR_1_x - 100,
           SO_05_SR_1_yC = -(SO_05_SR_1_y - 100),
           SO_06_SR_1_xC = SO_06_SR_1_x - 100,
           SO_06_SR_1_yC = -(SO_06_SR_1_y - 100),
           SO_07_SR_1_xC = SO_07_SR_1_x - 100,
           SO_07_SR_1_yC = -(SO_07_SR_1_y - 100),
           SO_08_SR_1_xC = SO_08_SR_1_x - 100,
           SO_08_SR_1_yC = -(SO_08_SR_1_y - 100),
           SO_09_SR_1_xC = SO_09_SR_1_x - 100,
           SO_09_SR_1_yC = -(SO_09_SR_1_y - 100),
           SO_10_SR_1_xC = SO_10_SR_1_x - 100,
           SO_10_SR_1_yC = -(SO_10_SR_1_y - 100),
           SO_11_SR_1_xC = SO_11_SR_1_x - 100,
           SO_11_SR_1_yC = -(SO_11_SR_1_y - 100),
           SO_12_SR_1_xC = SO_12_SR_1_x - 100,
           SO_12_SR_1_yC = -(SO_12_SR_1_y - 100)
    )

### calc rad from coordinates ----

# "coord2rad" is a function from the "circular" package
combDat <- combDat %>% 
    mutate(
        SO_Inst2_1_Rad = coord2rad(x = SO_Inst2_1_xC, y = SO_Inst2_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_01_SR_1_Rad = coord2rad(x = SO_01_SR_1_xC, y = SO_01_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_02_SR_1_Rad = coord2rad(x = SO_02_SR_1_xC, y = SO_02_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_03_SR_1_Rad = coord2rad(x = SO_03_SR_1_xC, y = SO_03_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_04_SR_1_Rad = coord2rad(x = SO_04_SR_1_xC, y = SO_04_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_05_SR_1_Rad = coord2rad(x = SO_05_SR_1_xC, y = SO_05_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_06_SR_1_Rad = coord2rad(x = SO_06_SR_1_xC, y = SO_06_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_07_SR_1_Rad = coord2rad(x = SO_07_SR_1_xC, y = SO_07_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_08_SR_1_Rad = coord2rad(x = SO_08_SR_1_xC, y = SO_08_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_09_SR_1_Rad = coord2rad(x = SO_09_SR_1_xC, y = SO_09_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_10_SR_1_Rad = coord2rad(x = SO_10_SR_1_xC, y = SO_10_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_11_SR_1_Rad = coord2rad(x = SO_11_SR_1_xC, y = SO_11_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
        SO_12_SR_1_Rad = coord2rad(x = SO_12_SR_1_xC, y = SO_12_SR_1_yC,
                                   control.circular = list(zero = pi/2, rotation = "clock")),
    )

### converts rad to deg ----

# "deg" is a function from the "circular" package
combDat <- combDat %>% 
    mutate(
        SO_Inst2_1_A = deg(SO_Inst2_1_Rad),
        SO_01_SR_1_A = deg(SO_01_SR_1_Rad),
        SO_02_SR_1_A = deg(SO_02_SR_1_Rad),
        SO_03_SR_1_A = deg(SO_03_SR_1_Rad),
        SO_04_SR_1_A = deg(SO_04_SR_1_Rad),
        SO_05_SR_1_A = deg(SO_05_SR_1_Rad),
        SO_06_SR_1_A = deg(SO_06_SR_1_Rad),
        SO_07_SR_1_A = deg(SO_07_SR_1_Rad),
        SO_08_SR_1_A = deg(SO_08_SR_1_Rad),
        SO_09_SR_1_A = deg(SO_09_SR_1_Rad),
        SO_10_SR_1_A = deg(SO_10_SR_1_Rad),
        SO_11_SR_1_A = deg(SO_11_SR_1_Rad),
        SO_12_SR_1_A = deg(SO_12_SR_1_Rad)
    )

### calc signed angular deviation ----

# "distanceAng" is a function specified at the start of this script
combDat <- combDat %>%
    mutate(
        SO_Inst2_1_Dist = distanceAng(SO_Inst2_1_A, 315),
        SO_01_SR_1_Dist = distanceAng(SO_01_SR_1_A, 123),
        SO_02_SR_1_Dist = distanceAng(SO_02_SR_1_A, 237),
        SO_03_SR_1_Dist = distanceAng(SO_03_SR_1_A, 83),
        SO_04_SR_1_Dist = distanceAng(SO_04_SR_1_A, 156),
        SO_05_SR_1_Dist = distanceAng(SO_05_SR_1_A, 319),
        SO_06_SR_1_Dist = distanceAng(SO_06_SR_1_A, 235),
        SO_07_SR_1_Dist = distanceAng(SO_07_SR_1_A, 333),
        SO_08_SR_1_Dist = distanceAng(SO_08_SR_1_A, 260),
        SO_09_SR_1_Dist = distanceAng(SO_09_SR_1_A, 280),
        SO_10_SR_1_Dist = distanceAng(SO_10_SR_1_A, 48),
        SO_11_SR_1_Dist = distanceAng(SO_11_SR_1_A, 26),
        SO_12_SR_1_Dist = distanceAng(SO_12_SR_1_A, 150)
    )

### calc absolute angular deviation ----
combDat <- combDat %>%
    mutate(
        SO_Inst2_1_AD = abs(SO_Inst2_1_Dist),
        SO_01_SR_1_AD = abs(SO_01_SR_1_Dist),
        SO_02_SR_1_AD = abs(SO_02_SR_1_Dist),
        SO_03_SR_1_AD = abs(SO_03_SR_1_Dist),
        SO_04_SR_1_AD = abs(SO_04_SR_1_Dist),
        SO_05_SR_1_AD = abs(SO_05_SR_1_Dist),
        SO_06_SR_1_AD = abs(SO_06_SR_1_Dist),
        SO_07_SR_1_AD = abs(SO_07_SR_1_Dist),
        SO_08_SR_1_AD = abs(SO_08_SR_1_Dist),
        SO_09_SR_1_AD = abs(SO_09_SR_1_Dist),
        SO_10_SR_1_AD = abs(SO_10_SR_1_Dist),
        SO_11_SR_1_AD = abs(SO_11_SR_1_Dist),
        SO_12_SR_1_AD = abs(SO_12_SR_1_Dist)
    )

### calc mean angular deviation ----
combDat$angDivMean <- combDat %>%
    select(starts_with("SO") & ends_with("_AD") & -contains("Inst")) %>%
    rowMeans(x = ., na.rm = T)

## False Belief task ----

### determine correct response ----
combDat <- combDat %>%
    mutate(
        FB_01_RC = ifelse(FB_01_R == 1, 1, 0),
        FB_02_RC = ifelse(FB_02_R == 1, 1, 0),
        FB_03_RC = ifelse(FB_03_R == 2, 1, 0),
        FB_04_RC = ifelse(FB_04_R == 2, 1, 0),
        FB_05_RC = ifelse(FB_05_R == 1, 1, 0),
        FB_06_RC = ifelse(FB_06_R == 2, 1, 0),
        FB_07_RC = ifelse(FB_07_R == 2, 1, 0),
        FB_08_RC = ifelse(FB_08_R == 1, 1, 0),
        FB_09_RC = ifelse(FB_09_R == 1, 1, 0),
        FB_10_RC = ifelse(FB_10_R == 1, 1, 0),
        FB_11_RC = ifelse(FB_11_R == 2, 1, 0),
        FB_12_RC = ifelse(FB_12_R == 1, 1, 0)
    )

### compute total scores
combDat <- combDat %>%
    mutate(
        FB_Total = FB_01_RC + FB_02_RC + FB_03_RC + FB_04_RC + FB_05_RC +  + FB_06_RC,
        FB_Control_Total = FB_07_RC + FB_08_RC + FB_09_RC + FB_10_RC + FB_11_RC + FB_12_RC
    )

# scoring questionnaires ----

## Big 5 Personality ----

### reverse code ----
combDat <- combDat %>%
    mutate(
        bigFiveF_1_3C	= 5 - bigFiveF_1_3 + 1,
        bigFiveF_1_4C	= 5 - bigFiveF_1_4 + 1,
        bigFiveF_1_5C	= 5 - bigFiveF_1_5 + 1,
        bigFiveF_1_8C	= 5 - bigFiveF_1_8 + 1,
        bigFiveF_1_9C	= 5 - bigFiveF_1_9 + 1,
        bigFiveF_2_2C	= 5 - bigFiveF_2_2 + 1,
        bigFiveF_2_3C	= 5 - bigFiveF_2_3 + 1,
        bigFiveF_2_7C	= 5 - bigFiveF_2_7 + 1,
        bigFiveF_2_8C	= 5 - bigFiveF_2_8 + 1,
        bigFiveF_3_4C	= 5 - bigFiveF_3_4 + 1,
        bigFiveF_3_5C	= 5 - bigFiveF_3_5 + 1,
        bigFiveF_3_6C	= 5 - bigFiveF_3_6 + 1,
        bigFiveF_3_7C	= 5 - bigFiveF_3_7 + 1,
        bigFiveF_3_8C	= 5 - bigFiveF_3_8 + 1,
        bigFiveF_4_1C	= 5 - bigFiveF_4_1 + 1,
        bigFiveF_4_2C	= 5 - bigFiveF_4_2 + 1,
        bigFiveF_4_3C	= 5 - bigFiveF_4_3 + 1,
        bigFiveF_4_4C	= 5 - bigFiveF_4_4 + 1,
        bigFiveF_4_9C	= 5 - bigFiveF_4_9 + 1,
        bigFiveF_5_1C	= 5 - bigFiveF_5_1 + 1,
        bigFiveF_5_6C	= 5 - bigFiveF_5_6 + 1,
        bigFiveF_5_8C	= 5 - bigFiveF_5_8 + 1,
        bigFiveF_5_9C	= 5 - bigFiveF_5_9 + 1,
        bigFiveF_6_2C	= 5 - bigFiveF_6_2 + 1,
        bigFiveF_6_3C	= 5 - bigFiveF_6_3 + 1,
        bigFiveF_6_4C	= 5 - bigFiveF_6_4 + 1,
        bigFiveF_6_5C	= 5 - bigFiveF_6_5 + 1,
        bigFiveF_6_6C	= 5 - bigFiveF_6_6 + 1,
        bigFiveF_7_1C	= 5 - bigFiveF_7_1 + 1,
        bigFiveF_7_4C	= 5 - bigFiveF_7_4 + 1
    )

### add sub-facet variables ----
combDat <- combDat %>%
    mutate(
        bigFiveF_E1	= bigFiveF_1_1 + bigFiveF_2_7C + bigFiveF_4_4C + bigFiveF_6_1,
        bigFiveF_E2	= bigFiveF_1_6 + bigFiveF_3_3 + bigFiveF_4_9C + bigFiveF_6_6C,
        bigFiveF_E3	= bigFiveF_2_2C + bigFiveF_3_8C + bigFiveF_5_5 + bigFiveF_7_2,
        bigFiveF_E	= bigFiveF_E1 + bigFiveF_E2 + bigFiveF_E3,
        bigFiveF_A1	= bigFiveF_1_2 + bigFiveF_2_8C + bigFiveF_4_5 + bigFiveF_6_2C,
        bigFiveF_A2	= bigFiveF_1_7 + bigFiveF_3_4C + bigFiveF_5_1C + bigFiveF_6_7,
        bigFiveF_A3	= bigFiveF_2_3C + bigFiveF_3_9 + bigFiveF_5_6C + bigFiveF_7_3,
        bigFiveF_A	= bigFiveF_A1 + bigFiveF_A2 + bigFiveF_A3,
        bigFiveF_C1	= bigFiveF_1_3C + bigFiveF_2_9 + bigFiveF_4_6 + bigFiveF_6_3C,
        bigFiveF_C2	= bigFiveF_1_8C + bigFiveF_3_5C + bigFiveF_5_2 + bigFiveF_6_8,
        bigFiveF_C3	= bigFiveF_2_4 + bigFiveF_4_1C + bigFiveF_5_7 + bigFiveF_7_4C,
        bigFiveF_C	= bigFiveF_C1 + bigFiveF_C2 + bigFiveF_C3,
        bigFiveF_N1	= bigFiveF_1_4C + bigFiveF_3_1 + bigFiveF_4_7 + bigFiveF_6_4C,
        bigFiveF_N2	= bigFiveF_1_9C + bigFiveF_3_6C + bigFiveF_5_3 + bigFiveF_6_9,
        bigFiveF_N3	= bigFiveF_2_5 + bigFiveF_4_2C + bigFiveF_5_8C + bigFiveF_7_5,
        bigFiveF_N	= bigFiveF_N1 + bigFiveF_N2 + bigFiveF_N3,
        bigFiveF_O1	= bigFiveF_2_1 + bigFiveF_3_7C + bigFiveF_5_4 + bigFiveF_7_1C,
        bigFiveF_O2	= bigFiveF_1_5C + bigFiveF_3_2 + bigFiveF_4_8 + bigFiveF_6_5C,
        bigFiveF_O3	= bigFiveF_2_6 + bigFiveF_4_3C + bigFiveF_5_9C + bigFiveF_7_6,
        bigFiveF_O	= bigFiveF_O1 + bigFiveF_O2 + bigFiveF_O3
    )

# bigFiveF_E1	= Sociability
# bigFiveF_E2	= Assertiveness
# bigFiveF_E3	= Energy Level
# bigFiveF_E	= Extraversion
# bigFiveF_A1	= Compassion
# bigFiveF_A2	= Respectfulness
# bigFiveF_A3	= Trust
# bigFiveF_A	= Agreeableness
# bigFiveF_C1	= Organization
# bigFiveF_C2	= Productiveness
# bigFiveF_C3	= Responsibility
# bigFiveF_C	= Conscientiousness
# bigFiveF_N1	= Anxiety
# bigFiveF_N2	= Depression
# bigFiveF_N3	= Emotional Volatility
# bigFiveF_N	= Negative Emotionality
# bigFiveF_O1	= Intellectual Curiosity
# bigFiveF_O2	= Aesthetic Sensitivity
# bigFiveF_O3	= Creative Imagination
# bigFiveF_O	= Open-Mindedness

## SAQ ----

### compute scores ----

combDat <- combDat %>%
    mutate(
        SAQ_Total	= rowSums(select(combDat, starts_with("SAQ")) - 1, na.rm = T),
        SAQ_Prop	= SAQ_Total/96,
        SAQ_I_Total	= (SAQ01 + SAQ04 + SAQ07 + SAQ11 + SAQ13 + SAQ15 + SAQ20 + SAQ24) - 8,
        SAQ_I_Prop	= SAQ_I_Total/32,
        SAQ_N_Total	= (SAQ02 + SAQ06 + SAQ08 + SAQ10 + SAQ14 + SAQ18 + SAQ22 + SAQ23) - 8,
        SAQ_N_Prop	= SAQ_N_Total/32,
        SAQ_M_Total	= (SAQ03 + SAQ05 + SAQ09 + SAQ12 + SAQ16 + SAQ17 + SAQ19 + SAQ21) - 8,
        SAQ_M_Prop	= SAQ_M_Total/32,
    )

# I = Imagery Ability
# N = Navigation Ability
# M = Mental-Manipulation Ability

## ASQ ----

### compute score ----
combDat <- combDat %>%
    mutate(
        ASQ_Total = rowSums(select(combDat, starts_with("ASQ") & ends_with("_1")), na.rm = T),
        ASQ_Prop = ASQ_Total/340
    )

# Compile clean data ----

cleanData <- combDat %>%
    select(id, ResponseId, duration, ConsentForm, location, gender,
           starts_with("D0", ignore.case = F), keyChoice, handChoice,
           grep('bigFiveF_[[:alpha:]]', colnames(combDat)),
           grep('SAQ_*_', colnames(combDat)),
           grep('ASQ_[[:alpha:]]', colnames(combDat)),
           grep('FB_[[:alpha:]]', colnames(combDat)),
           starts_with("FB") & ends_with("C"),
           starts_with("FB") & ends_with("Submit"),
           starts_with("SO") & ends_with("_Dist") | ends_with("_AD") & -contains("Inst"),
           angDivMean,
           starts_with("SO") & ends_with("Submit"),
           -(contains(c("_x", "_y"))),
           PTOrtMedian, PTSrtMedian, PTStotal, PTOtotal, PTLtotal, PTRtotal,
           starts_with("PT") & ends_with("Submit"),
           starts_with("PT") & ends_with("_C")
    )

# Export clean data ----

write.csv(cleanData, file = "data/cleanData.csv", row.names = F)
