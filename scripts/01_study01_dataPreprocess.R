# import packages ----
library(dplyr)
library(tidyr)
library(data.table)

# helper functions ----

# function for reverse coding Likert scales
# given a Likert score and the range of the Likert scale, return the reversed score
# input: a = Likert score (integer)
#        b = range of the Likert scale (integer)
# output: reversed Likert score
revCode <- function(a, b) {
    result = (b + 1) - a
    return(result)
}

# import data ----

# importing Qualtrics data involve removing second, extraneous, row

## import study 01 data ----

# read only the first row and saves it with a name "header"
header01 <- read.csv("data/study01_Data20210415.csv", header = F, nrows = 1) %>% as.character()

# read the whole data excluding the first three rows and names it "data01"
data01 <- fread("data/study01_Data20210415.csv", col.names = header01, skip = 2)

data01 <- data01 %>%
    filter(ConsentForm == 1 & Progress == 100 & Finished == 1 & id > 90000)

data01 <- data01 %>%
    select(id, ResponseId, duration = `Duration (in seconds)`, ConsentForm, FL_66_DO,
           id, Progress, starts_with("D0", ignore.case = F), keyChoice, handChoice,
           starts_with("P"), contains("Rate"),
           starts_with("bigFiveF_"), c(starts_with("FB"), -ends_with("_DO")),
           starts_with("SAQ"), starts_with("ASQ"), -contains("BT", ignore.case = F),
           starts_with("VV"),
           -c(ends_with("Click Count"), ends_with("Last Click"), ends_with("First Click"), ends_with("Date")))

# create new variable for gender
data01$gender <-
    case_when(data01$D03 == 1 ~ "Male", data01$D03 == 2 ~ "Female") %>%
    factor()

# variable for task order (1 = perspective task first)
data01$ptFirst <-
    ifelse(data01$FL_66_DO == "FL_64", 1, 0) %>%
    factor()

# scoring tasks ----

## perspective taking task ----

### determine correct response ----

data01 <- data01 %>%
    mutate(
        PPFR01_C = if_else(PTLFR01 == 2, 1, 0),
        PPFR02_C = if_else(PTLFR01 == 2, 1, 0),
        PPFR03_C = if_else(PTLFR01 == 2, 1, 0),
        PPFR04_C = if_else(PTLFR01 == 2, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PPLR01_C = if_else(PTLFR01 == 1, 1, 0),
        PPLR02_C = if_else(PTLFR01 == 1, 1, 0),
        PPLR03_C = if_else(PTLFR01 == 1, 1, 0),
        PPLR04_C = if_else(PTLFR01 == 1, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PPRR01_C = if_else(PPRR01 == 3, 1, 0),
        PPRR02_C = if_else(PPRR02 == 3, 1, 0),
        PPRR03_C = if_else(PPRR03 == 3, 1, 0),
        PPRR04_C = if_else(PPRR04 == 3, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PTLFR01_C = if_else(PTLFR01 == 2, 1, 0),
        PTLFR02_C = if_else(PTLFR02 == 2, 1, 0),
        PTLFR03_C = if_else(PTLFR03 == 2, 1, 0),
        PTLFR04_C = if_else(PTLFR04 == 2, 1, 0),
        PTLFR05_C = if_else(PTLFR05 == 2, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PTLRR01_C = if_else(PTLRR01 == 3, 1, 0),
        PTLRR02_C = if_else(PTLRR02 == 3, 1, 0),
        PTLRR03_C = if_else(PTLRR03 == 3, 1, 0),
        PTLRR04_C = if_else(PTLRR04 == 3, 1, 0),
        PTLRR05_C = if_else(PTLRR05 == 3, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PTRFR01_C = if_else(PTRFR01 == 2, 1, 0),
        PTRFR02_C = if_else(PTRFR02 == 2, 1, 0),
        PTRFR03_C = if_else(PTRFR03 == 2, 1, 0),
        PTRFR04_C = if_else(PTRFR04 == 2, 1, 0),
        PTRFR05_C = if_else(PTRFR05 == 2, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PTRLR01_C = if_else(PTRLR01 == 1, 1, 0),
        PTRLR02_C = if_else(PTRLR02 == 1, 1, 0),
        PTRLR03_C = if_else(PTRLR03 == 1, 1, 0),
        PTRLR04_C = if_else(PTRLR04 == 1, 1, 0),
        PTRLR05_C = if_else(PTRLR05 == 1, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PTSFR01_C = if_else(PTSFR01 == 2, 1, 0),
        PTSFR02_C = if_else(PTSFR02 == 2, 1, 0),
        PTSFR03_C = if_else(PTSFR03 == 2, 1, 0),
        PTSFR04_C = if_else(PTSFR04 == 2, 1, 0),
        PTSFR05_C = if_else(PTSFR05 == 2, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PTSLR01_C = if_else(PTSLR01 == 1, 1, 0),
        PTSLR02_C = if_else(PTSLR02 == 1, 1, 0),
        PTSLR03_C = if_else(PTSLR03 == 1, 1, 0),
        PTSLR04_C = if_else(PTSLR04 == 1, 1, 0),
        PTSLR05_C = if_else(PTSLR05 == 1, 1, 0)
    )

data01 <- data01 %>%
    mutate(
        PTSRR01_C = if_else(PTSRR01 == 3, 1, 0),
        PTSRR02_C = if_else(PTSRR02 == 3, 1, 0),
        PTSRR03_C = if_else(PTSRR03 == 3, 1, 0),
        PTSRR04_C = if_else(PTSRR04 == 3, 1, 0),
        PTSRR05_C = if_else(PTSRR05 == 3, 1, 0)
    )

### get total score ----

# Calc PT left score
data01$PTLtotal <- data01 %>%
    select(ends_with("_C") & starts_with("PTL")) %>%
    rowSums(., na.rm = T)

# Calc PT right score
data01$PTRtotal <- data01 %>%
    select(ends_with("_C") & starts_with("PTR")) %>%
    rowSums(., na.rm = T)

# Calc PT other score
data01$PTOtotal <- data01$PTLtotal + data01$PTRtotal

# Calc PT self score
data01$PTStotal <- data01 %>%
    select(ends_with("_C") & starts_with("PTS")) %>%
    rowSums(., na.rm = T)

data01$PTSprop <- data01$PTStotal / 15

data01$PTOprop <- data01$PTOtotal / 20

### calculate median response time ----

data01 %>%
    select(contains("Submit") & starts_with("PT") & -contains("PTS")) %>%
    apply(.,1,median)

# Calc PT other median RT
data01$PTOrtMedian <- data01 %>%
    select(contains("Submit") & starts_with("PT") & -contains("PTS")) %>%
    apply(., 1, median, na.rm = T)

# Calc PT self median RT
data01$PTSrtMedian <- data01 %>%
    select(contains("Submit") & starts_with("PTS")) %>%
    apply(., 1, median, na.rm = T)


## false belief task ----

### determine correct response ----
data01 <- data01 %>%
    mutate(
        FB_01_RC = ifelse(FB01 == 1, 1, 0),
        FB_02_RC = ifelse(FB02 == 1, 1, 0),
        FB_03_RC = ifelse(FB03 == 2, 1, 0),
        FB_04_RC = ifelse(FB04 == 2, 1, 0),
        FB_05_RC = ifelse(FB05 == 1, 1, 0),
        FB_06_RC = ifelse(FB06 == 2, 1, 0),
        FB_07_RC = ifelse(FB07 == 2, 1, 0),
        FB_08_RC = ifelse(FB08 == 1, 1, 0),
        FB_09_RC = ifelse(FB09 == 1, 1, 0),
        FB_10_RC = ifelse(FB10 == 1, 1, 0),
        FB_11_RC = ifelse(FB11 == 2, 1, 0),
        FB_12_RC = ifelse(FB12 == 1, 1, 0)
    )

### compute total scores ----
data01 <- data01 %>%
    mutate(
        FBOtotal = FB_01_RC + FB_02_RC + FB_03_RC + FB_04_RC + FB_05_RC +  + FB_06_RC,
        FBStotal = FB_07_RC + FB_08_RC + FB_09_RC + FB_10_RC + FB_11_RC + FB_12_RC,
        FBOprop = FBOtotal / 6,
        FBSprop = FBStotal / 6
        
    )


## big-5 personality ----

### correct for reverse coding ----

data01 <- data01 %>%
    mutate(
        bigFiveF_3C	= revCode(bigFiveF_3, 5),
        bigFiveF_4C	= revCode(bigFiveF_4, 5),
        bigFiveF_5C	= revCode(bigFiveF_5, 5),
        bigFiveF_8C	= revCode(bigFiveF_8, 5),
        bigFiveF_9C	= revCode(bigFiveF_9, 5),
        bigFiveF_11C = revCode(bigFiveF_11, 5),
        bigFiveF_12C = revCode(bigFiveF_12, 5),
        bigFiveF_16C = revCode(bigFiveF_16, 5),
        bigFiveF_17C = revCode(bigFiveF_17, 5),
        bigFiveF_22C = revCode(bigFiveF_22, 5),
        bigFiveF_23C = revCode(bigFiveF_23, 5),
        bigFiveF_24C = revCode(bigFiveF_24, 5),
        bigFiveF_25C = revCode(bigFiveF_25, 5),
        bigFiveF_26C = revCode(bigFiveF_26, 5),
        bigFiveF_28C = revCode(bigFiveF_28, 5),
        bigFiveF_29C = revCode(bigFiveF_29, 5),
        bigFiveF_30C = revCode(bigFiveF_30, 5),
        bigFiveF_31C = revCode(bigFiveF_31, 5),
        bigFiveF_36C = revCode(bigFiveF_36, 5),
        bigFiveF_37C = revCode(bigFiveF_37, 5),
        bigFiveF_42C = revCode(bigFiveF_42, 5),
        bigFiveF_44C = revCode(bigFiveF_44, 5),
        bigFiveF_45C = revCode(bigFiveF_45, 5),
        bigFiveF_47C = revCode(bigFiveF_47, 5),
        bigFiveF_48C = revCode(bigFiveF_48, 5),
        bigFiveF_49C = revCode(bigFiveF_49, 5),
        bigFiveF_50C = revCode(bigFiveF_50, 5),
        bigFiveF_51C = revCode(bigFiveF_51, 5),
        bigFiveF_55C = revCode(bigFiveF_55, 5),
        bigFiveF_58C = revCode(bigFiveF_58, 5)
    )

### get total score ----

data01 <- data01 %>%
    mutate(
        bigFiveF_E1 = bigFiveF_1 + bigFiveF_16C + bigFiveF_31C + bigFiveF_46,
        bigFiveF_E2 = bigFiveF_6 + bigFiveF_21 + bigFiveF_36C + bigFiveF_51C,
        bigFiveF_E3 = bigFiveF_11C + bigFiveF_26C + bigFiveF_41 + bigFiveF_56,
        bigFiveF_A1 = bigFiveF_2 + bigFiveF_17C + bigFiveF_32 + bigFiveF_47C,
        bigFiveF_A2 = bigFiveF_7 + bigFiveF_22C + bigFiveF_37C + bigFiveF_52,
        bigFiveF_A3 = bigFiveF_12C + bigFiveF_27 + bigFiveF_42C + bigFiveF_57,
        bigFiveF_C1 = bigFiveF_3C + bigFiveF_18 + bigFiveF_33 + bigFiveF_48C,
        bigFiveF_C2 = bigFiveF_8C + bigFiveF_23C + bigFiveF_38 + bigFiveF_53,
        bigFiveF_C3 = bigFiveF_13 + bigFiveF_28C + bigFiveF_43 + bigFiveF_58C,
        bigFiveF_N1 = bigFiveF_4C + bigFiveF_19 + bigFiveF_34 + bigFiveF_49C,
        bigFiveF_N2 = bigFiveF_9C + bigFiveF_24C + bigFiveF_39 + bigFiveF_54,
        bigFiveF_N3 = bigFiveF_14 + bigFiveF_29C + bigFiveF_44C + bigFiveF_59,
        bigFiveF_O1 = bigFiveF_10 + bigFiveF_25C + bigFiveF_40 + bigFiveF_55C,
        bigFiveF_O2 = bigFiveF_5C + bigFiveF_20 + bigFiveF_35 + bigFiveF_50C,
        bigFiveF_O3 = bigFiveF_15 + bigFiveF_30C + bigFiveF_45C + bigFiveF_60,
        bigFiveF_E = bigFiveF_E1 + bigFiveF_E2 + bigFiveF_E3,
        bigFiveF_A = bigFiveF_A1 + bigFiveF_A2 + bigFiveF_A3,
        bigFiveF_C = bigFiveF_C1 + bigFiveF_C2 + bigFiveF_C3,
        bigFiveF_N = bigFiveF_N1 + bigFiveF_N2 + bigFiveF_N3,
        bigFiveF_O = bigFiveF_O1 + bigFiveF_O2 + bigFiveF_O3
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

## VVIQ ----

data01$VVIQtotal <- data01 %>%
    select(starts_with("VV")) %>%
    rowSums(., na.rm = T)

## SAQ ----

data01 <- data01 %>%
    mutate(
        SAQ_I_Total	= (SAQ01 + SAQ04 + SAQ07 + SAQ11 + SAQ13 + SAQ15 + SAQ20 + SAQ24) - 8,
        SAQ_I_Prop	= SAQ_I_Total/32,
        SAQ_N_Total	= (SAQ02 + SAQ06 + SAQ08 + SAQ10 + SAQ14 + SAQ18 + SAQ22 + SAQ23) - 8,
        SAQ_N_Prop	= SAQ_N_Total/32,
        SAQ_M_Total	= (SAQ03 + SAQ05 + SAQ09 + SAQ12 + SAQ16 + SAQ17 + SAQ19 + SAQ21) - 8,
        SAQ_M_Prop	= SAQ_M_Total/32,
        SAQ_Total	= SAQ_I_Total + SAQ_N_Total + SAQ_M_Total,
        SAQ_Prop	= SAQ_Total/96,
        
    )

## ASQ ----

data01 <- data01 %>%
    mutate(
        ASQ_Total = rowSums(select(data01, starts_with("ASQ") & ends_with("_1")), na.rm = T),
        ASQ_Prop = ASQ_Total/340
    )

# Compile clean data ----

cleanData <- data01 %>%
    select(id, ResponseId, duration, ConsentForm, gender, ptFirst,
           starts_with("D0", ignore.case = F), keyChoice, handChoice,
           grep('bigFiveF_[[:alpha:]]', colnames(data01)),
           grep('SAQ_*_', colnames(data01)),
           grep('ASQ_[[:alpha:]]', colnames(data01)),
           VVIQtotal,
           grep('FB_[[:alpha:]]', colnames(data01)),
           starts_with("FB") & ends_with("C"),
           starts_with("FB") & ends_with("Submit"),
           FBOtotal, FBStotal, FBOprop, FBSprop,
           PTLtotal, PTRtotal, PTOtotal, PTStotal, PTOprop, PTSprop,
           PTOrtMedian, PTSrtMedian,
           starts_with("PT") & ends_with("Submit"),
           starts_with("PT") & ends_with("_C")
    )

# Export clean data ----

write.csv(cleanData, file = "data/study01_cleanData.csv", row.names = F)
