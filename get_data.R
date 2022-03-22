library(magrittr)
library(dplyr)
library(ggplot2)
get_data <- function() {
  ori_data <- readxl::read_excel('data/hand_injury_data.xlsx')
  data <- ori_data %>%
    mutate(
      ryrq = as.POSIXct(ryrq %>% as.numeric %>% divide_by(1000), origin = "1970-01-01 00:00:00"),
      cyrq = as.POSIXct(cyrq %>% as.numeric %>% divide_by(1000), origin =
                          "1970-01-01 00:00:00"),
      LOS = difftime(cyrq, ryrq, units = 'days'),
      injured_time = surg_wait_time %>% as.numeric + injured_time_before_in_hospital %>% as.numeric
    ) %>%
    filter(injury_site %in% c('hand', 'wrist')) %>%
    subset(
      select = c(
        age,
        alcohol_abuse,
        anethsia,
        bone_injured,
        diabetes,
        hypertension,
        injured_time,
        muscle_injured,
        nerve_injured,
        payment,
        Sex,
        smoking_history,
        surg_duration,
        tendon_injured,
        tourniquet_use,
        vessel_injured,
        `检验--WBC--白细胞`,
        `检验--RBC--红细胞`,
        `检验--HGB--血红蛋白`,
        `检验--NE#--中性粒细胞`,
        LOS
      )
    ) %>%
    setNames(
      c(
        'Age',
        'Drinking',
        'Anesthesia',
        'Bone injury',
        'Diabetes',
        'Hypertension',
        'Time to surgery',
        'Muscle injury',
        'Nerve injury',
        'Payment',
        'Sex',
        'Smoking',
        'Operation time',
        'Tendon injury',
        'Tourniquet use',
        'Vessel injury',
        'WBC',
        'RBC',
        'HGB',
        'NE#',
        'LOS'
      )
    )
  data %>% colnames %>% lapply(function(col) {
    sum(is.na(data[col]))
  }) %>% data.frame %>% setNames(colnames(data))
  for (colname in colnames(data)) {
    data[colname][is.na(data[colname])] <- median(data[colname])
  }
  data %<>%
    mutate(Age = Age %>% as.numeric()) %>%
    filter(Age < 80) %>%
    mutate(
      `Drinking` = `Drinking` %>% factor(levels = c('否', '是'), labels = c("No", "Yes")),
      Anesthesia = Anesthesia %>% grepl('^全身', .) %>% factor(labels = c(
        'Local anesthesia', 'General anesthesia'
      )),
      `Bone injury` = `Bone injury` %>% factor(levels = c('否', '是'), labels =
                                                 c("No", "Yes")),
      Diabetes = Diabetes %>% factor(levels = c('否', '是'), labels = c("No", "Yes")),
      Hypertension = Hypertension %>% factor(levels = c('否', '是'), labels =
                                               c("No", "Yes")),
      `Time to surgery` = `Time to surgery` %>% as.numeric,
      `Muscle injury` = `Muscle injury` %>% factor(levels = c('否', '是'), labels =
                                                     c("No", "Yes")),
      `Nerve injury` = `Nerve injury` %>% factor(levels = c('否', '是'), labels =
                                                   c("No", "Yes")),
      Payment = Payment %>% factor(
        levels = c(
          "自费",
          "自费欠费",
          "商业保险",
          "城镇医保",
          "新农合",
          "省医保",
          "武汉市职保",
          "武汉市居保",
          "江汉油田",
          "省公医(一般)",
          "武汉市大学生医保",
          "市居保精准扶贫",
          "省级转诊新农合"
        ),
        labels = c(
          "Self-paying",
          "Self-paying",
          "Commercial insurance",
          rep('National medical insurance', 10)
        )
      ),
      Sex = Sex %>% grepl('男', .) %>% factor(labels = c('Female', 'Male')),
      `Smoking` = `Smoking` %>% factor(levels = c('否', '是'), labels = c("No", "Yes")),
      `Operation time` = `Operation time` %>% as.numeric(),
      `Tendon injury` = `Tendon injury` %>% factor(levels = c('否', '是'), labels =
                                                     c("No", "Yes")),
      `Tourniquet use` = `Tourniquet use` %>% factor(levels = c('否', '是'), labels =
                                                       c("No", "Yes")),
      `Vessel injury` = `Vessel injury` %>% factor(levels = c('否', '是'), labels =
                                                     c("No", "Yes")),
      WBC = WBC %>% as.numeric %>% cut(
        breaks = c(-Inf, 3.5, 9.5, Inf),
        labels = c('Abnormal', 'Normal', 'Abnormal')
      ) %>% factor(levels = c('Normal', 'Abnormal')),
      RBC = RBC %>% as.numeric %>% cut(
        breaks = c(-Inf, 3.8, 5.1, Inf),
        labels = c('Abnormal', 'Normal', 'Abnormal')
      ) %>% factor(levels = c('Normal', 'Abnormal')),
      HGB = HGB %>% as.numeric %>% cut(
        breaks = c(-Inf, 115, 150, Inf),
        labels = c('Abnormal', 'Normal', 'Abnormal')
      ) %>% factor(levels = c('Normal', 'Abnormal')),
      `NE#` = `NE#` %>% as.numeric %>% cut(
        breaks = c(-Inf, 1.8, 6.3, Inf),
        labels = c('Abnormal', 'Normal', 'Abnormal')
      ) %>% factor(levels = c('Normal', 'Abnormal')),
      LOS = LOS %>% as.numeric() %>% floor()
    )
  data$LOS %>% quantile(probs = 0.5) # 7
  data$LOS %<>% cut(breaks = c(-Inf, 7, Inf),
                    labels = c('Short', 'Prolonged'))
  data
}
