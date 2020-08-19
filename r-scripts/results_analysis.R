library(dplyr)
library(readr)

results <-
  read_csv("C:/Users/marcu/TRG/t2f/Analysis/qgis/revised/results.csv")

gt1 <- results %>% filter(groundtype == 1)
gt2 <- results %>% filter(groundtype == 2)
gt3 <- results %>% filter(groundtype == 3)

interventions <- c('softpads120', 'usp100', 'usp250', 'mat27')

item <-
  c(
    'within 200m',
    'within 80m',
    '<30dB(A)',
    '>=30dB(A)',
    '>=30 <35dB(A)',
    '>=35 <40dB(A)',
    '>=40 <45dB(A)',
    '>=45 <50dB(A)',
    '>=50 <55dB(A)',
    '>=55 <60dB(A)',
    'net dB(A) change (30dB and above)',
    'value(£)2002'
  )

### gt1

summary_gt1 <- as.data.frame(item, stringsAsFactors = FALSE)

summary_gt1[1, "reference"] <- sum(gt1$pop)
summary_gt1[2, "reference"] <- gt1 %>% filter(distance <= 80) %>%
  summarise(sum(pop))
summary_gt1[3, "reference"] <- gt1 %>% filter(reference < 30) %>%
  summarise(sum(pop))
summary_gt1[4, "reference"] <-
  gt1 %>% filter(reference >= 30) %>%
  summarise(sum(pop))
summary_gt1[5, "reference"] <-
  gt1 %>% filter(reference >= 30 & reference < 35) %>%
  summarise(sum(pop))
summary_gt1[6, "reference"] <-
  gt1 %>% filter(reference >= 35 & reference < 40) %>%
  summarise(sum(pop))
summary_gt1[7, "reference"] <-
  gt1 %>% filter(reference >= 40 & reference < 45) %>%
  summarise(sum(pop))
summary_gt1[8, "reference"] <-
  gt1 %>% filter(reference >= 45 & reference < 50) %>%
  summarise(sum(pop))
summary_gt1[9, "reference"] <-
  gt1 %>% filter(reference >= 50 & reference < 55) %>%
  summarise(sum(pop))
summary_gt1[10, "reference"] <-
  gt1 %>% filter(reference >= 55 & reference < 60) %>%
  summarise(sum(pop))

# This code is to determine the change in total noise level (increase or decrease) that occurs 
# in each of the noise band. In this case the highest band predicted was 55-60, so don't need to
# consider beyond that.
# There really must be a better way of calculating this - perhaps ask a computer scientist!

for (name in interventions) {
  temp <-
    gt1 %>% select(pop, distance, reference, intervention = !!name)
  
  # process decrease or no change
  for (i in 1:nrow(temp)) {
    ref <- temp$reference[i]
    int <- temp$intervention[i]
    
    # Only where a decrease or no change - intervention below reference
    # Only interested where starting point is >=30
    if (ref >= 30 & int <= ref) {
      # if level is below 30 after intervention
      # set level to 30 - not interested in improvements below 30
      int <- ifelse(int >= 30, int, 30)
      dif <- int - ref
      
      if (ref >= 55 & int < 35) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 55 & int >= 35 & int < 40) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 55 & int >= 40 & int < 45) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 45 & int < 50) {
        a <- 55 - ref
        b <- -5
        c <- int - 50
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 50 & int < 55) {
        a <- 55 - ref
        b <- int - 55
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 55 & int < 60) {
        a <- int - ref
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 50 & int < 35) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 50 & int >= 35 & int < 40) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 50 & int >= 40 & int < 45) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 50 & int >= 45 & int < 50) {
        a <- 0
        b <- 50 - ref
        c <- int - 50
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 50 & int >= 50 & int < 55) {
        a <- 0
        b <- int - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 45 & int < 35) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 45 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 45 & int >= 40 & int < 45) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 45 & int >= 45 & int < 50) {
        a <- 0
        b <- 0
        c <- int - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 40 & int < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 40 - ref
        e <- -5
        f <- int - 35
      } else if (ref >= 40 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 40 - ref
        e <- int - 40
        f <- 0
      } else if (ref >= 40 & int >= 40 & int < 45) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - ref
        e <- 0
        f <- 0
      } else if (ref >= 35 & int < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 35 - ref
        f <- int - 35
      } else if (ref >= 35 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - ref
        f <- 0
      } else {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- int - ref
      }
      
      temp[i, "dif"] <- dif
      temp[i, "t_60"] <- a
      temp[i, "t_55"] <- b
      temp[i, "t_50"] <- c
      temp[i, "t_45"] <- d
      temp[i, "t_40"] <- e
      temp[i, "t_35"] <- f
      temp[i, "check"] <- isTRUE(dif == a + b + c + d + e + f)
      
    }
    
    # process increases
    # only interested where level is higher after intervention
    # and only interested where level after intervention is >=30
    if (int >= 30 & int > ref) {
      # if the reference level is below 30 then raise it up to 30
      # we are not interested in increases happening below 30
      ref <- ifelse(ref >= 30, ref, 30)
      dif <- int - ref
      
      if (int >= 55 & ref < 35) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 55 & ref >= 35 & ref < 40) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 55 & ref >= 40 & ref < 45) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 45 & ref < 50) {
        a <- int - 55
        b <- 5
        c <- 50 - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 50 & ref < 55) {
        a <- int - 55
        b <- 55 - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 55 & ref < 60) {
        a <- int - ref
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 50 & ref < 35) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 50 & ref >= 35 & ref < 40) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 50 & ref >= 40 & ref < 45) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 50 & ref >= 45 & ref < 50) {
        a <- 0
        b <- int - 50
        c <- 50 - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 50 & ref >= 50 & ref < 55) {
        a <- 0
        b <- int - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 45 & ref < 35) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 45 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 45 & ref >= 40 & ref < 45) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 45 & ref >= 45 & ref < 50) {
        a <- 0
        b <- 0
        c <- int - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 40 & ref < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - 40
        e <- 5
        f <- 35 - ref
      } else if (int >= 40 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - 40
        e <- 40 - ref
        f <- 0
      } else if (int >= 40 & ref >= 40 & ref < 45) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - ref
        e <- 0
        f <- 0
      } else if (int >= 35 & ref < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - 35
        f <- 35 - ref
      } else if (int >= 35 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - ref
        f <- 0
      } else {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- int - ref
      }
      
      temp[i, "dif"] <- dif
      temp[i, "t_60"] <- a
      temp[i, "t_55"] <- b
      temp[i, "t_50"] <- c
      temp[i, "t_45"] <- d
      temp[i, "t_40"] <- e
      temp[i, "t_35"] <- f
      temp[i, "check"] <- isTRUE(dif == a + b + c + d + e + f)
      
      if (nrow(filter(temp, check == FALSE)) > 0) {
        warning(paste("error in ", name))
      }
      
    }
    
    
  }
  
  temp <-
    temp %>% mutate(value = (t_35 * 5.8 * pop) + (t_40 * 11.4 * pop) + (t_45 * 17.0 * pop) + (t_50 * 22.6 * pop) + (t_55 * 28.1 * pop) + (t_60 * 33.7 * pop))
  
  summary_gt1[1, paste(name)] <- sum(temp$pop)
  
  summary_gt1[2, paste(name)] <- temp %>% filter(distance <= 80) %>%
    summarise(sum(pop))
  
  summary_gt1[3, paste(name)] <-
    temp %>% filter(intervention < 30) %>%
    summarise(sum(pop))
  
  summary_gt1[4, paste(name)] <-
    temp %>% filter(intervention >= 30) %>%
    summarise(sum(pop))
  
  summary_gt1[5, paste(name)] <-
    temp %>% filter(intervention >= 30 & intervention < 35) %>%
    summarise(sum(pop))
  
  summary_gt1[6, paste(name)] <-
    temp %>% filter(intervention >= 35 & intervention < 40) %>%
    summarise(sum(pop))
  
  summary_gt1[7, paste(name)] <-
    temp %>% filter(intervention >= 40 & intervention < 45) %>%
    summarise(sum(pop))
  
  summary_gt1[8, paste(name)] <-
    temp %>% filter(intervention >= 45 & intervention < 50) %>%
    summarise(sum(pop))
  
  summary_gt1[9, paste(name)] <-
    temp %>% filter(intervention >= 50 & intervention < 55) %>%
    summarise(sum(pop))
  
  summary_gt1[10, paste(name)] <-
    temp %>% filter(intervention >= 55 & intervention < 60) %>%
    summarise(sum(pop))
  
  summary_gt1[11, paste(name)] <-
    temp %>% summarise(sum(dif * pop, na.rm = TRUE))
  
  summary_gt1[12, paste(name)] <-
    temp %>% summarise(sum(value, na.rm = TRUE))
  
}

summary_gt1 <- summary_gt1 %>% mutate_if(is.numeric, round, 2)
write.csv(file = "../qgis/revised/summary_gt1_new.csv", summary_gt1)


### gt2

summary_gt2 <- as.data.frame(item, stringsAsFactors = FALSE)

summary_gt2[1, "reference"] <- sum(gt2$pop)
summary_gt2[2, "reference"] <- gt2 %>% filter(distance <= 80) %>%
  summarise(sum(pop))
summary_gt2[3, "reference"] <- gt2 %>% filter(reference < 30) %>%
  summarise(sum(pop))
summary_gt2[4, "reference"] <-
  gt2 %>% filter(reference >= 30) %>%
  summarise(sum(pop))
summary_gt2[5, "reference"] <-
  gt2 %>% filter(reference >= 30 & reference < 35) %>%
  summarise(sum(pop))
summary_gt2[6, "reference"] <-
  gt2 %>% filter(reference >= 35 & reference < 40) %>%
  summarise(sum(pop))
summary_gt2[7, "reference"] <-
  gt2 %>% filter(reference >= 40 & reference < 45) %>%
  summarise(sum(pop))
summary_gt2[8, "reference"] <-
  gt2 %>% filter(reference >= 45 & reference < 50) %>%
  summarise(sum(pop))
summary_gt2[9, "reference"] <-
  gt2 %>% filter(reference >= 50 & reference < 55) %>%
  summarise(sum(pop))
summary_gt2[10, "reference"] <-
  gt2 %>% filter(reference >= 55 & reference < 60) %>%
  summarise(sum(pop))


for (name in interventions) {
  temp <-
    gt2 %>% select(pop, distance, reference, intervention = !!name)
  
  # process decrease or no change
  for (i in 1:nrow(temp)) {
    ref <- temp$reference[i]
    int <- temp$intervention[i]
    
    # Only where a decrease or no change - intervention below reference
    # Only interested where starting point is >=30
    if (ref >= 30 & int <= ref) {
      # if level is below 30 after intervention
      # set level to 30 - not interested in improvements below 30
      int <- ifelse(int >= 30, int, 30)
      dif <- int - ref
      
      if (ref >= 55 & int < 35) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 55 & int >= 35 & int < 40) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 55 & int >= 40 & int < 45) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 45 & int < 50) {
        a <- 55 - ref
        b <- -5
        c <- int - 50
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 50 & int < 55) {
        a <- 55 - ref
        b <- int - 55
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 55 & int < 60) {
        a <- int - ref
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 50 & int < 35) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 50 & int >= 35 & int < 40) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 50 & int >= 40 & int < 45) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 50 & int >= 45 & int < 50) {
        a <- 0
        b <- 50 - ref
        c <- int - 50
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 50 & int >= 50 & int < 55) {
        a <- 0
        b <- int - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 45 & int < 35) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 45 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 45 & int >= 40 & int < 45) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 45 & int >= 45 & int < 50) {
        a <- 0
        b <- 0
        c <- int - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 40 & int < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 40 - ref
        e <- -5
        f <- int - 35
      } else if (ref >= 40 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 40 - ref
        e <- int - 40
        f <- 0
      } else if (ref >= 40 & int >= 40 & int < 45) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - ref
        e <- 0
        f <- 0
      } else if (ref >= 35 & int < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 35 - ref
        f <- int - 35
      } else if (ref >= 35 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - ref
        f <- 0
      } else {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- int - ref
      }
      
      temp[i, "dif"] <- dif
      temp[i, "t_60"] <- a
      temp[i, "t_55"] <- b
      temp[i, "t_50"] <- c
      temp[i, "t_45"] <- d
      temp[i, "t_40"] <- e
      temp[i, "t_35"] <- f
      temp[i, "check"] <- isTRUE(dif == a + b + c + d + e + f)
      
    }
    
    # process increases
    # only interested where level is higher after intervention
    # and only interested where level after intervention is >=30
    if (int >= 30 & int > ref) {
      # if the reference level is below 30 then raise it up to 30
      # we are not interested in increases happening below 30
      ref <- ifelse(ref >= 30, ref, 30)
      dif <- int - ref
      
      if (int >= 55 & ref < 35) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 55 & ref >= 35 & ref < 40) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 55 & ref >= 40 & ref < 45) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 45 & ref < 50) {
        a <- int - 55
        b <- 5
        c <- 50 - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 50 & ref < 55) {
        a <- int - 55
        b <- 55 - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 55 & ref < 60) {
        a <- int - ref
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 50 & ref < 35) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 50 & ref >= 35 & ref < 40) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 50 & ref >= 40 & ref < 45) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 50 & ref >= 45 & ref < 50) {
        a <- 0
        b <- int - 50
        c <- 50 - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 50 & ref >= 50 & ref < 55) {
        a <- 0
        b <- int - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 45 & ref < 35) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 45 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 45 & ref >= 40 & ref < 45) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 45 & ref >= 45 & ref < 50) {
        a <- 0
        b <- 0
        c <- int - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 40 & ref < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - 40
        e <- 5
        f <- 35 - ref
      } else if (int >= 40 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - 40
        e <- 40 - ref
        f <- 0
      } else if (int >= 40 & ref >= 40 & ref < 45) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - ref
        e <- 0
        f <- 0
      } else if (int >= 35 & ref < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - 35
        f <- 35 - ref
      } else if (int >= 35 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - ref
        f <- 0
      } else {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- int - ref
      }
      
      temp[i, "dif"] <- dif
      temp[i, "t_60"] <- a
      temp[i, "t_55"] <- b
      temp[i, "t_50"] <- c
      temp[i, "t_45"] <- d
      temp[i, "t_40"] <- e
      temp[i, "t_35"] <- f
      temp[i, "check"] <- isTRUE(dif == a + b + c + d + e + f)
      
      if (nrow(filter(temp, check == FALSE)) > 0) {
        warning(paste("error in ", name))
      }
      
    }
    
    
  }
  
  temp <-
    temp %>% mutate(value = (t_35 * 5.8 * pop) + (t_40 * 11.4 * pop) + (t_45 * 17.0 * pop) + (t_50 * 22.6 * pop) + (t_55 * 28.1 * pop) + (t_60 * 33.7 * pop))
  
  summary_gt2[1, paste(name)] <- sum(temp$pop)
  
  summary_gt2[2, paste(name)] <- temp %>% filter(distance <= 80) %>%
    summarise(sum(pop))
  
  summary_gt2[3, paste(name)] <-
    temp %>% filter(intervention < 30) %>%
    summarise(sum(pop))
  
  summary_gt2[4, paste(name)] <-
    temp %>% filter(intervention >= 30) %>%
    summarise(sum(pop))
  
  summary_gt2[5, paste(name)] <-
    temp %>% filter(intervention >= 30 & intervention < 35) %>%
    summarise(sum(pop))
  
  summary_gt2[6, paste(name)] <-
    temp %>% filter(intervention >= 35 & intervention < 40) %>%
    summarise(sum(pop))
  
  summary_gt2[7, paste(name)] <-
    temp %>% filter(intervention >= 40 & intervention < 45) %>%
    summarise(sum(pop))
  
  summary_gt2[8, paste(name)] <-
    temp %>% filter(intervention >= 45 & intervention < 50) %>%
    summarise(sum(pop))
  
  summary_gt2[9, paste(name)] <-
    temp %>% filter(intervention >= 50 & intervention < 55) %>%
    summarise(sum(pop))
  
  summary_gt2[10, paste(name)] <-
    temp %>% filter(intervention >= 55 & intervention < 60) %>%
    summarise(sum(pop))
  
  summary_gt2[11, paste(name)] <-
    temp %>% summarise(sum(dif * pop, na.rm = TRUE))
  
  summary_gt2[12, paste(name)] <-
    temp %>% summarise(sum(value, na.rm = TRUE))
  
}

summary_gt2 <- summary_gt2 %>% mutate_if(is.numeric, round, 2)
write.csv(file = "../qgis/revised/summary_gt2_new.csv", summary_gt2)

## gt3

summary_gt3 <- as.data.frame(item, stringsAsFactors = FALSE)

summary_gt3[1, "reference"] <- sum(gt3$pop)
summary_gt3[2, "reference"] <- gt3 %>% filter(distance <= 80) %>%
  summarise(sum(pop))
summary_gt3[3, "reference"] <- gt3 %>% filter(reference < 30) %>%
  summarise(sum(pop))
summary_gt3[4, "reference"] <-
  gt3 %>% filter(reference >= 30) %>%
  summarise(sum(pop))
summary_gt3[5, "reference"] <-
  gt3 %>% filter(reference >= 30 & reference < 35) %>%
  summarise(sum(pop))
summary_gt3[6, "reference"] <-
  gt3 %>% filter(reference >= 35 & reference < 40) %>%
  summarise(sum(pop))
summary_gt3[7, "reference"] <-
  gt3 %>% filter(reference >= 40 & reference < 45) %>%
  summarise(sum(pop))
summary_gt3[8, "reference"] <-
  gt3 %>% filter(reference >= 45 & reference < 50) %>%
  summarise(sum(pop))
summary_gt3[9, "reference"] <-
  gt3 %>% filter(reference >= 50 & reference < 55) %>%
  summarise(sum(pop))
summary_gt3[10, "reference"] <-
  gt3 %>% filter(reference >= 55 & reference < 60) %>%
  summarise(sum(pop))


for (name in interventions) {
  temp <-
    gt3 %>% select(pop, distance, reference, intervention = !!name)
  
  # process decrease or no change
  for (i in 1:nrow(temp)) {
    ref <- temp$reference[i]
    int <- temp$intervention[i]
    
    # Only where a decrease or no change - intervention below reference
    # Only interested where starting point is >=30
    if (ref >= 30 & int <= ref) {
      # if level is below 30 after intervention
      # set level to 30 - not interested in improvements below 30
      int <- ifelse(int >= 30, int, 30)
      dif <- int - ref
      
      if (ref >= 55 & int < 35) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 55 & int >= 35 & int < 40) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 55 & int >= 40 & int < 45) {
        a <- 55 - ref
        b <- -5
        c <- -5
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 45 & int < 50) {
        a <- 55 - ref
        b <- -5
        c <- int - 50
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 50 & int < 55) {
        a <- 55 - ref
        b <- int - 55
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 55 & int >= 55 & int < 60) {
        a <- int - ref
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 50 & int < 35) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 50 & int >= 35 & int < 40) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 50 & int >= 40 & int < 45) {
        a <- 0
        b <- 50 - ref
        c <- -5
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 50 & int >= 45 & int < 50) {
        a <- 0
        b <- 50 - ref
        c <- int - 50
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 50 & int >= 50 & int < 55) {
        a <- 0
        b <- int - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 45 & int < 35) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- -5
        e <- -5
        f <- int - 35
      } else if (ref >= 45 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- -5
        e <- int - 40
        f <- 0
      } else if (ref >= 45 & int >= 40 & int < 45) {
        a <- 0
        b <- 0
        c <- 45 - ref
        d <- int - 45
        e <- 0
        f <- 0
      } else if (ref >= 45 & int >= 45 & int < 50) {
        a <- 0
        b <- 0
        c <- int - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (ref >= 40 & int < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 40 - ref
        e <- -5
        f <- int - 35
      } else if (ref >= 40 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 40 - ref
        e <- int - 40
        f <- 0
      } else if (ref >= 40 & int >= 40 & int < 45) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - ref
        e <- 0
        f <- 0
      } else if (ref >= 35 & int < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 35 - ref
        f <- int - 35
      } else if (ref >= 35 & int >= 35 & int < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - ref
        f <- 0
      } else {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- int - ref
      }
      
      temp[i, "dif"] <- dif
      temp[i, "t_60"] <- a
      temp[i, "t_55"] <- b
      temp[i, "t_50"] <- c
      temp[i, "t_45"] <- d
      temp[i, "t_40"] <- e
      temp[i, "t_35"] <- f
      temp[i, "check"] <- isTRUE(dif == a + b + c + d + e + f)
      
    }
    
    # process increases
    # only interested where level is higher after intervention
    # and only interested where level after intervention is >=30
    if (int >= 30 & int > ref) {
      # if the reference level is below 30 then raise it up to 30
      # we are not interested in increases happening below 30
      ref <- ifelse(ref >= 30, ref, 30)
      dif <- int - ref
      
      if (int >= 55 & ref < 35) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 55 & ref >= 35 & ref < 40) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 55 & ref >= 40 & ref < 45) {
        a <- int - 55
        b <- 5
        c <- 5
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 45 & ref < 50) {
        a <- int - 55
        b <- 5
        c <- 50 - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 50 & ref < 55) {
        a <- int - 55
        b <- 55 - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 55 & ref >= 55 & ref < 60) {
        a <- int - ref
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 50 & ref < 35) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 50 & ref >= 35 & ref < 40) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 50 & ref >= 40 & ref < 45) {
        a <- 0
        b <- int - 50
        c <- 5
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 50 & ref >= 45 & ref < 50) {
        a <- 0
        b <- int - 50
        c <- 50 - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 50 & ref >= 50 & ref < 55) {
        a <- 0
        b <- int - ref
        c <- 0
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 45 & ref < 35) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 5
        e <- 5
        f <- 35 - ref
      } else if (int >= 45 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 5
        e <- 40 - ref
        f <- 0
      } else if (int >= 45 & ref >= 40 & ref < 45) {
        a <- 0
        b <- 0
        c <- int - 45
        d <- 45 - ref
        e <- 0
        f <- 0
      } else if (int >= 45 & ref >= 45 & ref < 50) {
        a <- 0
        b <- 0
        c <- int - ref
        d <- 0
        e <- 0
        f <- 0
      } else if (int >= 40 & ref < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - 40
        e <- 5
        f <- 35 - ref
      } else if (int >= 40 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - 40
        e <- 40 - ref
        f <- 0
      } else if (int >= 40 & ref >= 40 & ref < 45) {
        a <- 0
        b <- 0
        c <- 0
        d <- int - ref
        e <- 0
        f <- 0
      } else if (int >= 35 & ref < 35) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - 35
        f <- 35 - ref
      } else if (int >= 35 & ref >= 35 & ref < 40) {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- int - ref
        f <- 0
      } else {
        a <- 0
        b <- 0
        c <- 0
        d <- 0
        e <- 0
        f <- int - ref
      }
      
      temp[i, "dif"] <- dif
      temp[i, "t_60"] <- a
      temp[i, "t_55"] <- b
      temp[i, "t_50"] <- c
      temp[i, "t_45"] <- d
      temp[i, "t_40"] <- e
      temp[i, "t_35"] <- f
      temp[i, "check"] <- isTRUE(dif == a + b + c + d + e + f)
      
      if (nrow(filter(temp, check == FALSE)) > 0) {
        warning(paste("error in ", name))
      }
      
    }
    
    
  }
  
  temp <-
    temp %>% mutate(value = (t_35 * 5.8 * pop) + (t_40 * 11.4 * pop) + (t_45 * 17.0 * pop) + (t_50 * 22.6 * pop) + (t_55 * 28.1 * pop) + (t_60 * 33.7 * pop))
  
  summary_gt3[1, paste(name)] <- sum(temp$pop)
  
  summary_gt3[2, paste(name)] <- temp %>% filter(distance <= 80) %>%
    summarise(sum(pop))
  
  summary_gt3[3, paste(name)] <-
    temp %>% filter(intervention < 30) %>%
    summarise(sum(pop))
  
  summary_gt3[4, paste(name)] <-
    temp %>% filter(intervention >= 30) %>%
    summarise(sum(pop))
  
  summary_gt3[5, paste(name)] <-
    temp %>% filter(intervention >= 30 & intervention < 35) %>%
    summarise(sum(pop))
  
  summary_gt3[6, paste(name)] <-
    temp %>% filter(intervention >= 35 & intervention < 40) %>%
    summarise(sum(pop))
  
  summary_gt3[7, paste(name)] <-
    temp %>% filter(intervention >= 40 & intervention < 45) %>%
    summarise(sum(pop))
  
  summary_gt3[8, paste(name)] <-
    temp %>% filter(intervention >= 45 & intervention < 50) %>%
    summarise(sum(pop))
  
  summary_gt3[9, paste(name)] <-
    temp %>% filter(intervention >= 50 & intervention < 55) %>%
    summarise(sum(pop))
  
  summary_gt3[10, paste(name)] <-
    temp %>% filter(intervention >= 55 & intervention < 60) %>%
    summarise(sum(pop))
  
  summary_gt3[11, paste(name)] <-
    temp %>% summarise(sum(dif * pop, na.rm = TRUE))
  
  summary_gt3[12, paste(name)] <-
    temp %>% summarise(sum(value, na.rm = TRUE))
  
}

summary_gt3 <- summary_gt3 %>% mutate_if(is.numeric, round, 2)
write.csv(file = "../qgis/revised/summary_gt3_new.csv", summary_gt3)