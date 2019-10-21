data.cleansing <- function() {
  liver_patient$Gender <- as.integer(liver_patient$Gender)      # 1 - Female | 2 - Male
  
  for (x in 1:nrow(liver_patient)) {
    row1 <- liver_patient[x,]
    # print(row1[10])
    if (is.na(row1[10])) {              # 10 corresponds to Albumin_and_Globulin_Ratio which has NA in dataset
      age <- row1[1] %/% 10             # 1 corresponds to Age
      age = age*10
      print(paste("Age limit: ", age, "-", age+10))
      average = 0
      count = 0
      for (y in 1:nrow(liver_patient)) {
        if (y != x) {
          row2 <- liver_patient[y,]
          if (row2[1] >= age & row2[1] <= age+10 ) {
            average = average + row2[10]
            count = count + 1
          }
        }
      }
      average = average / count
      # print(round(average, 1))
      liver_patient[x,][10] <- round(average, 1)
      print(liver_patient[x,][10])
    }
  }
}

main <- function() {
  print(typeof(liver_patient))
  
  data.cleansing()
  # print(head(liver_patient))
  print(summary(liver_patient))
  
  liver.data <- with(liver_patient, data.frame(Age,
                                               Gender = as.integer(Gender),
                                               TotBilirubin = Total_Bilirubin,
                                               DirBilirubin = Direct_Bilirubin,
                                               AlkPhosph = Alkaline_Phosphotase,
                                               AlamAmino = Alamine_Aminotransferase,
                                               AspAmino = Aspartate_Aminotransferase,
                                               TotProteins=Total_Protiens,
                                               Albumin,
                                               AlbuminGlobulinRatio = Albumin_and_Globulin_Ratio,
                                               Result=Dataset))
  
  print(typeof(liver.data))
}


liver_patient <- read.csv("./assets/indian_liver_patient.csv")
main()


