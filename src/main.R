# source("./src/data_visualization.R")

get.average <- function(row1, x) {
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
  return(average)
}

main <- function() {
  print(typeof(liver_patient))
  
  for (x in 1:nrow(liver_patient)) {            # Creating valid data value for NA entries
    row <- liver_patient[x,]
    if (is.na(row[10])) {
      average <- get.average(row, x)
      liver_patient[x,][10] <- average
      print(liver_patient[x,][10])
    }
  }
  
  # print(head(liver_patient))
  # print(summary(liver_patient))
  
  liver.data <- with(liver_patient, data.frame(Age,
                                               Gender = factor(Gender, levels=c("Female", "Male"), labels=c("Female", "Male")),
                                               TotBrubin = Total_Bilirubin,
                                               DirBrubin = Direct_Bilirubin,
                                               AlkPhosph = Alkaline_Phosphotase,
                                               AlamAmino = Alamine_Aminotransferase,
                                               AspAmino = Aspartate_Aminotransferase,
                                               TotProt=Total_Protiens,
                                               Albumin,
                                               AlbGlobRat = Albumin_and_Globulin_Ratio,
                                               Result=factor(Dataset, levels=c(1, 2), labels=c("Yes", "No"))))
  
  print(typeof(liver.data))
  print(summary(liver.data))
  

  # visualize(liver.data)
}


liver_patient <- read.csv("./assets/indian_liver_patient.csv")
main()


