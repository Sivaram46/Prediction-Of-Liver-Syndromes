# source("./src/data_visualization.R")

get.average <- function(row1, x) {
  age <- row1[1] %/% 10             # 1 corresponds to Age
  age = age*10
  #print(paste("Age limit: ", age, "-", age+10))
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
  #print(typeof(liver_patient))
  
  for (x in 1:nrow(liver_patient)) {            # Creating valid data value for NA entries
    row <- liver_patient[x,]
    if (is.na(row[10])) {
      average <- get.average(row, x)
      liver_patient[x,][10] <- average
      # print(liver_patient[x,][10])
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
  
  # print(typeof(liver.data))
  # print(summary(liver.data))
  
  hypothesis_testing()
  
  
  # visualize(liver.data)
}

hypothesis_testing <- function()
{
  cat("Correlation between Alamine aminotransferase and Aspartate aminotransferase:\n")
  
  cor_1 = cor.test(liver_patient$Alamine_Aminotransferase, liver_patient$Aspartate_Aminotransferase)
  
  cat("Null hypothesis, H0: Correlation of two atributes is 0.\n")
  cat("Alternating hypothesis, H1: True correlation is non zer0.\n\n")
  
  cf = 0.95 # confidence interval

  # printing result of the testing  
  if(cor_1$p.value <= 1 - cf){
    cat("Alamine aminotransferase and Aspartate aminotransferase are correlated.\n")
    cat("Correlation: ", cor(liver_patient$Alamine_Aminotransferase, liver_patient$Aspartate_Aminotransferase), "\n\n")
  }else{
    cat("Alamine aminotransferase and Aspartate aminotransferase are not correlated.\n\n")
  }
  
  # plottings
  plot(liver_patient$Alamine_Aminotransferase, liver_patient$Aspartate_Aminotransferase, pch = 19, col = "blue",
       main = "Alamine  vs Aspartate ", xlab = "Alamine aminotransferase", ylab = "Aspartate aminotransferase"
  )
  abline(lm(liver_patient$Aspartate_Aminotransferase ~ liver_patient$Alamine_Aminotransferase, data = liver_patient), col = "red")

  
  cat("Correlation between Total bilirubin and Direct bilirubin:\n")
  
  cor_2 = cor.test(liver_patient$Total_Bilirubin, liver_patient$Direct_Bilirubin)
  
  cat("Null hypothesis, H0: Correlation of two atributes is 0.\n")
  cat("Alternating hypothesis, H1: True correlation is non zer0.\n\n")
  
  cf = 0.95 # confidence interval
  
  # printing result of the testing  
  if(cor_2$p.value <= 1 - cf){
    cat("Total bilirubin and Direct bilirubin are correlated.\n")
    cat("Correlation: ", cor(liver_patient$Total_Bilirubin, liver_patient$Direct_Bilirubin), "\n\n")
  }else{
    cat("Total bilirubin and Direct bilirubin are not correlated.\n\n")
  }
  
  # plottings
  plot(liver_patient$Total_Bilirubin, liver_patient$Direct_Bilirubin, pch = 19, col = "blue",
       main = "Total bilirubin vs direct bilirubin", xlab = "Total bilirubin", ylab = "Direct bilirubin"
  )
  abline(lm(liver_patient$Direct_Bilirubin ~ liver_patient$Total_Bilirubin, data = liver_patient), col = "red")
}

#liver_patient <- read.csv("./assets/indian_liver_patient.csv")
liver_patient <- read.csv("C:/Users/sivaram/Downloads/assets/indian_liver_patient.csv")
main()


