############################################
## UNIWERSALNY SYSTEM CZYSZCZENIA DANYCH W R
############################################

clean_character <- function(df) {

  df$Sex   <- tolower(df$Sex)
  df$Stage <- tolower(df$Stage)

  df$Sex[df$Sex %in% c("female", "k", "kobieta")] <- "K"
  df$Sex[df$Sex %in% c("male", "m", "mezczyzna")] <- "M"

  df$Stage[df$Stage == "1"] <- "i"
  df$Stage[df$Stage == "2"] <- "ii"
  df$Stage[df$Stage == "3"] <- "iii"
  df$Stage[df$Stage == "4"] <- "iv"

  df$VitalStatus[df$VitalStatus %in% c("death", "zgon")] <- "0"
  df$VitalStatus[df$VitalStatus %in% c("alive", "zyje")] <- "1"

  return(df)
}


clean_numeric <- function(df) {
  df[df$ObservationTime >= 0 & df$ObservationTime <= 100, ]
}

clean_na = function(x){
  my.data = my.data[rowSums(my.data == "" | is.na(my.data)) == 0,]
}

my.data <- clean_character(my.data)
my.data <- clean_na(my.data)
my.data <- clean_numeric(my.data)