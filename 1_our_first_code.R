library(tidyverse)

#loading the file
raw.data<-read.csv("C:\\Users\\itayg\\OneDrive\\Documents\\STATS LAB\\rawdata.csv")


plot(table(raw.data$pregweek))
table(floor(raw.data$pregweek)) #number of observations per week
round(prop.table(table(floor(raw.data$pregweek))),3)*100 # the proportion of observations in every week
barplot(table(floor(raw.data$pregweek))) #showing the weeks in barplot


sum(raw.data$pregweek > 41)

data40 <- raw.data[raw.data$pregweek < 41, ]

# מחזיר את מספר הנשים השונות במדגם (ע"פ מזהה אישי snumber)
# כל אישה מופיעה רק פעם אחת בחישוב הזה גם אם יש לה כמה מדידות
length(unique(data40$snumber))

preg_id <- data40$snumber + data40$expected / 100000
length(unique(preg_id))

tab_preg_id <- table(preg_id)
table(tab_preg_id)

cat("Number of unique women:", length(unique(data40$snumber)), "\n")
cat("Number of unique pregnancies:", length(unique(preg_id)), "\n")
cat("Distribution of number of measurements per pregnancy:\n")
print(table(tab_preg_id))


bp = boxplot(data40$pregweek ~ is.na(data40$ab_cir),
        main = "Pregnancy Week by Missingness in ab_cir",
        xlab = "Is ab_cir Missing?",
        ylab = "Pregnancy Week")
summary(bp)


summary(data40$pregweek[is.na(data40$ab_cir)])   # for TRUE (missing)
summary(data40$pregweek[!is.na(data40$ab_cir)])  # for FALSE (not missing)
str(bp)
bp$stats

by(data40$ab_cir, floor(data40$pregweek), summary)


# סך הכל תצפיות בכל שבוע
by(data40$ab_cir, floor(data40$pregweek), length)

# תצפיות שאינן חסרות בכל שבוע
by(data40$ab_cir, floor(data40$pregweek), function(x) sum(!is.na(x)))

# תצפיות חסרות בכל שבוע (אחרי עיגול כלפי מטה)
by(data40$ab_cir, floor(data40$pregweek), function(x) sum(is.na(x)))


# שלב 1: נחשב כמה חסרים יש בכל שבוע (מעוגל כלפי מטה)
missing_by_week <- tapply(is.na(data40$ab_cir), floor(data40$pregweek), sum)

# שלב 2: מציירים ברפלוט
barplot(missing_by_week,
        main = "Number of Missing ab_cir Values by Pregnancy Week",
        xlab = "Pregnancy Week (rounded down)",
        ylab = "Number of Missing Values",
        col = "tomato")

# עיגול שבוע הריון כלפי מטה
weeks <- floor(data40$pregweek)

# סך הכל תצפיות בכל שבוע
total <- tapply(data40$ab_cir, weeks, length)

# תצפיות שאינן חסרות
non_missing <- tapply(data40$ab_cir, weeks, function(x) sum(!is.na(x)))

# תצפיות חסרות
missing <- tapply(data40$ab_cir, weeks, function(x) sum(is.na(x)))

# אחוז תצפיות חסרות (נכפיל ב-100)
missing_percent <- round(100 * missing / total, 2)

# מאחדים לטבלה
summary_table <- data.frame(
  Week = as.numeric(names(total)),
  Total = total,
  NonMissing = non_missing,
  Missing = missing,
  MissingPercent = missing_percent
)

# מציגים את הטבלה
print(summary_table)

barplot(summary_table$MissingPercent,
        names.arg = summary_table$Week,
        main = "Percentage of Missing ab_cir Values by Pregnancy Week",
        xlab = "Pregnancy Week",
        ylab = "Missing (%)",
        col = "steelblue")


data_clean <- data40[!is.na(data40$ab_cir) & data40$pregweek >= 19, ]
summary(data_clean$pregweek)

ac <- data_clean$ab_cir
weeks <- data_clean$pregweek

plot(weeks, ac,
     main = "Abdominal Circumference vs. Pregnancy Week (≥19)",
     xlab = "Pregnancy Week",
     ylab = "AC",
     pch = 20, col = "darkgreen", cex = 0.5)
# לא רואים כלום...

# ברור שיש טעויות ביחידות מידה, אז בואו נסתכל עד 50 כדי לראות את הסקייל של הקטנים

plot(data_clean$pregweek,
     data_clean$ab_cir,
     main = "ab_cir vs Pregnancy Week (Zoomed In: 0–50)",
     xlab = "Pregnancy Week",
     ylab = "ab_cir",
     cex = 0.4,
     ylim = c(0, 50))

# סקייל מ80 עד 450

plot(data_clean$pregweek,
     data_clean$ab_cir,
     main = "ab_cir vs Pregnancy Week (Zoomed In: 55–450)",
     xlab = "Pregnancy Week",
     ylab = "ab_cir",
     cex = 0.4,
     ylim = c(55, 450))


# עכשיו קל להבין שהסגמנט הבינוני נמדד במ"מ! הגיוני סך הכל כי סטנדרט קליני הוא במילימטרים 
# לעומת זאת אנחנו נעבוד עם נוסחאות הדלוק ובספרות האקדמית המדידה היא בסנטימטרים
# לכן נמיר את הסגמנט הבינוני לס"מ. נחלק ב10 



ac <- data_clean$ab_cir  

# יצירת עותק לעבודה, כדי לא לפגוע בנתוני המקור
ac_cleaned <- ac

# שלב 1: ערכים מאוד גדולים (כנראה שגיאת הקלדה -> לחלק ב-100)
ac_cleaned[ac_cleaned > 1000] <- NA

# שלב 2: ערכים בינוניים כנראה במילימטרים -> לחלק ב-10
to_convert_cm <- ac_cleaned > 0 & ac_cleaned <= 50 & !is.na(ac_cleaned)
ac_cleaned[to_convert_cm] <- ac_cleaned[to_convert_cm] * 10

# שלב 3: כל השאר - לא נוגעים (<= 80) כי סביר שהם כבר בס"מ

# עדכון בעמודת המקור או יצירת חדשה
data_clean$ac_mm <- ac_cleaned  # משתנה חדש עם נתונים מנורמלים לס"מ
data_clean <- data_clean[!is.na(data_clean$ac_mm), ]

# אפשר לשרטט לבדוק:
plot(data_clean$pregweek, data_clean$ac_mm,
     main = "Abdominal Circumference (Standardized to mm)",
     xlab = "Pregnancy Week", ylab = "AC (cm)", cex = 0.4)


summary(data_clean$ac_mm)
table(data_clean$ac_mm < 100)
table(data_clean$ac_mm > 425)
 
# בקיצור יש שתי תצפיות שהן מתחת ל5 סנטימטר
# וכן 6 תצפיות שהן מעל 60 סנטימטר

# אלו תוצאות לא הגיונות גם לעוברים קטנים מאוד ולעוברים גדולים מאוד.
# נשאיר? נוריד? מצריך דיון


table(data_clean$ac_mm > 425)

ac_mm_final = data_clean$ac_mm
pregweek = data_clean$pregweek

mean.AC <- 10*(-13.3+1.61*pregweek-0.00998*pregweek^2) # calculating the E(x) according to the given formulas
SD.AC <- 13.4 # calculating the sd according to the given formulas 
plot(pregweek,ac_mm_final,cex=0.4)
x <- seq(min(pregweek),max(pregweek),len=500)
mean.x <- 10*(-13.3 + 1.61*x -0.00998*x^2)
SD.x <- 13.4
lines(x,mean.x,col='tomato',cex=0.5,pch=16,lwd=3)
lines(x,mean.x-3*SD.x,col='tomato',lwd=3)
lines(x,mean.x+3*SD.x,col='tomato',lwd=3)



accept.p <- function(p,n){
  left.p <- p-2*sqrt(p*(1-p)/n)
  right.p <- p+2*sqrt(p*(1-p)/n)
  return(CI=c(left.p,right.p))
}

z.ac_mm_final <- (ac_mm_final-mean.AC)/SD.AC
n = length(z.ac_mm_final)

below.3 <- (z.ac_mm_final < qnorm(0.03))
mean(below.3)
accept.p(0.03,n)

below.5 <- (z.ac_mm_final < qnorm(0.05))
mean(below.5)
accept.p(0.05,n)

below.10 <- (z.ac_mm_final < qnorm(0.10))
mean(below.10)
accept.p(0.1,n)

above.95 <- (z.ac_mm_final > qnorm(0.95))
mean(above.95)
(1-accept.p(0.95,n))[c(2,1)]

above.97.5 <- (z.ac_mm_final > qnorm(0.975))
mean(above.97.5)
(1-accept.p(0.975,n))[c(2,1)]

plot(pregweek,z.ac_mm_final,ylim=c(-4,4))
abline(h=0,col='red',lwd=2)
abline(h=qnorm(c(0.03,0.05,0.10,0.95,0.975)),col='blue',lwd=2)


mean.AC.by.K = -89.39 + 12.03*pregweek - 0.000863*(pregweek^3)
SD.AC.by.K = 1.179 + 0.4753*pregweek

z.ac_mm_final.by.K <- (ac_mm_final-mean.AC.by.K)/SD.AC.by.K
n = length(z.ac_mm_final.by.K)

below.3 <- (z.ac_mm_final.by.K < qnorm(0.03))
mean(below.3)
accept.p(0.03,n)

below.5 <- (z.ac_mm_final.by.K < qnorm(0.05))
mean(below.5)
accept.p(0.05,n)

below.10 <- (z.ac_mm_final.by.K < qnorm(0.10))
mean(below.10)
accept.p(0.1,n)

above.95 <- (z.ac_mm_final.by.K > qnorm(0.95))
mean(above.95)
(1-accept.p(0.95,n))[c(2,1)]

above.97.5 <- (z.ac_mm_final.by.K > qnorm(0.975))
mean(above.97.5)
(1-accept.p(0.975,n))[c(2,1)]

plot(pregweek,z.ac_mm_final.by.K,ylim=c(-4,4))
abline(h=0,col='red',lwd=2)
abline(h=qnorm(c(0.03,0.05,0.10,0.95,0.975)),col='blue',lwd=2)

