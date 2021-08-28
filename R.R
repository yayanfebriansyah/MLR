library(AICcmodavg)
library(MASS)

data1=read.csv("D:/OneDrive - Institut Teknologi Bandung/Documents/jatim.csv")
data1


model = lm(data1$PAD~data1$PD+data1$Penduduk+data1$Upah)
summary(model)

data_2018=data1[data1$Tahun==2018,]
data_2019=data1[data1$Tahun==2019,]
data_2020=data1[data1$Tahun==2020,]



#data2 adalah data setelah ditambahkan dummy variabel
data2=read.csv("D:/OneDrive - Institut Teknologi Bandung/Documents/jatim1.csv")
data2

#data_log adalah data setelah melakukan operasi ln
#pada tiap variabel
data_log=read.csv("D:/OneDrive - Institut Teknologi Bandung/Documents/jatim2.csv")
data_log

#model 1 variabel
#model PAD dan PD
model_PD=lm(PAD~PD, data = data2)
summary(model_PD)
#model PAD dan Penduduk
model_Penduduk=lm(PAD~Penduduk, data = data2)
summary(model_Penduduk)
#model PAD dan Upah Minimum
model_Upah=lm(PAD~Upah, data = data2)
summary(model_Upah)
#model PAD dan PD dengan tahun (variabel dummy)
model_PD_Tahun=lm(PAD~PD+Tahun_2018+
              Tahun_2019, data = data2)
summary(model_PD_Tahun)
#model PAD dan Jumlah Penduduk dengan tahun
model_Penduduk_Tahun=lm(PAD~Penduduk+Tahun_2018+
                          Tahun_2019, data = data2)
summary(model_Penduduk_Tahun)
#model PAD dan Upah Minimum dengan tahun
model_Upah_Tahun=lm(PAD~Upah+Tahun_2018+
                      Tahun_2019, data = data2)
summary(model_Upah_Tahun)
#model PAD, PD, dan Jumlah Penduduk
model_PD_Penduduk=lm(PAD~PD+Penduduk, data = data2)
summary(model_PD_Penduduk)
#model PAD, PD, dan Upah Minimum
model_PD_Upah=lm(PAD~PD+Upah, data = data2)
summary(model_PD_Upah)
#model PAD, Penduduk, dan Upah Minimum
model_Penduduk_Upah=lm(PAD~Penduduk+Upah, data = data2)
summary(model_Penduduk_Upah)
#model PAD, PD, Penduduk, dan Upah Minimum
model_All=lm(PAD~PD+Penduduk+Upah, data = data2)
summary(model_All)
#membuat model2 yaitu model semua variabel + dummy
model_All_Tahun=lm(PAD~PD+Penduduk+Upah+Tahun_2018
          +Tahun_2019, data = data2)
summary(model_All_Tahun)

models = list(model_PD,model_Penduduk,model_Upah,
           model_PD_Tahun,model_Penduduk_Tahun,
           model_Upah_Tahun,model_PD_Penduduk,
           model_PD_Upah,model_Penduduk_Upah,
           model_All,model_All_Tahun)
model.names = c('PD.model','Penduduk.model',
                'Upah.model','PD.Tahun.model',
                'Penduduk.Tahun.model','Upah.Tahun.model',
                'PD.Penduduk.model','PD.Upah.model',
                'Penduduk.Upah.model','All.model',
                'All.Tahun.model')

aictab(cand.set = models, modnames = model.names)





#Lakukan stepwise regression dengan dummy
modelstepwise=stepAIC(model_All_Tahun,direction="both",
                   trace=FALSE)
summary(modelstepwise)

#model100 yaitu model semua variabel tanpa dummy
model100=lm(PAD~PD+Penduduk+Upah, data = data2)
summary(model100)

#Lakukan stepwise regression
modelstepwise100=stepAIC(model100,direction="both",
                      trace=FALSE)
summary(modelstepwise100)


#data log
data_100 = data2[c("PAD","PD","Penduduk","Upah")]
data_100
#Stepwise Regression
model_all_variables_100 = lm(PAD~.,data=data_100)
formula(model_all_variables_100)

model_start_100 = lm(PAD~1,data=data_100)
step(model_start_100, direction = "both", scope = formula(model_all_variables_100))



qqnorm(resid(modelstepwise100),ylab="Residuals")
qqline(resid(modelstepwise100),col="red")

hist(residuals(modelstepwise100))
shapiro.test(residuals(modelstepwise100))
shapiro.test(modelstepwise100$residuals)
#uji autokorelasi
library(lmtest)
dwtest(modelstepwise100)
bptest(modelstepwise100, studentize = FALSE)
library(MASS)
#studentized residual untuk mendeteksi outlier
studres(modelstepwise100)
abline(modelstepwise100)
#deteksi outlier
library(car)
outlierTest(modelstepwise100)



#data tanpa kota surabaya
data3 = subset(data2, Kab!='Kota Surabaya')
data3

model101=lm(PAD~PD+Penduduk, data = data3)
summary(model101)

modelstepwise101=stepAIC(model101,direction="both",
                         trace=FALSE)
summary(modelstepwise101)
shapiro.test(residuals(modelstepwise101))
qqnorm(resid(modelstepwise101),ylab="Residuals")
qqline(resid(modelstepwise101),col="red")


model_PAD_PD = lm(PAD~Penduduk+Tahun_2018+
                    Tahun_2019+Penduduk*Tahun_2018+Penduduk*Tahun_2019,data = data2)
summary(model_PAD_PD)

shapiro.test(residuals(model_PAD_PD))


data_model = data2[c("PAD","PD","Penduduk","Upah")]
data_model
#Stepwise Regression
model_all_variables = lm(PAD~.,data=data_model)
formula(model_all_variables)

model_start = lm(PAD~1,data=data_model)
step(model_start, direction = "both", scope = formula(model_all_variables))

data_log=read.csv("D:/OneDrive - Institut Teknologi Bandung/Documents/jatim2.csv")
data_log


#data log
data_model_log = data_log[c("PAD","PD","Penduduk","Upah")]
data_model_log
#Stepwise Regression
model_all_variables_log = lm(PAD~.,data=data_model_log)
formula(model_all_variables_log)

model_start_log = lm(PAD~1,data=data_model_log)
step(model_start_log, direction = "both", scope = formula(model_all_variables_log))

model105=lm(PAD~PD+Penduduk, data = data_log)
summary(model105)
modelstepwise105=stepAIC(model105,direction="both",
                         trace=FALSE)
summary(modelstepwise105)
#uji normalitas
shapiro.test(residuals(modelstepwise105))
#uji autokorelasi
library(lmtest)
dwtest(modelstepwise105)
bptest(modelstepwise105, studentize = FALSE)
library(MASS)
#studentized residual untuk mendeteksi outlier
studres(modelstepwise105)
abline(modelstepwise105)
#deteksi outlier
library(car)
outlierTest(modelstepwise105)
rstudent(modelstepwise105)==studres(modelstepwise105)


