library(plotly)
library(dplyr)
library(ggplot2)
library(sjPlot)
library(lmtest)
library(strucchange)

data_1 = read.csv('data/1.txt',
                  header = F)

data_2 = read.csv('data/2.txt',
                  header = F)

data_3 = read.csv('data/3.txt',
                  header = F)

#------Регрессия------
glimpse(data_1)

model = lm(data = data_1, V2~V1)
summary(model)

coeftest(model)
coefci(model, level=0.95)

y_pred = model %>% predict(data_1)

fig = plot_ly(data=data_1, x=~V1, y=~V2, type = 'scatter', mode='markers', name="Orginal")%>%
  add_trace(x=~V1, y=y_pred, mode="lines", name="Regression fit")
fig

qt(0.95, df=98)

#------Тест Чоу------

set_1 = data.frame(x=data_2$V1, dummy=data_2$V2, y=data_2$V4)
set_2 = data.frame(x=data_2$V1, dummy=data_2$V3, y=data_2$V4)

testing = set_1[set_1$dummy==1,]

Choy = function(data, with_lines=T){
  model = lm(data=data, y~x)
  data.0 = data[data$dummy==0,]
  data.1 = data[data$dummy==1,]
  model.0 = lm(data=data.0, y~x)
  model.1 = lm(data=data.1, y~x)
  
  RSS = deviance(model)
  RSS.0 = deviance(model.0)
  RSS.1 = deviance(model.1)
  
  F_val = ((RSS - RSS.0 - RSS.1)/2)/((RSS.0 + RSS.1)/(nrow(data)-2*2))
  
  print(RSS)
  print(RSS.0)
  print(RSS.1)
  print(F_val)
  
  data$color = c("red", "blue")[data$dummy+1]
  
  fig = plot_ly(data=data,  type = 'scatter', mode='markers')%>%
    add_trace(x=~x, y=~y, mode='markers', name="Orginal", marker=list(color = ~color))
  
  if (F_val>10) {
    fig = fig%>%
      add_trace(x=~x, y=predict(model.0, data), mode="lines", name="red regression")%>%
      add_trace(x=~x, y=predict(model.1, data), mode="lines", name="blue regression")
  } else{
    fig = fig%>%
      add_trace(x=~x, y=predict(model, data), mode="lines", name="Regression fit")
  }
  if (with_lines) fig = fig%>%add_trace(x=~x, y=predict(lm(data=data, y~dummy*x+(1-dummy)*x), data), mode="markers", name="Regression fit", marker=list(color="black"))
  fig
}

Choy(set_1)
Choy(set_2)

#------Гетероскедастичность-----

model = lm(data=data_3, V2~V1)
summary(model)

fig = plot_ly(data=data_3, x=~V1, y=~V2, type = 'scatter', mode='markers', name="Orginal")%>%
  add_trace(x=~V1, y=predict(model, data_3), mode="lines", name="Regression fit")
fig
ost = abs(resid(model))
ost.ranked = rank(ost)
x = data_3$V1
x.ranked = rank(x)
n=nrow(data_3)
p = 1-6*sum((x.ranked-ost.ranked)**2) / (n**3-n)
statis = p*(n-2)**0.5/(1-p**2)**0.5

data = data_3%>% arrange(desc(x))
m=round(3/8*n)


data.first = data[1:m,]
data.end = data[((n-m)+1):n,]

model.first = lm(data=data.first, V2~V1)
model.end = lm(data=data.end, V2~V1)

RSS.first = deviance(model.first)
RSS.end = deviance(model.end)

Ttest = RSS.first/RSS.end

rstandard(model)

fig = plot_ly(x=~data_3$V1, y=~ost**2, type = 'scatter', mode='markers')
fig

gqtest(model, fraction=0.2)
