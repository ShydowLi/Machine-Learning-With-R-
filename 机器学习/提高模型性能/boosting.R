#--boosting--自适应boosting---

library(adabag)

m_adaboosting<-boosting(default~.,data = data)
pred_adaboost<-predict(m_adaboosting,data)

model<-boosting.cv(default~.,data = data)
