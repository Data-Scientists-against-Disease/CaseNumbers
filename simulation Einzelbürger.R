### Simuationssoftware mit Einzelfällen
#

n = 8000       # nur Einzelfälle statt 80 Mio.


# stati = c("before", "infected new", "infected infectuous", "infected symptoms", "infected known", "recovered", "dead")

stati = factor(c("before", "new", "infectuous", "symptoms", "known", "recovered", "dead"))
shareDiscovered = 0.8  #wieviele von den Kontaktpersonen werden identifiziert
anteilSymptomatisch = 0.5
anteilTest = 0.8 # wie wahrscheinlich macht jemand mit Symptomen einen Test

# Übergangswahrscheinlichkeiten
bn = .02   #infektionsrate
ni = c(0, 0.2, 0.5, 0.5, 1)
is = c(.1, .3, .5, .7, .8, .9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) * anteilSymptomatisch
sk = c(.1, .3, .5, .5, 1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1) * anteilTest

sd = c(0,0,0, 0.001, 0.001, 0.001, 0.002, 0.002, 0.002, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
sr = c(0,0,0,0,0,0,0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.1 ,0.1, .1, .1, .1, .1, .2, .2, .2, .2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
ir = sr


person = data.frame(p=1:n)
person$status = "before"
person$since = 1
person$known = FALSE
person$age = sample(1:80)

for (tage in 1:50)
{
  print(tage)
  for (i in 1:n)
  {
    oldStatus = person$status[i]
    
    if (person$status[i] == "symptoms")
      if (runif(1)<sd[person$since[i]])
        person$status[i] = "dead"
      
      if (person$status[i] == "symptoms")
        if (runif(1)<sr[person$since[i]])
          person$status[i] = "recovered"
        
        if (person$status[i] == "infectuous")
          if (runif(1)<is[person$since[i]])
            person$status[i] = "symptoms"
          
          if (person$status[i] == "infectuous")
            if (runif(1)<ir[person$since[i]])
              person$status[i] = "recovered"
          
          if (person$status[i] == "new")
            if (runif(1)<ni[person$since[i]])
              person$status[i] = "infectuous"
    
          
          if (person$status[i] == "symptoms")
            if (runif(1)<sk[person$since[i]])
              person$known[i] = TRUE
            

            if (person$status[i] in c("infectuous","symptoms"))
            {
              newPerson = runif(1, min=1, max=8000)
              if (runif(1)<bn)
                if (person$status[newPerson] == "before")
                {
                  person$status[newPerson] = "new"
                  person$since[newPerson] = 1
                }}
                      

              
              if (person$status[i] != oldStatus)
                person$since[i] = 1
              else
                person$since[i] = person$since[i]+1
              
  }
  print(table(person$status))
  print(table(person$since))
  
}

ni
i
person[i,]
print table(person$status)
print table (person$since)
is[person$since[i]]
iS[5]
person$since[i]
person$status[i]
i

runif()
View(person)
bn[person$since[1:10]]
#person ist dataframe n, Datum mit Zustand stati und since
bn
table(person$status)

xf <- factor(x, levels = c("Male", "Man" , "Lady",   "Female"), labels = c("Male", "Male", "Female", "Female"))
xf
str(xf)
data.frame(1, 1:10, sample(c("a","b","cf"), 10, replace = TRUE))
