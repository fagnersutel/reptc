novenove = read.csv("Avenida_Independencia_2019-11-11_2019-11-15.csv")
novenove = novenove[novenove$speed.m.s. > 0, ]
novenove$speed.m.s. = 3.6 * novenove$speed.m.s.
head(novenove)
locais = unique(novenove$junction_name)
locais
Sys.setlocale('LC_ALL','C')
library(data.table)
independencia = novenove[grep("^Avenida", novenove$junction_name), ]
View(independencia)
locais_ind   = unique(independencia$junction_name)


teste = independencia$junction_name

lookup = c(
  "Avenida Independ¨ºncia_819814663" = "Independencia x Rua Joao Teles",
  "Avenida Independ¨ºncia_Rua Coronel Vicente" = "Independencia x Rua Coronel Vicente",
  "Avenida Independ¨ºncia_Rua Garibaldi" = "Independencia x Rua Garibaldi",
  "Avenida Independ¨ºncia_Rua Tomaz Flores" = "Independencia x Rua Tomaz Flores",
  "Avenida Independ¨ºncia_Rua Santo Ant_nio" = "Independencia x Rua Santo Antonio"
)

independencia$local = lookup[teste]
unique(independencia$hour)

### Indpendencia x Rua Joao Teles ###
ind_jtl = independencia[independencia$local == "Independencia x Rua Joao Teles", ]
agr_ind_jtl = aggregate(ind_jtl[, 7], list(ind_jtl$hour), mean)
agr_ind_jtl$Group.1 = strptime(agr_ind_jtl$Group.1, format="%H:%M")
agr_ind_jtl = agr_ind_jtl[order(agr_ind_jtl$Group.1),]
head(ind_jtl)
agr_ind_jtl_t = data.frame(matrix(1:length(agr_ind_jtl$Group.1), nrow=1))
names(agr_ind_jtl_t) = agr_ind_jtl$Group.1
agr_ind_jtl_t[1,] = agr_ind_jtl$x
#View(agr_ind_jtl_t)

### Indpendencia x Rua Coronel Vicente ###
ind_cvi = independencia[independencia$local == "Independencia x Rua Coronel Vicente", ]
head(ind_jtl)
agr_ind_cvi = aggregate(ind_cvi[, 7], list(ind_cvi$hour), mean)
agr_ind_cvi$Group.1 = strptime(agr_ind_cvi$Group.1, format="%H:%M")
agr_ind_cvi = agr_ind_cvi[order(agr_ind_cvi$Group.1),]
agr_ind_cvi_t = data.frame(matrix(1:length(agr_ind_cvi$Group.1), nrow=1))
names(agr_ind_cvi_t) = agr_ind_cvi$Group.1
agr_ind_cvi_t[1,] = agr_ind_cvi$x

###Indpendencia x Rua Garibaldi ###
ind_gar = independencia[independencia$local == "Independencia x Rua Garibaldi", ]
head(ind_gar)
agr_ind_gar = aggregate(ind_gar[, 7], list(ind_gar$hour), mean)
agr_ind_gar$Group.1 = strptime(agr_ind_gar$Group.1, format="%H:%M")
agr_ind_gar = agr_ind_gar[order(agr_ind_gar$Group.1),]
agr_ind_gar_t = data.frame(matrix(1:length(agr_ind_gar$Group.1), nrow=1))
names(agr_ind_gar_t) = agr_ind_gar$Group.1
agr_ind_gar_t[1,] = agr_ind_gar$x

###Independencia x Rua Tomaz Flores ###
ind_tmz = independencia[independencia$local == "Independencia x Rua Tomaz Flores", ]
head(ind_tmz)
agr_ind_tmz = aggregate(ind_tmz[, 7], list(ind_tmz$hour), mean)
agr_ind_tmz$Group.1 = strptime(agr_ind_tmz$Group.1, format="%H:%M")
agr_ind_tmz = agr_ind_tmz[order(agr_ind_tmz$Group.1),]
agr_ind_tmz_t = data.frame(matrix(1:length(agr_ind_tmz$Group.1), nrow=1))
names(agr_ind_tmz_t) = agr_ind_tmz$Group.1
agr_ind_tmz_t[1,] = agr_ind_tmz$x

###Independencia x Rua Santo Antonio ###
ind_sta = independencia[independencia$local == "Independencia x Rua Santo Antonio", ]
head(ind_sta)
agr_ind_sta = aggregate(ind_sta[, 7], list(ind_sta$hour), mean)
agr_ind_sta$Group.1 = strptime(agr_ind_sta$Group.1, format="%H:%M")
agr_ind_sta = agr_ind_sta[order(agr_ind_sta$Group.1),]
agr_ind_sta_t = data.frame(matrix(1:length(agr_ind_sta$Group.1), nrow=1))
names(agr_ind_sta_t) = agr_ind_sta$Group.1
agr_ind_sta_t[1,] = agr_ind_sta$x

plot(agr_ind_jtl$Group.1, agr_ind_jtl$x, type="b", pch=19, lwd = 3,col="red", xlab="Faixa Horária", ylab="Vel. Média km/h", ylim=c(0,60))
par(new=TRUE)
plot(agr_ind_cvi$Group.1,agr_ind_cvi$x, pch=16, lwd = 3,col="blue", type="b", lty=4, , xlab="Faixa Horária", ylab="Vel. Média km/h", ylim=c(0,60))
par(new=TRUE)
plot(agr_ind_gar$Group.1,agr_ind_gar$x, pch=16, lwd = 3,col="green", type="b", lty=4, , xlab="Faixa Horária", ylab="Vel. Média km/h", ylim=c(0,60))
par(new=TRUE)
plot(agr_ind_tmz$Group.1,agr_ind_tmz$x, pch=16, lwd = 3,col="black", type="b", lty=4, , xlab="Faixa Horária", ylab="Vel. Média km/h", ylim=c(0,60))
par(new=TRUE)
plot(agr_ind_sta$Group.1,agr_ind_sta$x, pch=16, lwd = 3,col="orange", type="b", lty=4, , xlab="Faixa Horária", ylab="Vel. Média km/h", ylim=c(0,60))
par("usr")
legend(1574869472.0, 60, legend=c("Independencia x Rua João Teles", 
                                  "Independencia x Rua Coronel Vicente", 
                                  "Independencia x Rua Garibaldi", 
                                  "Independencia x Rua Tomaz Flores", 
                                  "Independencia x Rua Santo Antonio"),
       col=c("red", "blue", "green", "black", "orange"), lty=1:5, lwd = 3, cex=1)

