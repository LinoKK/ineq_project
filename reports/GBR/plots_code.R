

# Mit ggplot2

library("ggplot2")

y.leg <- c(4.5, 3, 2.1, 1.4, .7)
leg.txt <- c("verfügbares Einkommen", "Faktoreinkommen", "natürliches Einkommen")

ggplot(data = gini.tot.p2) + 
geom_line(mapping = aes(x = year, y = gini.disp.inc, color = "red"), show.legend = FALSE) + 
geom_point(mapping = aes(x = year, y = gini.disp.inc, color = "red"), show.legend = FALSE) +
geom_line(mapping = aes(x = year, y = gini.fac.inc, color = "blue"), show.legend = FALSE) + 
geom_point(mapping = aes(x = year, y = gini.fac.inc, color = "blue"), show.legend = FALSE) +
geom_line(mapping = aes(x = year, y = gini.nat.inc, color = "green"), show.legend = FALSE) + 
geom_point(mapping = aes(x = year, y = gini.nat.inc, color = "green"), show.legend = FALSE)+
  geom_text(mapping = aes (x= 2010, y = 0.52, label = "Verfügbares Einkommen")) +
  geom_text(mapping = aes (x= 2010, y = 0.48, label = "Faktoreinkommen")) +
  geom_text(mapping = aes (x= 2010, y = 0.43, label = "Naturalieneinkommen"))

savePlot(filename = "reports/GBR/plots/plot_gini_tot_p2", type ="png")


#P2 Haushalte

attach(gini.tot.p2)
plot(main = "Gini P2 Haushalte - verfügbares Einkommen", xlab = "year", ylab = "gini coefficient", x = year, y = gini.disp.inc, col ="red", type = "b")

savePlot(filename = "reports/GBR/plots/plot_gini_disp_p2_hh", type ="png")

