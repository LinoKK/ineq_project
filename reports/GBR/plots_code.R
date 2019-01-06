

#ggplot2
#ggplot(data = gini.tot.p2) + 
#  geom_line(mapping = aes(x = year, y = gini.disp.inc, color = "red")) + 
#  geom_point(mapping = aes(x = year, y = gini.disp.inc, color = "red")) +
#  geom_line(mapping = aes(x = year, y = gini.fac.inc, color = "blue")) + 
#  geom_point(mapping = aes(x = year, y = gini.fac.inc, color = "blue")) +
#  geom_line(mapping = aes(x = year, y = gini.nat.inc, color = "green")) + 
#  geom_point(mapping = aes(x = year, y = gini.nat.inc, color = "green"))



#P2 Haushalte

attach(gini.tot.p2)
plot(main = "Gini P2 Haushalte - verfügbares Einkommen", xlab = "year", ylab = "gini coefficient", x = year, y = gini.disp.inc, col ="red", type = "b")

savePlot(filename = "reports/GBR/plots/plot_gini_disp_p2_hh", type ="png")

