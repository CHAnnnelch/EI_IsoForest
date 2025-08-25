rm(list=ls())

library(dplyr)
library(tidyverse)
library(see)
library(ggplot2)
library(performance)

df <- read.csv("new_oecd.csv")

d <- df %>%
  filter(country=="Brazil")

d$month <- as.Date(d$month, format="%d/%m/%Y")

modelo <- lm(fdi_inf ~ fdi_out, data=d)

ggplot(d, aes(x=fdi_inf, y=fdi_out)) +
  geom_point(color="black") +  # pontos
  geom_smooth(method="lm", se=TRUE, color="red", fill="purple", alpha=0.3) + 
  labs(
    title="Inflows de FDI vs. Outflows de FDI do Brasil - em bilhões de US$",
    subtitle="Ajustamento Linear",
    x="Inflows de FDI",
    y="Outflows de FDI"
  ) +
  theme_minimal()

#loess
ggplot(d, aes(x=fdi_inf, y=fdi_out)) +
  geom_point(color="black") +  # pontos
  geom_smooth(method="loess", se=TRUE, color="red", fill="purple", alpha=0.3) + 
  labs(
    title="Inflows de FDI vs. Outflows de FDI do Brasil - em bilhões de US$",
    subtitle="Ajustamento LOESS",
    x="Inflows de FDI",
    y="Outflows de FDI"
  ) +
  theme_minimal()

sub_lula <- subset(d, month >= "2003-01-01" & month <= "2011-01-01")

sub_dilma <- subset(d, month >= "2011-01-01" & month <= "2016-08-31")

sub_temer <- subset(d, month >= "2016-08-31" & month <= "2018-12-31")

sub_bolsonaro <- subset(d, month >= "2019-01-01" & month <= "2023-01-01")

library(patchwork)

#lula
p1 <- ggplot(sub_lula, aes(x=fdi_inf, y=fdi_out)) +
  geom_point(color="black") +  # pontos
  geom_smooth(method="lm", se=TRUE, color="red", fill="purple", alpha=0.3) + 
  labs(
    title="Governo Lula",
    subtitle="Ajustamento Linear",
    x="Inflows de FDI",
    y="Outflows de FDI"
  ) +
  theme_minimal()

#dilma
p2 <- ggplot(sub_dilma, aes(x=fdi_inf, y=fdi_out)) +
  geom_point(color="black") +  # pontos
  geom_smooth(method="lm", se=TRUE, color="red", fill="purple", alpha=0.3) + 
  labs(
    title="Governo Dilma",
    subtitle="Ajustamento Linear",
    x="Inflows de FDI",
    y="Outflows de FDI"
  ) +
  theme_minimal()

#temer
p3 <- ggplot(sub_temer, aes(x=fdi_inf, y=fdi_out)) +
  geom_point(color="black") +  # pontos
  geom_smooth(method="lm", se=TRUE, color="red", fill="purple", alpha=0.3) + 
  labs(
    title="Governo Temer",
    subtitle="Ajustamento Linear",
    x="Inflows de FDI",
    y="Outflows de FDI"
  ) +
  theme_minimal()

#bolsonaro
p4 <- ggplot(sub_bolsonaro, aes(x=fdi_inf, y=fdi_out)) +
  geom_point(color="black") +  # pontos
  geom_smooth(method="lm", se=TRUE, color="red", fill="purple", alpha=0.3) + 
  labs(
    title="Governo Bolsonaro",
    subtitle="Ajustamento Linear",
    x="Inflows de FDI",
    y="Outflows de FDI"
  ) +
  theme_minimal()

(p1 | p2) / (p3 | p4)

#geral
par(mfrow=c(1,1))
bp <- breakpoints(fdi_out ~ fdi_inf, data=d)
#obs 114 172 306

d$month[114]
d$month[172]
d$month[306]

#lula
bp_lula <- breakpoints(fdi_out ~ fdi_inf, data=sub_lula)

sub_lula$month[45]
sub_lula$month[76]

#dilma
bp_dilma <- breakpoints(fdi_out ~ fdi_inf, data=sub_dilma)

sub_dilma$month[10]
sub_dilma$month[24]
sub_dilma$month[39]
sub_dilma$month[49]
sub_dilma$month[62]

#temer
bp_temer <- breakpoints(fdi_out ~ fdi_inf, data=sub_temer)

sub_temer$month[12]
sub_temer$month[15]
sub_temer$month[18]

#bolsonaro
bp_bolsonaro <- breakpoints(fdi_out ~ fdi_inf, data=sub_bolsonaro)

sub_bolsonaro$month[13]
sub_bolsonaro$month[20]
sub_bolsonaro$month[32]
sub_bolsonaro$month[40]

#chow geral
sctest(fdi_out ~ fdi_inf, type = "Chow", data=d)

#chow lula
sctest(fdi_out ~ fdi_inf, type = "Chow", data=sub_lula)

#chow dilma
sctest(fdi_out ~ fdi_inf, type = "Chow", data=sub_dilma)

#chow temer
sctest(fdi_out ~ fdi_inf, type = "Chow", data=sub_temer)

#chow bolsonaro
sctest(fdi_out ~ fdi_inf, type = "Chow", data=sub_bolsonaro)