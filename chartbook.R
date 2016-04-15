######################################################################
## Copyright (C) 2016, Dave Straube, http://davestraube.com
##     
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA
######################################################################

setwd("~/R/OurOregon")
rm(list = ls())

suppressMessages(library(tidyr))
suppressMessages(library(scales))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))

source("./utils.R", echo = FALSE)
source("./joinpoint.R", echo = FALSE)

###################################################################################################
# Constants.
###################################################################################################

SHORT_VERSION <- FALSE       # Process all hospitals or just a short subset.
JP_FIT <- "best_fit"         # Joinpoint fit method - "best_fit" or "first_fit".
JP_MAX <- 2                  # Joinpoint inflection point upper bound and starting point.
JP_BETWEEN <- 10             # Joinpoint interpolation count.

###################################################################################################
# Load data, remove incomplete cases, define some common dates, normalize percentages.
###################################################################################################
load("./hospitals.RData")
incomp <- hosp.data[!complete.cases(hosp.data), c("hospital", "year")]
colnames(incomp) <- c("Hospital", "Year")
s <- "Deleting incomplete cases!"
for ( i in seq(dim(incomp)[1]) ) {
    s <- paste0(s, "\n\t", incomp[i, "Hospital"], " - ", incomp[i, "Year"])
}
warning(s)
hd <- hosp.data[complete.cases(hosp.data),]
hd$type <- factor(as.character(hd$type),
                  levels = c("A", "B", "DRG", "All"),
                  labels = c("Hospital type: A", "Hospital type: B",
                             "Hospital type: DRG", "All hospitals combined"))

# ACA was enacted on 3/23/2010 which was 82nd day of that year.
aca.enacted <- 2010 + (82/365)
# Great Recession peaked on 12/1/2007 and ended on 6/1/2009 as per St. Louis Fed.
# https://research.stlouisfed.org/fred2/help-faq/
gr.peak <- 2007 + (335/365)
gr.end <- 2009 + (152/365)

# Calculate various uncompensated care ratios and convert all percentages to 0-100 range.
hd$charity.care.pct <- 100 * (hd$charity.care / hd$gpr)
hd$bad.debt.pct <- 100 * (hd$bad.debt / hd$gpr)
hd$uncomp.care.pct <- 100 * (hd$tot.uncomp.care / hd$gpr)
hd$op.margin <- 100 * hd$op.margin
hd$tot.margin <- 100 * hd$tot.margin

###################################################################################################
# Plot MEAN charity care and bad debt as % of gpr by year faceted by type.
###################################################################################################
uc.mean <- as.data.frame(rbind(mutate(group_by(hd, year, type) %>% summarize(pct = mean(charity.care.pct)),
                                      pct = pct, uctype = "Charity Care"),
                               mutate(group_by(hd, year) %>% summarize(pct = mean(charity.care.pct)),
                                      pct = pct, uctype = "Charity Care", type = "All hospitals combined"),
                               mutate(group_by(hd, year, type) %>% summarize(pct = mean(bad.debt.pct)),
                                      pct = pct, uctype = "Bad Debt"),
                               mutate(group_by(hd, year) %>% summarize(pct = mean(bad.debt.pct)),
                                      pct = pct, uctype = "Bad Debt", type = "All hospitals combined")))

pl.uc.mean.as.pct.of.gpr <- ggplot(data = uc.mean) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    # Our data.
    geom_area(color = "black",
              aes(x = year, y = pct, fill = uctype)) +
    facet_wrap( ~ type, ncol = 2) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = pct0, breaks = seq(0, 100, 2)) +
    labs(x = "Year", y = "Percentage of GPR", fill = "Uncompensated Care Category") +
    ggtitle("Mean Uncompensated Care as Percentage\nof Gross Patient Revenue by Hospital Type") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Plot MEDIAN charity care and bad debt as % of gpr by year faceted by type.
###################################################################################################
uc.median <- rbind(mutate(group_by(hd, year, type) %>% summarize(pct = median(charity.care.pct)),
                          pct = pct, uctype = "Charity Care"),
                   mutate(group_by(hd, year) %>% summarize(pct = median(charity.care.pct)),
                          pct = pct, uctype = "Charity Care", type = "All hospitals combined"),
                   mutate(group_by(hd, year, type) %>% summarize(pct = median(bad.debt.pct)),
                          pct = pct, uctype = "Bad Debt"),
                   mutate(group_by(hd, year) %>% summarize(pct = median(bad.debt.pct)),
                          pct = pct, uctype = "Bad Debt", type = "All hospitals combined")
                   ) %>% as.data.frame()

pl.uc.median.as.pct.of.gpr <- ggplot(data = uc.median) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    # Our data.
    geom_area(color = "black",
              aes(x = year, y = pct, fill = uctype)) +
    facet_wrap( ~ type, ncol = 2) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = pct0, breaks = seq(0, 100, 2)) +
    labs(x = "Year", y = "Percentage of GPR", fill = "Uncompensated Care Category") +
    ggtitle("Median Uncompensated Care as Percentage\nof Gross Patient Revenue by Hospital Type") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Plot MEAN operating and total margins by year factored by hospital type.
###################################################################################################
mean.margins <- left_join(group_by(hd, year, type) %>% summarize(op.margin = mean(op.margin)),
                          group_by(hd, year, type) %>% summarize(tot.margin = mean(tot.margin)),
                          by = c("year", "type")) %>% as.data.frame()

pl.mean.margins <- ggplot(data = mean.margins) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    # Our data.
    geom_line(size = 1, linetype = "dashed", aes(x = year, y = op.margin, color = type)) +
    geom_line(size = 1, aes(x = year, y = tot.margin, color = type)) +
    # Hline at zero to highlight negative margins.
    geom_hline(yintercept = 0, size = 1) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = pct0, breaks = seq(0, 100, 2)) +
    ggtitle("Mean Operating and Total Margins\n(Operating - dashed, Total - solid)") +
    labs(x = "Year", y = "Margin as %", color = "Type") +
    theme(legend.position = "bottom")

###################################################################################################
# Plot MEDIAN operating and total margins by year factored by hospital type.
###################################################################################################
median.margins <- left_join(group_by(hd, year, type) %>% summarize(op.margin = median(op.margin)),
                            group_by(hd, year, type) %>% summarize(tot.margin = median(tot.margin)),
                            by = c("year", "type"))

pl.median.margins <- ggplot(data = median.margins) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    # Our data.
    geom_line(size = 1, linetype = "dashed", aes(x = year, y = op.margin, color = type)) +
    geom_line(size = 1, aes(x = year, y = tot.margin, color = type)) +
    # Hline at zero to highlight negative margins.
    geom_hline(yintercept = 0, size = 1) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = pct0, breaks = seq(-100, 100, 2)) +
    ggtitle("Median Operating and Total Margins\n(Operating - dashed, Total - solid)") +
    labs(x = "Year", y = "Margin as %", color = "Type") +
    theme(legend.position = "bottom")

###################################################################################################
# Create plot which demonstrates relative sizes of MEAN op.income and net.non.op.rev.
###################################################################################################
plotdata <- rbind(mutate(group_by(hd, type, year) %>% summarize(inc = mean(op.income)),
                         inc = inc, inctype = "Operating Income"),
                  mutate(group_by(hd, year) %>% summarize(inc = mean(op.income)),
                         inc = inc, inctype = "Operating Income", type = "All hospitals combined"),
                  mutate(group_by(hd, type, year) %>% summarize(inc = mean(net.non.op.rev)),
                         inc = inc, inctype = "Net Non-Operating Income"),
                  mutate(group_by(hd, year) %>% summarize(inc = mean(net.non.op.rev)),
                         inc = inc, inctype = "Net Non-Operating Income", type = "All hospitals combined")
                  ) %>% as.data.frame()
plotdata$inc <- plotdata$inc / 1000000
plotdata.pos <- subset(plotdata, inc >= 0)
plotdata.neg <- subset(plotdata, inc < 0)
yhi <- mround(2 * max(abs(plotdata$inc)), 2)
ylo <- -1 * yhi

pl.mean.net.income <- ggplot(data = plotdata) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    geom_bar(data = plotdata.pos,
             aes(x = year, y = inc, fill = inctype), stat = "identity") +
    geom_bar(data = plotdata.neg,
             aes(x = year, y = inc, fill = inctype), stat = "identity") +
    geom_hline(yintercept = 0, size = 1) +
    facet_wrap( ~ type, ncol = 2) +
    scale_fill_manual(values = c("Operating Income" = "forestgreen", "Net Non-Operating Income" = "firebrick")) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = moneyM, breaks = seq(ylo, yhi, 2)) +
    ggtitle("Mean Operating Income and Net Non-Operating Revenue\nby Year and Hospital Type (in $Millions)") +
    labs(x = "Year", y = "Mean of Net Income Component (in $Millions)", fill = "Net Income Component") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Create plot which demonstrates relative sizes of MEDIAN op.income and net.non.op.rev.
###################################################################################################
plotdata <- rbind(mutate(group_by(hd, type, year) %>% summarize(inc = median(op.income)),
                         inc = inc, inctype = "Operating Income"),
                  mutate(group_by(hd, year) %>% summarize(inc = median(op.income)),
                         inc = inc, inctype = "Operating Income", type = "All hospitals combined"),
                  mutate(group_by(hd, type, year) %>% summarize(inc = median(net.non.op.rev)),
                         inc = inc, inctype = "Net Non-Operating Income"),
                  mutate(group_by(hd, year) %>% summarize(inc = median(net.non.op.rev)),
                         inc = inc, inctype = "Net Non-Operating Income", type = "All hospitals combined")
) %>% as.data.frame()
plotdata$inc <- plotdata$inc / 1000000
plotdata.pos <- subset(plotdata, inc >= 0)
plotdata.neg <- subset(plotdata, inc < 0)
yhi <- mround(2 * max(abs(plotdata$inc)), 2)
ylo <- -1 * yhi

pl.median.net.income <- ggplot(data = plotdata) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    geom_bar(data = plotdata.pos,
             aes(x = year, y = inc, fill = inctype), stat = "identity") +
    geom_bar(data = plotdata.neg,
             aes(x = year, y = inc, fill = inctype), stat = "identity") +
    geom_hline(yintercept = 0, size = 1) +
    facet_wrap( ~ type, ncol = 2) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = moneyM, breaks = seq(ylo, yhi, 2)) +
    scale_fill_manual(values = c("Operating Income" = "forestgreen", "Net Non-Operating Income" = "firebrick")) +
    ggtitle("Median Operating Income and Net Non-Operating Revenue\nby Year and Hospital Type (in $Millions)") +
    labs(x = "Year", y = "Median of Net Income Component (in $Millions)", fill = "Net Income Component") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Boxplot of DRG hospital uncompensated care as % of grp by year with 2x private hospitals superimposed as points.
###################################################################################################
drg <- hd[hd$type == "Hospital type: DRG",]
drg$year <- factor(drg$year)
drg$Hospital <- drg$hospital
drg <- rbind(mutate(drg, pct = charity.care.pct, uctype = "Charity Care"),
             mutate(drg, pct = bad.debt.pct, uctype = "Bad Debt"),
             mutate(drg, pct = uncomp.care.pct, uctype = "Total Uncompensated Care"))
for.profit.hospitals = c("McKenzie-Willamette Medical Center", "Willamette Valley Medical Center")
drg.for.profit <- drg[drg$hospital %in% for.profit.hospitals,]
drg.non.profit <- drg[!drg$hospital %in% for.profit.hospitals,]

pl.drg.uc <- ggplot() +
    geom_boxplot(data = drg.non.profit,
                 aes(x = year, y = pct)) +
    geom_point(data = drg.for.profit,
               size = 3,
               aes(x = year, y = pct, color = Hospital, shape = Hospital)) +
    facet_wrap( ~ uctype, ncol = 1) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(labels = pct0, breaks = seq(-100, 100, 2)) +
    ggtitle("DRG Non-Profit Hospital Uncompensated Care as % of GPR\n with For-Profits Annotated") +
    labs(x = "Year", y = "Percent of GPR") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Boxplot of DRG hospital margins by year with 2x private hospitals superimposed as points.
###################################################################################################
drg <- hd[hd$type == "Hospital type: DRG",]
drg$year <- factor(drg$year)
drg$Hospital <- drg$hospital
drg <- rbind(mutate(drg, pct = op.margin, mtype = "Operating Margin"),
             mutate(drg, pct = tot.margin, mtype = "Total Margin"))
for.profit.hospitals = c("McKenzie-Willamette Medical Center", "Willamette Valley Medical Center")
drg.for.profit <- drg[drg$hospital %in% for.profit.hospitals,]
drg.non.profit <- drg[!drg$hospital %in% for.profit.hospitals,]

pl.drg.margin <- ggplot() +
    geom_boxplot(data = drg.non.profit,
                 aes(x = year, y = pct)) +
    geom_point(data = drg.for.profit,
               size = 3,
               aes(x = year, y = pct, color = Hospital, shape = Hospital)) +
    facet_wrap( ~ mtype, ncol = 1) +
    scale_color_brewer(palette = "Set1") +
    scale_y_continuous(labels = pct0, breaks = seq(-100, 100, 5)) +
    ggtitle("DRG Non-Profit Hospital Margins\n with For-Profits Annotated") +
    labs(x = "Year", y = "Margin Percentage") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Individual hospital plot and table of net income components as positive/negative stacked bars.
###################################################################################################
hospital.net.income <- function(h.name) {
    
    h.type <- hd[hd$year == 2014 & hd$hospital == h.name, "type"]
    h.data <- hd[hd$hospital == h.name, c("year", "op.income", "net.non.op.rev", "net.income")]
    plotdata <- rbind(data.frame(year = h.data$year, inc = h.data$op.income, inctype = "Operating Income"),
                      data.frame(year = h.data$year, inc = h.data$net.non.op.rev, inctype = "Net Non-Operating Income"))
    plotdata$inc <- plotdata$inc / 1000000
    plotdata$year <- factor(plotdata$year)
    plotdata.pos <- subset(plotdata, inc >= 0)
    plotdata.neg <- subset(plotdata, inc < 0)
    
    # Generate plot.
    yhi <- max(abs(h.data$net.income / 1000000))
    ylo <- -1 * yhi
    ydelta <- ifelse(yhi <= 10, 1, ifelse(yhi <= 20, 2, ifelse(yhi < 50, 5, 10)))
    ylo <- mround(ylo, ydelta)
    yhi <- mround(yhi, ydelta)
    pl <- ggplot(data = plotdata) +
        geom_bar(data = plotdata.pos,
                 aes(x = year, y = inc, fill = inctype), stat = "identity") +
        geom_bar(data = plotdata.neg,
                 aes(x = year, y = inc, fill = inctype), stat = "identity") +
        geom_hline(yintercept = 0, size = 1) +
        scale_fill_manual(values = c("Operating Income" = "forestgreen", "Net Non-Operating Income" = "firebrick")) +
        scale_x_discrete(breaks = seq(2006, 2014)) +
        scale_y_continuous(labels = moneyM, breaks = seq(ylo, yhi, ydelta)) +
        ggtitle(paste0(h.name, " - ", h.type,
                       "\nNet Income = Operating Income + Net Non-Operating Revenue ($Millions)")) +
        labs(x = "Year", y = "Income (in $Millions)", fill = "Net Income Component") +
        theme(legend.position = "bottom",
              strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))
    
    # Generate display table.
    df <- rbind(data.frame(year = h.data$year, inc = h.data$op.income, inctype = "Operating"),
                data.frame(year = h.data$year, inc = h.data$net.non.op.rev, inctype = "Non-Operating"),
                data.frame(year = h.data$year, inc = h.data$net.income, inctype = "Net"))
    df$inc <- paste0(pp(df$inc / 1000000, digits = 1), "M")
    df <- spread(df, year, inc)
    colnames(df)[which(colnames(df) == "inctype")] <- "Income Type"
    
    # Output.
    tbl <- tableGrob(df, rows = NULL)
    grid.arrange(pl, tbl, heights = c(5, 1))
}

###################################################################################################
# Individual hospital plot and table of margins overlaid on group margin boxplot.
###################################################################################################
hospital.margins <- function(h.name) {
    
    h.type <- hd[hd$year == 2014 & hd$hospital == h.name, "type"]
    # Boxplot data.
    t.data <- hd[hd$type == h.type, c("year", "op.margin", "tot.margin")]
    boxdata <- rbind(data.frame(year = t.data$year, margin = t.data$op.margin, mtype = "Operating Margin"),
                     data.frame(year = t.data$year, margin = t.data$tot.margin, mtype = "Total Margin"))
    # Point data.
    h.data <- hd[hd$hospital == h.name, c("year", "op.margin", "tot.margin")]
    pointdata <- rbind(data.frame(year = h.data$year, margin = h.data$op.margin, mtype = "Operating Margin"),
                       data.frame(year = h.data$year, margin = h.data$tot.margin, mtype = "Total Margin"))
    # Line (joinpoint) data.
    jp.op.margin <- joinpoint(h.data$year, h.data$op.margin, JP_FIT, JP_MAX, JP_BETWEEN)
    jp.tot.margin <- joinpoint(h.data$year, h.data$tot.margin, JP_FIT, JP_MAX, JP_BETWEEN)
    linedata <- rbind(data.frame(year = jp.op.margin$x,
                                 jp = jp.op.margin$y,
                                 mtype = "Operating Margin"),
                      data.frame(year = jp.tot.margin$x,
                                 jp = jp.tot.margin$y,
                                 mtype = "Total Margin"))
    
    # Generate plot.
    pl <- ggplot() +
        geom_hline(yintercept = 0, size = 0.5) +
        geom_boxplot(data = boxdata,
                     aes(x = year, y = margin, group = year)) +
        geom_point(data = pointdata,
                   size = 3, color = "red",
                   aes(x = year, y = margin)) +
        geom_line(data = linedata,
                  size = 1, color = "red",
                  aes(x = year, y = jp, group = mtype)) +
        facet_wrap( ~ mtype, ncol = 1) +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = seq(2006, 2014)) +
        scale_y_continuous(labels = pct0, breaks = seq(-100, 100, 5)) +
        ggtitle(paste0(h.name, " - Margins\n(Boxplot is for ", h.type, ")")) +
        labs(x = "Year", y = "Margin Percentage") +
        theme(legend.position = "bottom",
              strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))
    
    # Generate display table.
    df <- pointdata
    df$margin <- pct(df$margin, digits = 1)
    df$mtype <- gsub(" Margin", "", as.character(df$mtype))
    df <- spread(df, year, margin)
    colnames(df)[which(colnames(df) == "mtype")] <- "Margin Type"
    
    # Output.
    tbl <- tableGrob(df, rows = NULL)
    grid.arrange(pl, tbl, heights = c(5, 1))
}

###################################################################################################
# Individual hospital plot and table of uncompensated care as % of GPR on group margin boxplot.
###################################################################################################
hospital.uncompensated.care <- function(h.name) {

    h.type <- hd[hd$year == 2014 & hd$hospital == h.name, "type"]
    # Boxplot data.
    t.data <- hd[hd$type == h.type, c("year", "bad.debt.pct", "charity.care.pct", "uncomp.care.pct")]
    boxdata <- rbind(data.frame(year = t.data$year, uc = t.data$bad.debt.pct, uctype = "Bad Debt"),
                     data.frame(year = t.data$year, uc = t.data$charity.care.pct, uctype = "Charity Care"),
                     data.frame(year = t.data$year, uc = t.data$uncomp.care.pct, uctype = "Total Uncompensated Care"))
    # Point data.
    h.data <- hd[hd$hospital == h.name, c("year", "bad.debt.pct", "charity.care.pct", "uncomp.care.pct")]
    pointdata <- rbind(data.frame(year = h.data$year, uc = h.data$bad.debt.pct, uctype = "Bad Debt"),
                       data.frame(year = h.data$year, uc = h.data$charity.care.pct, uctype = "Charity Care"),
                       data.frame(year = h.data$year, uc = h.data$uncomp.care.pct, uctype = "Total Uncompensated Care"))
    # Line (joinpoint) data.
    jp.bad.debt <- joinpoint(h.data$year, h.data$bad.debt.pct, JP_FIT, JP_MAX, JP_BETWEEN)
    jp.charity.care <- joinpoint(h.data$year, h.data$charity.care.pct, JP_FIT, JP_MAX, JP_BETWEEN)
    jp.uncomp.care <- joinpoint(h.data$year, h.data$uncomp.care.pct, JP_FIT, JP_MAX, JP_BETWEEN)
    linedata <- rbind(data.frame(year = jp.bad.debt$x,
                                 jp = jp.bad.debt$y,
                                 uctype = "Bad Debt"),
                      data.frame(year = jp.charity.care$x,
                                 jp = jp.charity.care$y,
                                 uctype = "Charity Care"),
                      data.frame(year = jp.uncomp.care$x,
                                 jp = jp.uncomp.care$y,
                                 uctype = "Total Uncompensated Care"))
    
    # Generate plot.
    pl <- ggplot() +
        geom_boxplot(data = boxdata,
                     aes(x = year, y = uc, group = year)) +
        geom_point(data = pointdata,
                   size = 3, color = "red",
                   aes(x = year, y = uc)) +
        geom_line(data = linedata,
                  size = 1, color = "red",
                  aes(x = year, y = jp, group = uctype)) +
        facet_wrap( ~ uctype, ncol = 1) +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = seq(2006, 2014)) +
        scale_y_continuous(labels = pct0, breaks = seq(0, 100, 2)) +
        ggtitle(paste0(h.name, " - Uncompensated Care as % of GPR\n(Boxplot is for ", h.type, ")")) +
        labs(x = "Year", y = "Uncompensated Care Percentage") +
        theme(legend.position = "bottom",
              strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))
    
    # Generate display table.
    df <- pointdata
    df$uc <- pct(df$uc, digits = 1)
    df$uctype <- gsub(" Uncompensated Care", "", as.character(df$uctype))
    df <- spread(df, year, uc)
    colnames(df)[which(colnames(df) == "uctype")] <- "Deficit Type"
    
    # Output.
    tbl <- tableGrob(df, rows = NULL)
    grid.arrange(pl, tbl, heights = c(5, 1))
}

###################################################################################################
# Generate perhosp.Rmd which generates output for each hospital and is "included" by chartbook.Rmd.
###################################################################################################
all.hosp.names <- sort(unique(hd$hospital))
if ( SHORT_VERSION ) {
    # Use first, middle, and last hospitals only.
    all.hosp.names <- all.hosp.names[c(1, floor(length(all.hosp.names) / 2), length(all.hosp.names))]
}
sink("./perhosp.Rmd", type = "output")
for ( h.name in all.hosp.names ) {
    cat("\\newpage\n")
    cat(paste0("\n## ", h.name, "\n\n"))
    cat("```{R, echo = FALSE, fig.width = 7, fig.height = 9.5}\n")
    cat(paste0("suppressWarnings(hospital.net.income(\"", h.name, "\"))\n"))
    cat("```\n")
    cat("\\newpage\n")
    cat("```{R, echo = FALSE, fig.width = 7, fig.height = 10}\n")
    cat(paste0("hospital.margins(\"", h.name, "\")\n"))
    cat("```\n")
    cat("\\newpage\n")
    cat("```{R, echo = FALSE, fig.width = 7, fig.height = 10}\n")
    cat(paste0("hospital.uncompensated.care(\"", h.name, "\")\n"))
    cat("```\n")
}
sink()

###################################################################################################
# Test all hospitals.
###################################################################################################
hospital.test <- function() {
    for ( h.name in sort(unique(hd$hospital)) ) {
        cat(paste0(h.name, "\n"))
        cat("\thospital.net.income()\n")
        hospital.net.income(h.name)
        cat("\thospital.margins()\n")
        hospital.margins(h.name)
        cat("\thospital.uncompensated.care()\n")
        hospital.uncompensated.care(h.name)
    }
}
#hospital.test()

###################################################################################################
# List health systems, their hospitals, and type.
###################################################################################################
list.systems <- function() {
    for ( syst in names(hosp.sys) ) {
        cat(paste0("\t", syst, "\n"))
        for ( hosp in hosp.sys[[syst]] ) {
            cat(paste0("\t\t", hosp, " (", hd[hd$hospital == hosp & hd$year == 2014, "type"], ")\n"))
        }
    }
}
#list.systems()

###################################################################################################
# Plot median uncompensated care for large hospital systems relative to DRG median values.
###################################################################################################
# Construct DRG median values.
drg <- hd[hd$type == "Hospital type: DRG",]
pointdata <- rbind(group_by(drg, year) %>% summarise(pct = median(bad.debt.pct),
                                                     uctype = "Bad Debt",
                                                     System = "Hospital type: DRG") %>% as.data.frame(),
                   group_by(drg, year) %>% summarise(pct = median(charity.care.pct),
                                                     uctype = "Charity Care",
                                                     System = "Hospital type: DRG") %>% as.data.frame(),
                   group_by(drg, year) %>% summarise(pct = median(uncomp.care.pct),
                                                     uctype = "Total Uncompensated Care",
                                                     System = "Hospital type: DRG") %>% as.data.frame())
# Add hospital system median values.
for ( syst in names(hosp.sys) ) {
    if ( length(hosp.sys[[syst]]) >= 4 ) {
        hd.syst <- hd[hd$hospital %in% hosp.sys[[syst]],]
        pointdata <- rbind(pointdata,
                           group_by(hd.syst, year) %>% summarise(pct = median(bad.debt.pct),
                                                                 uctype = "Bad Debt",
                                                                 System = syst) %>% as.data.frame(),
                           group_by(hd.syst, year) %>% summarise(pct = median(charity.care.pct),
                                                                 uctype = "Charity Care",
                                                                 System = syst) %>% as.data.frame(),
                           group_by(hd.syst, year) %>% summarise(pct = median(uncomp.care.pct),
                                                                 uctype = "Total Uncompensated Care",
                                                                 System = syst) %>% as.data.frame())
    }
}

# Get joinpoint values.
dofun <- function(grp) {
    jp <- joinpoint(grp$year, grp$pct, JP_FIT, JP_MAX, JP_BETWEEN)
    data.frame(year = jp$x, pct = jp$y)
}
linedata <- group_by(pointdata, uctype, System) %>% do(dofun(.))

pl.median.uc.by.syst <- ggplot() +
    geom_point(data = pointdata,
               aes(x = year, y = pct, group = System, color = System, shape = System)) +
    geom_line(data = linedata,
              aes(x = year, y = pct, group = System, color = System, linetype = System)) +
    facet_wrap( ~ uctype, ncol = 1) +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = pct0, breaks = seq(0, 100, 2)) +
    labs(x = "Year", y = "Percentage of GPR") +
    ggtitle("Mean Uncompensated Care as Percentage\nof Gross Patient Revenue by Hospital System") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Plot median margins for large hospital systems relative to DRG median values.
###################################################################################################
# Construct DRG median values.
drg <- hd[hd$type == "Hospital type: DRG",]
pointdata <- rbind(group_by(drg, year) %>% summarise(margin = median(op.margin),
                                                     mtype = "Operating Margin",
                                                     System = "Hospital type: DRG") %>% as.data.frame(),
                   group_by(drg, year) %>% summarise(margin = median(tot.margin),
                                                     mtype = "Total Margin",
                                                     System = "Hospital type: DRG") %>% as.data.frame())
# Add hospital system median values.
for ( syst in names(hosp.sys) ) {
    if ( length(hosp.sys[[syst]]) >= 4 ) {
        hd.syst <- hd[hd$hospital %in% hosp.sys[[syst]],]
        pointdata <- rbind(pointdata,
                           group_by(hd.syst, year) %>% summarise(margin = median(op.margin),
                                                                 mtype = "Operating Margin",
                                                                 System = syst) %>% as.data.frame(),
                           group_by(hd.syst, year) %>% summarise(margin = mean(tot.margin),
                                                                 mtype = "Total Margin",
                                                                 System = syst) %>% as.data.frame())
    }
}

# Get joinpoint values.
dofun <- function(grp) {
    jp <- joinpoint(grp$year, grp$margin, JP_FIT, JP_MAX, JP_BETWEEN)
    data.frame(year = jp$x, margin = jp$y)
}
linedata <- group_by(pointdata, mtype, System) %>% do(dofun(.))

pl.median.margins.by.syst <- ggplot() +
    geom_point(data = pointdata,
               aes(x = year, y = margin, group = System, color = System, shape = System)) +
    geom_line(data = linedata,
              aes(x = year, y = margin, group = System, color = System, linetype = System)) +
    facet_wrap( ~ mtype, ncol = 1) +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(labels = pct0, breaks = seq(-100, 100, 2)) +
    labs(x = "Year", y = "Margin as Percentage") +
    ggtitle("Margins by Hospital System") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

###################################################################################################
# Hospital system margins vis-a-vis system median.
###################################################################################################
system.margins <- function(syst) {
    
    hd.syst <- hd[hd$hospital %in% hosp.sys[[syst]],]
    
    # Derive mean for all hospitals in system.
    pointdata <- rbind(group_by(hd.syst, year) %>% summarise(pct = median(op.margin),
                                                             mtype = "Operating Margin",
                                                             Hospital = "Hospital System Mean") %>% as.data.frame(),
                       group_by(hd.syst, year) %>% summarise(pct = median(tot.margin),
                                                             mtype = "Total Margin",
                                                             Hospital = "Hospital System Mean") %>% as.data.frame())
    
    # Add hospital values.
    for ( hosp in hosp.sys[[syst]] ) {
        thishosp <- hd[hd$hospital == hosp,]
        pointdata <- rbind(pointdata,
                           transmute(thishosp,
                                     year = year,
                                     pct = op.margin,
                                     mtype = "Operating Margin",
                                     Hospital = hosp) %>% as.data.frame(),
                           transmute(thishosp,
                                     year = year,
                                     pct = tot.margin,
                                     mtype = "Total Margin",
                                     Hospital = hosp) %>% as.data.frame())
    }
    
    # Get joinpoint values.
    dofun <- function(grp) {
        jp <- joinpoint(grp$year, grp$pct, JP_FIT, JP_MAX, JP_BETWEEN)
        data.frame(year = jp$x, pct = jp$y)
    }
    linedata <- group_by(pointdata, mtype, Hospital) %>% do(dofun(.))
    
    # Insure "Hospital System Mean" is always first/red element in legend and rest are alphabetical order.
    h.names <- c(hosp.sys[[syst]], "Hospital System Mean")
    levels <- c(h.names[match("Hospital System Mean", h.names)], sort(setdiff(h.names, "Hospital System Mean")))
    pointdata$Hospital <- factor(pointdata$Hospital, levels = levels)
    linedata$Hospital <- factor(linedata$Hospital, levels = levels)
    
    ggplot() +
        geom_point(data = pointdata,
                   aes(x = year, y = pct, group = Hospital, color = Hospital, shape = Hospital)) +
        geom_line(data = linedata,
                  aes(x = year, y = pct, group = Hospital, color = Hospital, linetype = Hospital)) +
        facet_wrap( ~ mtype, ncol = 1) +
        scale_shape_manual(values = seq(0, length(hosp.sys[[syst]]))) +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = seq(2006, 2014)) +
        scale_y_continuous(labels = pct0, breaks = seq(-100, 100, 2)) +
        labs(x = "Year", y = "Percent Margin") +
        ggtitle(paste0(syst," Hospital System\nMargin Percentage")) +
        theme(strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))
}

###################################################################################################
# Hospital system uncompensated care vis-a-vis system median.
###################################################################################################
system.uncompensated.care <- function(syst) {
    
    hd.syst <- hd[hd$hospital %in% hosp.sys[[syst]],]
    
    # Derive mean for all hospitals in system.
    pointdata <- rbind(group_by(hd.syst, year) %>% summarise(pct = median(bad.debt.pct),
                                                             uctype = "Bad Debt",
                                                             Hospital = "Hospital System Mean") %>% as.data.frame(),
                       group_by(hd.syst, year) %>% summarise(pct = median(charity.care.pct),
                                                             uctype = "Charity Care",
                                                             Hospital = "Hospital System Mean") %>% as.data.frame(),
                       group_by(hd.syst, year) %>% summarise(pct = median(uncomp.care.pct),
                                                             uctype = "Total Uncompensated Care",
                                                             Hospital = "Hospital System Mean") %>% as.data.frame())
    
    # Add hospital values.
    for ( hosp in hosp.sys[[syst]] ) {
        thishosp <- hd[hd$hospital == hosp,]
        pointdata <- rbind(pointdata,
                           transmute(thishosp,
                                     year = year,
                                     pct = bad.debt.pct,
                                     uctype = "Bad Debt",
                                     Hospital = hosp) %>% as.data.frame(),
                           transmute(thishosp,
                                     year = year,
                                     pct = charity.care.pct,
                                     uctype = "Charity Care",
                                     Hospital = hosp) %>% as.data.frame(),
                           transmute(thishosp,
                                     year = year,
                                     pct = uncomp.care.pct,
                                     uctype = "Total Uncompensated Care",
                                     Hospital = hosp) %>% as.data.frame())
    }
    
    # Get joinpoint values.
    dofun <- function(grp) {
        jp <- joinpoint(grp$year, grp$pct, JP_FIT, JP_MAX, JP_BETWEEN)
        data.frame(year = jp$x, pct = jp$y)
    }
    linedata <- group_by(pointdata, uctype, Hospital) %>% do(dofun(.))
    
    # Insure "Hospital System Mean" is always first/red element in legend and rest are alphabetical order.
    h.names <- c(hosp.sys[[syst]], "Hospital System Mean")
    levels <- c(h.names[match("Hospital System Mean", h.names)], sort(setdiff(h.names, "Hospital System Mean")))
    pointdata$Hospital <- factor(pointdata$Hospital, levels = levels)
    linedata$Hospital <- factor(linedata$Hospital, levels = levels)
    
    ggplot() +
        geom_point(data = pointdata,
                   aes(x = year, y = pct, group = Hospital, color = Hospital, shape = Hospital)) +
        geom_line(data = linedata,
                  aes(x = year, y = pct, group = Hospital, color = Hospital, linetype = Hospital)) +
        facet_wrap( ~ uctype, ncol = 1) +
        scale_shape_manual(values = seq(0, length(hosp.sys[[syst]]))) +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = seq(2006, 2014)) +
        scale_y_continuous(labels = pct0, breaks = seq(0, 100, 2)) +
        labs(x = "Year", y = "Percentage of GPR") +
        ggtitle(paste0(syst," Hospital System\nUncompensated Care as % of GPR")) +
        theme(strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))
}

###################################################################################################
# Generate persyst.Rmd which generates output for each system and is "included" by chartbook.Rmd.
###################################################################################################
first.sys <- TRUE
sink("./persyst.Rmd", type = "output")
for ( syst in sort(names(hosp.sys)) ) {
    if ( length(hosp.sys[[syst]]) >= 4 ) {
        if ( first.sys ) {
            first.sys <- FALSE
        } else {
            cat("\\newpage\n")
        }
        cat(paste0("\n## ", syst, "\n\n"))
        cat("```{R, echo = FALSE, fig.width = 7, fig.height = 9}\n")
        cat(paste0("system.margins(\"", syst, "\")\n"))
        cat("```\n")
        cat("\\newpage\n")
        cat("```{R, echo = FALSE, fig.width = 7, fig.height = 10}\n")
        cat(paste0("system.uncompensated.care(\"", syst, "\")\n"))
        cat("```\n")
    }
}
sink()

###################################################################################################
# Test all systems.
###################################################################################################
system.test <- function() {
    for ( syst in sort(names(hosp.sys)) ) {
        if ( length(hosp.sys[[syst]]) >= 4 ) {
            cat(paste0(syst, "\n"))
            cat("\tsystem.margins()\n")
            system.margins(syst)
            cat("\tsystem.uncompensated.care()\n")
            system.uncompensated.care(syst)
        }
    }
}
#system.test()
