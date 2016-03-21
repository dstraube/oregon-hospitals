setwd("~/R/OurOregon")
rm(list = ls())

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))

source("./utils.R", echo = FALSE)

load("./hospitals.RData")
hd <- hosp.data[complete.cases(hosp.data),]

# ACA was enacted on 3/23/2010 which was 82nd day of that year.
aca.enacted <- 2010 + (82/365)
# Great Recession peaked on 12/1/2007 and ended on 6/1/2009 as per St. Louis Fed.
# https://research.stlouisfed.org/fred2/help-faq/
gr.peak <- 2007 + (335/365)
gr.end <- 2009 + (152/365)

# Calculate various uncompensated care ratios.
hd$charity.care.pct <- hd$charity.care / hd$gpr
hd$bad.debt.pct <- hd$bad.debt / hd$gpr
hd$uncomp.care.pct <- hd$tot.uncomp.care / hd$gpr

# Plot charity care as % of gpr by year.
pl.cc.as.pct.of.gpr <- ggplot() +
    geom_line(data = hd,
              aes(x = year, y = 100 * charity.care.pct, color = hospital)) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    labs(x = "Year", y = "Charity Care Percentage") +
    ggtitle("Charity Care as\nPercentage of Gross Patient Revenue") +
    theme(legend.position = "none")

# Plot bad debt as % of gpr by year.
pl.bd.as.pct.of.gpr <- ggplot() +
    geom_line(data = hd,
              aes(x = year, y = 100 * bad.debt.pct, color = hospital)) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    labs(x = "Year", y = "Bad Debt as Percentage") +
    ggtitle("Bad Debt as\nPercentage of Gross Patient Revenue") +
    theme(legend.position = "none")

# Plot total uncompensated care as % of gpr by year.
pl.uc.as.pct.of.gpr <- ggplot() +
    geom_line(data = hd,
              aes(x = year, y = 100 * uncomp.care.pct, color = hospital)) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    labs(x = "Year", y = "Total Uncompensated Care as Percentage") +
    ggtitle("Total Uncompensated Care as\nPercentage of Gross Patient Revenue") +
    theme(legend.position = "none")

# Plot mean charity care and bad debt as % of gpr by year faceted by type.
cc.mean <- group_by(hd, year, type) %>% summarize(mean = mean(charity.care.pct)) %>% as.data.frame()
cc.mean$htype <- cc.mean$type
cc.mean$type <- NULL
cc.mean$ctype <- "Charity Care"
bd.mean <- group_by(hd, year, type) %>% summarize(mean = mean(bad.debt.pct)) %>% as.data.frame()
bd.mean$htype <- bd.mean$type
bd.mean$type <- NULL
bd.mean$ctype <- "Bad Debt"
cc.mean.all <- group_by(hd, year) %>% summarize(mean = mean(charity.care.pct)) %>% as.data.frame()
cc.mean.all$htype <- "All"
cc.mean.all$ctype <- "Charity Care"
bd.mean.all <- group_by(hd, year) %>% summarize(mean = mean(bad.debt.pct)) %>% as.data.frame()
bd.mean.all$htype <- "All"
bd.mean.all$ctype <- "Bad Debt"
uc.mean <- rbind(cc.mean, bd.mean, cc.mean.all, bd.mean.all)
uc.mean$mean <- uc.mean$mean * 100
uc.mean$htype <- factor(uc.mean$htype,
                        levels = c("A", "B", "DRG", "All"),
                        labels = c("Hospital type: A", "Hospital type: B",
                                   "Hospital type: DRG", "All hospitals combined"))
pl.uc.mean.as.pct.of.gpr <- ggplot(data = uc.mean) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    # Our data.
    geom_area(color = "black",
              aes(x = year, y = mean, fill = ctype)) +
    facet_wrap( ~ htype, ncol = 2) +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    scale_y_continuous(breaks = seq(0, 100, 2)) +
    labs(x = "Year", y = "Percentage of GPR", fill = "Uncompensated Care Category") +
    ggtitle("Mean Uncompensated Care as Percentage\nof Gross Patient Revenue by Hospital Type") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))

# grid.arrange(pl.cc.as.pct.of.gpr, pl.bd.as.pct.of.gpr,
#              pl.uc.as.pct.of.gpr, pl.mean.cc.as.pct.of.gpr,
#              nrow = 2, ncol = 2)

# Plot operating margin by year.
pl.op.margin <- ggplot() +
    geom_line(data = hd,
              aes(x = year, y = op.margin, color = hospital)) +
    ggtitle("Oerating Margin") +
    theme(legend.position = "none")

# Plot mean operating and total margins by year factored by hospital type.
mean.op.margin <- group_by(hd, year, type) %>% summarize(op.margin = mean(op.margin)) %>% as.data.frame()
mean.op.margin$op.margin <- mean.op.margin$op.margin * 100
mean.tot.margin <- group_by(hd, year, type) %>% summarize(tot.margin = mean(tot.margin)) %>% as.data.frame()
mean.tot.margin$tot.margin <- mean.tot.margin$tot.margin * 100
mean.margins <- left_join(mean.op.margin, mean.tot.margin, by = c("year", "type"))

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
    scale_y_continuous(breaks = seq(0, 100, 2)) +
    ggtitle("Mean Operating and Total Margins\n(Operating - dashed, Total - solid)") +
    labs(x = "Year", y = "Margin as %", color = "Type")

# Create plot which demonstrates relative sizes of op.income and net.non.op.rev.
tmp <- group_by(hd, year) %>% summarize(oi = mean(op.income),
                                        nnor = mean(net.non.op.rev)) %>% as.data.frame()
tmp.oi.all <- tmp[, c("year", "oi")]
colnames(tmp.oi.all) <- c("year", "mean")
tmp.oi.all$type = "All"
tmp.oi.all$revtype <- "Operating Income"
tmp.nnor.all <- tmp[, c("year", "nnor")]
colnames(tmp.nnor.all) <- c("year", "mean")
tmp.nnor.all$type = "All"
tmp.nnor.all$revtype = "Net Non-operating Income"
tmp <- group_by(hd, type, year) %>% summarize(oi = mean(op.income),
                                        nnor = mean(net.non.op.rev)) %>% as.data.frame()
tmp.oi <- tmp[, c("year", "type", "oi")]
colnames(tmp.oi) <- c("year", "type", "mean")
tmp.oi$revtype <- "Operating Income"
tmp.nnor <- tmp[, c("year", "type", "nnor")]
colnames(tmp.nnor) <- c("year", "type", "mean")
tmp.nnor$revtype = "Net Non-operating Income"
plotdata <- rbind(tmp.oi, tmp.nnor, tmp.oi.all, tmp.nnor.all)
plotdata$type <- factor(plotdata$type,
                        levels = c("A", "B", "DRG", "All"),
                        labels = c("Hospital type: A", "Hospital type: B",
                                   "Hospital type: DRG", "All hospitals combined"))
plotdata.pos <- subset(plotdata, mean >= 0)
plotdata.neg <- subset(plotdata, mean < 0)
pl.net.income <- ggplot(data = plotdata) +
    # Add great recession.
    geom_rect(fill = "lightpink", alpha = 0.05,
              aes(xmin = gr.peak, ymin = -Inf, xmax = gr.end, ymax = +Inf)) +
    # Annotate ACA enactment date.
    geom_vline(xintercept = aca.enacted) +
    geom_bar(data = plotdata.pos,
             aes(x = year, y = mean / 1000000, fill = revtype), stat = "identity") +
    geom_bar(data = plotdata.neg,
             aes(x = year, y = mean / 1000000, fill = revtype), stat = "identity") +
    geom_hline(yintercept = 0, size = 1) +
    facet_wrap( ~ type, ncol = 2) +
    scale_fill_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(2006, 2014)) +
    ggtitle("Mean Operating Income and Net Non-operating Revenue\nby Year and Hospital Type") +
    labs(x = "Year", y = "Mean of Net Income Component (in Millions)", fill = "Net Income Component") +
    theme(legend.position = "bottom",
          strip.text.x = element_text(angle = 0, face = "bold", size = rel(1.5)))
    






