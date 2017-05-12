source("~/swissinfo/_helpers/helpers.R")
font <- "Alegreya Sans"


############################################################################################
###    settings
############################################################################################

table.all <- read.csv("secteursJob_suisseUEEtatsTiers.csv", stringsAsFactors = F)
table.all$nombre <- as.numeric(gsub("(\\(|\\))", "", table.all$nombre))

table.all$job <- table.all$job <- factor(gsub("’", "'", table.all$job))
table2003 <- table.all[table.all$Année == 2003,]
table2013 <- table.all[table.all$Année == 2013,]


# make a proper table
tableize <- function(tb) {
	job <- as.character(unique(tb$job))
	tb <- as.data.frame(matrix(tb$nombre, ncol = 4, nrow = nlevels(tb$job), byrow = T))
	tb <- cbind(tb, job = job)
	colnames(tb) <- c('total', 'suisse', 'UE', 'EtatsTiers', 'job')
	tb
}
tables <- list(`2003` = tableize(table2003), `2013` = tableize(table2013))

# add percentage étranger
percentify <- function(tb, numerator = "UE", denominator = "total") {
	sapply(tb, function(tbb) {
		cbind(tbb, perc = tbb[,numerator] / tbb[,denominator])
	},simplify = FALSE)
}
roundify <- function(x, ...) round(x * 100, ...)


############################################################################################
###    slopegraph UE%
############################################################################################

# percentage UE
tableUE <- percentify(tables)

### relative numbers
table <- data.frame(`2003` = roundify(tableUE[['2003']]$perc), `2013` = roundify(tableUE[['2013']]$perc), total = tableUE[['2013']]$total,
	UE2013 = tableUE[['2013']]$UE, UE2003 = tableUE[['2003']]$UE, job = tableUE[['2013']]$job)

#quantile(table$total, seq(0.1, 0.9, 0.1))
lineBreaks <- c(0, 250, 500, 750, 1000, Inf)

# ylim
#ylim <- c(min(table[,1:2]) - diff(range(table[,1:2]))/100,max(table[,1:2]) + diff(range(table[,1:2]))/100)

# get a color by job category
colors <- structure(colorRampPalette(brewer.pal(8,"Dark2"))(nlevels(factor(table$job))), names = levels(factor(table$job)))
table$colors <- colors[as.character(table$job)]


rownames(table) <- table$job
cex <- 2

# define line widths
table$lineSize <- cut(table$total, lineBreaks)
table$lineSize <- as.numeric(table$lineSize) * cex



pdf("UEVsSuisseEmploi.pdf", width = 20, height = 10)
par(xpd=NA, oma = c(0,35, 1, 20), family = font)
slopegraph(table[,1:2], rescaleByColumn = F, col.lines=table$colors, col.lab = table$colors, cex.lab = cex, cex.num = cex,
	offset.x = 0.05, offset.lab = 0.05,lwd = table$lineSize, lab.sep = 0.2)

legend(-1.2,10, lty=c(1,1,1,1,1), c("< 250'000", paste("> ", lineBreaks[-c(1, length(lineBreaks))],
	"'000", sep ="")), lwd = sort(unique(table$lineSize)), cex = cex, box.col = "white",box.lwd = 0, col = "darkgrey")
dev.off()

############################################################################################
###    slopegraph Etats Tiers
############################################################################################

# percentage Etats Tiers
tables2 <- percentify(tables, numerator = "EtatsTiers")

### relative numbers
table2 <- data.frame(`2003` = roundify(tables2[['2003']]$perc), `2013` = roundify(tables2[['2013']]$perc), total = tables2[['2013']]$total,
	job = tables2[['2013']]$job)


# get a color by job category
table2$colors <- colors[as.character(table2$job)]


rownames(table2) <- table2$job

# define line widths
table2$lineSize <- cut(table2$total, lineBreaks)
table2$lineSize <- as.numeric(table2$lineSize) * cex

pdf("EtatTiersVsSuisseEmploi.pdf", width = 20, height = 10)
par(xpd=NA, oma = c(0,35, 1, 20), family = font)
slopegraph(table2[,1:2], rescaleByColumn = F, col.lines=table2$colors, col.lab = table2$colors, cex.lab = cex, cex.num = cex,
	offset.x = 0.05, offset.lab = 0.05,lwd = table2$lineSize, lab.sep = 0.2)
dev.off()



#### TO DO print job with right colors

rownames(table2)[c(2,3,5,7,8,9)]


