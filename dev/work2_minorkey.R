# key matching testcase

data <- data.frame("Strain" = c("Var1", "Var2", "RM03", "Var4", "Var5", "RM06"))
subKey <- data.frame("Strain" = c("RM01", "RM02", "RM03", "RM04", "RM05", "RM06", "RM-extra"),
                     "Variant" = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var-extra"))
targetCol <- "Strain"


replacements <- subKey[ ,targetCol][match(data[ ,targetCol], subKey[ ,2])]
originals <- data[,targetCol][!is.na(replacements)]
testData[,targetCol][!is.na(replacements)] <- replacements[!is.na(replacements)]

message(paste0("Renaming column ", targetCol, ": \n"))
message(paste(unique(originals[!is.na(originals)]),
              "renamed", unique(replacements[!is.na(replacements)]), "\n"))


indices <- match(data[,targetCol], key[, 2])
replacements <- key$Strain[indices]

data[!is.na(replacements), targetCol] <- replacements[!is.na(replacements)]



#

key <- data.frame(A = 1:3, B = LETTERS[1:3])

test <- c(2, 5, 4, 1)

swaps <- key$B[match(test, key$A)]
swaps

test[!is.na(swaps)] <- swaps[!is.na(swaps)]
test
