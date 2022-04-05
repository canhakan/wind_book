# denemeler

datas = list(data1,data2,data3,data4,data5,data6)

class(datas[[1]])
datas[1][1][2][[1]]

1
2
3
4
5
6

"aliaga"
"bares"
"dinar"
"geycek"
"soke"
"soma"


dim(laggedpowered1) # 4:192
lpp1 = polym(as.matrix(laggedpowered1[1,4:1]), degree = 2, raw = TRUE)
lpp1


dim(lagged1)

lp1 = polym(as.matrix(lagged1[,4:30]), degree = 2, raw = TRUE)
# 4:30'da bile => Error: vector memory exhausted (limit reached?)
# ve degree=2 yani. 3 de degil


data_explainer = data.table(region = c("Aliaga", "Bares", "Dinar", "Geycek", "Soke", "Soma"),
                            locations = c("3x3=9", "4x4=16", "4x4=16", "4x4=16", "4x4=16", "3x4=12"),
                            time_instances = c(33130, 33240, 33264, 24096, 33240, 33124))





results %>% filter(model == 'base')
results.pca


# Plotting for book -------------------------------------------------------

level_order <- factor(results$model, level = unique(results$model))
ggplot(results) +
    geom_point(mapping = aes(x = level_order, y = test_error, color =  station))

ggplot(results) +
    geom_point(mapping = aes(x = level_order, y = train_error, color =  station))




ggplot(results) +
    geom_point(mapping = aes(x = level_order, y = test_error, color =  station)) +
    scale_color_brewer(palette="Dark2") +
    labs(x = "Models", y = "Mean Absolute Error", title = "Full Models", subtitle = "Test Error Comparison")



# results.compare2 (with positive definite) -------------------------------
results.sparse.pdb = results.sparse.pd[model == "lagged/pdb" | model == "lagged + powered/pdb",]
results.sparse.pdl = results.sparse.pd[model == "lagged/pdl" | model == "lagged + powered/pdl",]

results.sparse.pdb$model = c(rep("lagged",6),rep("lagged + powered",6))
results.sparse.pdl$model = c(rep("lagged",6),rep("lagged + powered",6))


# pca book ------------!ONEMLI!-----------------------------
results[order(station)]
results.pca[order(station)]
results.kron[order(station)]
results.sparse[order(station)]

results.compare = results[1,][-1,]

for(st in unique(results$station)){
    for(md in unique(results.kron$model)){
        r1 = results %>% filter(station == st, model == md)
        r2 = results.pca %>% filter(station == st, model == md)
        r3 = results.kron %>% filter(station == st, model == md)
        r4 = results.sparse %>% filter(station == st, model == md)
        r5 = results.sparse.pdb %>% filter(station == st, model == md)
        r6 = results.sparse.pdl %>% filter(station == st, model == md)
        results.compare = rbind(results.compare, r1, r2, r3, r4, r5, r6)
    }
}
results.compare$method = rep(c("full", "pca", "kronecker", "sparse1", "sparse2", "sparse3"),12)

results.compare

### PLOTTTING ##
level_order2 <- factor(results.compare$method, level = unique(results.compare$method))[1:36]

ggplot(results.compare %>% filter(model == 'lagged')) +
    geom_point(mapping = aes(x = level_order2, y = test_error, color =  station)) +
    scale_color_brewer(palette="Dark2") +
    labs(x = "Models", y = "Mean Absolute Error", title = "Full Models", subtitle = "TEST Error Comparison")


ggplot(results.compare %>% filter(model == 'lagged + powered')) +
    geom_point(mapping = aes(x = level_order2, y = test_error, color =  station)) +
    scale_color_brewer(palette="Dark2") +
    labs(x = "Models", y = "Mean Absolute Error", title = "Full Models", subtitle = "TEST Error Comparison")

ggplot(results.compare %>% filter(model == 'lagged + powered')) +
    geom_point(mapping = aes(x = level_order2, y = train_error, color =  station)) +
    scale_color_brewer(palette="Dark2") +
    labs(x = "Models", y = "Mean Absolute Error", title = "Full Models", subtitle = "TRAIN Error Comparison")






