# PRINCIPAL COMPONENT PLOTS

# explained variance comparison
# base pca
plot(pca1$sdev/sum(pca1$sdev))
plot(pca2$sdev/sum(pca2$sdev))
plot(pca3$sdev/sum(pca3$sdev))
plot(pca4$sdev/sum(pca4$sdev))
plot(pca5$sdev/sum(pca5$sdev))
plot(pca6$sdev/sum(pca6$sdev))
# base lag
# ...
# lag kron
plot(kron.eVal1/sum(kron.eVal1))
plot(kron.eVal2/sum(kron.eVal2))
plot(kron.eVal3/sum(kron.eVal3))
plot(kron.eVal4/sum(kron.eVal4))
plot(kron.eVal5/sum(kron.eVal5))
plot(kron.eVal6/sum(kron.eVal6))
#

# lag sparse kron 1
plot(eigen(spkronpca1)$values/sum(eigen(spkronpca1)$values))
plot(eigen(spkronpca2)$values/sum(eigen(spkronpca2)$values))
plot(eigen(spkronpca3)$values/sum(eigen(spkronpca3)$values))
plot(eigen(spkronpca4)$values/sum(eigen(spkronpca4)$values))
plot(eigen(spkronpca5)$values/sum(eigen(spkronpca5)$values))
plot(eigen(spkronpca6)$values/sum(eigen(spkronpca6)$values))
# lag sparse kron 2
plot(eigen(spkron1.pdbefore)$values/sum(eigen(spkron1.pdbefore)$values))
plot(eigen(spkron2.pdbefore)$values/sum(eigen(spkron2.pdbefore)$values))
plot(eigen(spkron3.pdbefore)$values/sum(eigen(spkron3.pdbefore)$values))
plot(eigen(spkron4.pdbefore)$values/sum(eigen(spkron4.pdbefore)$values))
plot(eigen(spkron5.pdbefore)$values/sum(eigen(spkron5.pdbefore)$values))
plot(eigen(spkron6.pdbefore)$values/sum(eigen(spkron6.pdbefore)$values))
# lag sparse kron 3
plot(eigen(spkron1.pdlater)$values/sum(eigen(spkron1.pdlater)$values))
plot(eigen(spkron2.pdlater)$values/sum(eigen(spkron2.pdlater)$values))
plot(eigen(spkron3.pdlater)$values/sum(eigen(spkron3.pdlater)$values))
plot(eigen(spkron4.pdlater)$values/sum(eigen(spkron4.pdlater)$values))
plot(eigen(spkron5.pdlater)$values/sum(eigen(spkron5.pdlater)$values))
plot(eigen(spkron6.pdlater)$values/sum(eigen(spkron6.pdlater)$values))







# lag+pow sparse kron 1
plot(eigen(sppowkronpca1)$values/sum(eigen(sppowkronpca1)$values))
plot(eigen(sppowkronpca2)$values/sum(eigen(sppowkronpca2)$values))
plot(eigen(sppowkronpca3)$values/sum(eigen(sppowkronpca3)$values))
plot(eigen(sppowkronpca4)$values/sum(eigen(sppowkronpca4)$values))
plot(eigen(sppowkronpca5)$values/sum(eigen(sppowkronpca5)$values))
plot(eigen(sppowkronpca6)$values/sum(eigen(sppowkronpca6)$values))
# lag + pow sparse kron 2
plot(eigen(sppowkron1.pdbefore)$values / sum(eigen(sppowkron1.pdbefore)$values))
plot(eigen(sppowkron2.pdbefore)$values / sum(eigen(sppowkron2.pdbefore)$values))
plot(eigen(sppowkron3.pdbefore)$values / sum(eigen(sppowkron3.pdbefore)$values))
plot(eigen(sppowkron4.pdbefore)$values / sum(eigen(sppowkron4.pdbefore)$values))
plot(eigen(sppowkron5.pdbefore)$values / sum(eigen(sppowkron5.pdbefore)$values))
plot(eigen(sppowkron6.pdbefore)$values / sum(eigen(sppowkron6.pdbefore)$values))
# lag + pow sparse kron 3
plot(eigen(sppowkron1.pdlater)$values / sum(eigen(sppowkron1.pdlater)$values))
plot(eigen(sppowkron2.pdlater)$values / sum(eigen(sppowkron2.pdlater)$values))
plot(eigen(sppowkron3.pdlater)$values / sum(eigen(sppowkron3.pdlater)$values))
plot(eigen(sppowkron4.pdlater)$values / sum(eigen(sppowkron4.pdlater)$values))
plot(eigen(sppowkron5.pdlater)$values / sum(eigen(sppowkron5.pdlater)$values))
plot(eigen(sppowkron6.pdlater)$values / sum(eigen(sppowkron6.pdlater)$values))



