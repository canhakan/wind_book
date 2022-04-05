# Basic Functions

# Powered ---------------------------------------------------------------------
createPowered <- function(data, powers, other.head = 0, other.tail = 0){
    # remove others
    other = c(other.head,other.tail)
    dat = data[,-..other]
    # save it for binding at last
    res = dat
    # create powered features
    for(i in powers){
        tem.dat = dat
        for(p in c(2:i)){
            tem.dat = tem.dat*dat
        }
        colnames(tem.dat) = paste(colnames(tem.dat),paste('p',i,sep=''),sep='_')
        res = cbind(res,tem.dat)
    }
    res = cbind(data[,..other.head],res,data[,..other.tail])
    return(res)
}



# Lagged ------------------------------------------------------------------
createLagged <- function(data, lags, other.head = 0, other.tail = 0) {
    # remove 0 if it exists
    # get min/max for removing first/last rows for lag/lead
    minlag = min(0,lags)
    maxlag = max(0,lags)
    startN = 1 - minlag
    endN = nrow(data) - maxlag
    # seperating others
    other = c(other.head, other.tail)
    tobe.head = data[(startN:endN),..other.head]
    tobe.tail = data[(startN:endN),..other.tail]
    dat = data.table() # empty data table
    # adding lagged values one by one (column by column)
    for(i in lags){
        lagdat = data[((startN+i):(endN+i)),-..other]
        if(i<0){
            lagname = paste('lag',-i,sep='')
            colnames(lagdat) = paste(colnames(lagdat),lagname,sep='_')
        } else if(i>0){
            lagname = paste('lead',i,sep='')
            colnames(lagdat) = paste(colnames(lagdat),lagname,sep='_')
        }
        dat = cbind(dat,lagdat)
    }
    dat = cbind(tobe.head,dat,tobe.tail)
    return(dat)
}






# Temporal ----------------------------------------------------------------

# times should be the maximum time difference we want
createTemporal <- function(data, times, other){
    d = data[,-..other]
    l = nrow(d)
    res = data.table()
    for(i in c(0:times)){
        di = c(as.matrix(d[((1+times-i):(l-i)),]))
        res = cbind(res,di)
    }
    # colnames
    newnames = c()
    for(i in c(0:times)){
        tempname = paste("t", i, sep="")
        newnames = c(newnames, tempname)
    }
    colnames(res) = newnames
    return(res)
}



# function: Compare improvements over base model as percentage



# PC Picker ---------------------------------------------------------------
pcPicker <- function(eigenvalues, var_wanted=0.99){
    total = sum(eigenvalues)
    var_explained = 0
    i = 0
    for(ev in eigenvalues){
        i = i + 1
        var_explained = var_explained + ev/total
        if(var_explained > var_wanted){
            break
        }
    }
    return(i)
}


# PC Picker Advanced (for kronecker etc.)
pcLister <- function(eigenvalues, var_wanted){
    x = c()
    var_explained = 0
    total = sum(eigenvalues)
    while(var_explained < var_wanted){
        i = which.max(eigenvalues)
        x = c(x, i)
        e = eigenvalues[i]
        eigenvalues[i] = -100
        var_explained = var_explained + e/total
    }
    return(x)
}

# PC Picker Advanced with Complex numbers
pcListerComplex <- function(eigenvalues, var_wanted){
    eigenvalues = Re(eigenvalues)
    x = c()
    var_explained = 0
    total = sum(eigenvalues)
    while(var_explained < var_wanted){
        i = which.max(eigenvalues)
        x = c(x, i)
        e = eigenvalues[i]
        eigenvalues[i] = -100
        var_explained = var_explained + e/total
    }
    return(x)
}


# Neighbour Mapping -------------------------------------------------------
# returns neighbors and itself
neighborLister <- function(nlat,nlon,i){
    res = c(i)
    # touching left?
    if(i %% nlat != 1){
        res = c(res, (i-1))
    }
    # touching right?
    if(i %% nlat != 0){
        res = c(res, (i+1))
    }
    # touching botton?
    if(i > nlat){
        res = c(res, (i-nlat))
    }
    # touching top?
    if(i <= nlat*(nlon-1)){
        res = c(res, (i+nlat))
    }
    return(res)
}


# Sparse Maker 1Neighbor ------------------------------------------------------------
sparseMaker <- function(matrix, nlat, nlon){
    for(i in c(1:nrow(matrix))){
        neis = neighborLister(nlat = nlat, nlon = nlon, i = i)
        a = matrix[,i]
        a[-neis] = 0
        matrix[,i] = a
    }
    return(matrix)
}


# Sparse Maker Temporal t-2 ---------------------------------------------------
sparseTimer <- function(matrix){
    hersey = c(1:nrow(matrix))
    for(i in c(1:nrow(matrix))){
        a = matrix[,i]
        a[abs(hersey - i) > 2] = 0
        matrix[,i] = a
    }
    return(matrix)
}



# Sparse Maker with Powers --------------------------------------------
# assumes the order as: loc1 loc2 ... loc1_p2, loc2_p2, ....

sparseMakerPower <- function(matrix, nlat, nlon, power){
    for(i in c(1:nrow(matrix))){
        neis = neighborLister(nlat = nlat, nlon = nlon, i = i)
        neispow = neis
        tempneis = neis
        for(j in c(2:power)){
            tempneis = tempneis + nlat*nlon
            neispow = c(neispow, tempneis)
        }
        a = matrix[,i]
        a[-neispow] = 0
        matrix[,i] = a
    }
    return(matrix)
}
