#Private methods
setAs("character", "rInvoke", function(from) new("rInvoke", inputs = from))    

getSBMLString = function(sbmlModel, asText)
{
    if(!asText)
        sbml = toString.XMLNode(xmlInternalTreeParse(sbmlModel))
    else
        sbml = sbmlModel
    
    #Remove the XML heading
    sbml = strsplit(sbml,'\\?>')[[1]][[(length(sbml)+1)]]
    #Remove the leading \n
    if(substr(sbml,1,1)=="\n")
        sbml = substr(sbml, 2, nchar(sbml))
    return(sbml)
}





setBasisMethod = function(method) 
{
   req = method
   req = paste("<method name=\"", method, "\">", sep="")
   return(req)
}

addParam = function(reqStr, param)
{
   req = reqStr
   req = paste(reqStr, "<param>", param, "</param>", sep="")
   return(req)
}

completeReq = function(reqStr)
{
   req = reqStr
   req = paste(reqStr, "</method>", sep="")
   return(req)
}

getBasisConection = function(wsdl, verbose=TRUE)
{
    
    basis.wsdl = processWSDL(wsdl)
    basis.ff = genSOAPClientInterface(def = basis.wsdl, verbose = verbose)
    return(basis.ff)
}
  

callWebService = function(wsdl, req, verbose)
{
    req = completeReq(req)
    basis.ff = getBasisConection(wsdl, verbose)
    rst = basis.ff@functions$rInvoke(req)
    return(rst@result)
}

basis2df = function(sim_data)
{
    y = strsplit(sim_data,"\n")[[1]]

    headers = c(strsplit(y[1],"\t")[[1]])
    
    mat = matrix(0,length(y), length(headers)+1)
    sim_no = 0
    j=1;
    for(i in 2:length(y))
    {
        ss = strsplit(y[i], " ")[[1]]
        if(ss[1] == '#')
            sim_no = sim_no + 1
        else
        {
            mat[j,] = as.numeric(c(c(strsplit(y[i],"\t")[[1]]), sim_no))
            j = j+1
        }
    }
    df1 = as.data.frame(mat)
    colnames(df1) = c(headers,"sim_no")
    df1 = df1[df1$sim_no != 0,]
    return(df1)
}

forwardbasis2df = function(sim_data)
{
    z = unlist(strsplit(sim_data,"\n"))

    no_of_sps = length(unlist(strsplit(z[1]," ")))
    headers = unlist(strsplit(z[1]," "))
    z = z[2:length(z)]#Remove headers
    z_vector = unlist(lapply(z,strsplit," "))
    z_vector = as.numeric(z_vector)

    dim(z_vector) = c(no_of_sps, length(z_vector)/no_of_sps)
    
    mat = t(z_vector)
    df1 = as.data.frame(mat)
    colnames(df1) = headers
   
    return(df1)
}



############################################################
##TODO
#1. Determine function name dynamically
#2. Cycle through the argument list dynamically
############################################################

#Public methods
echo = function(arg1, arg2, verbose=FALSE)
{
    wsdl = "http://basis1.ncl.ac.uk:81//BasisWSRPortablService.wsdl"

    req = setBasisMethod("echo")
    req = addParam(req, arg1)
    req = addParam(req, arg2)
    return(callWebService(wsdl, req, verbose))

}

mod2sbml = function(mod, asText=FALSE, verbose=FALSE)
{
    if(!asText)
        mod = paste(readLines(mod), collapse = "\n")
    
    wsdl = "http://basis1.ncl.ac.uk:81//BasisWSRPortablService.wsdl"
    req = setBasisMethod("mod2sbml")
    req = addParam(req, mod)
    sbml = callWebService(wsdl, req, verbose)
    
    return(sbml)
}

getSessionID = function(username, password, verbose=FALSE)
{
    wsdl = "http://basis1.ncl.ac.uk:81//BasisWSRPortablService.wsdl"
    req = setBasisMethod("getSessionId")
    req = addParam(req, username)
    req = addParam(req, password)
    return(callWebService(wsdl, req, verbose))
}

putSBML = function(sid, sbmlModel, asText=FALSE, verbose=FALSE)
{
    wsdl = "http://basis1.ncl.ac.uk:81//BasisWSRPortablService.wsdl"
    sbml = getSBMLString(sbml, asText)

    req = setBasisMethod("putSBML")
    req = addParam(req, sid)
    req = addParam(req, sbml)
    return(callWebService(wsdl, req, verbose))
}

simulateModel = function(sid, model_urn, name, max_time, sims, iters, verbose=FALSE)
{
    checkModelUrn(model_urn)
    
    wsdl = "http://basis1.ncl.ac.uk:81//BasisWSRPortablService.wsdl"
    req = setBasisMethod("simulate")
    req = addParam(req, sid)
    req = addParam(req, model_urn)
    req = addParam(req, name)
    req = addParam(req, max_time)
    req = addParam(req, sims)
    req = addParam(req, iters)
    return(callWebService(wsdl, req, verbose))
}
    
isBasisFinished = function(sid, sim_urn, verbose=FALSE)
{
    wsdl = "http://basis1.ncl.ac.uk:81//BasisWSRPortablService.wsdl"
    req = setBasisMethod("isFinished")
    req = addParam(req, sid)
    req = addParam(req, sim_urn)
    result = callWebService(wsdl, req, verbose)
    if(result=="w")
        return("waiting")
    else if(result=="r")
        return("running")
    else if(result=="f")
        return("finished")
    else
        return(result)
}
   
getRawData = function(sid, sim_urn, verbose=FALSE)
{
    wsdl = "http://basis1.ncl.ac.uk:81//BasisWSRPortablService.wsdl"
    req = setBasisMethod("getRawData")
    req = addParam(req, sid)
    req = addParam(req, sim_urn)
    result = callWebService(wsdl, req, verbose)
    return(basis2df(result))
}



##########################################################
#Forward Simulate#########################################
##########################################################


forwardSimulate = function(sbml, max_time, iters, asText=FALSE, verbose=FALSE)
{
    wsdl = "http://basis1.ncl.ac.uk:81/BasisWSRPortablService.wsdl"
    
    sbml = getSBMLString(sbml, asText)

    req = setBasisMethod("forwardSimSubmit")
    req = addParam(req, sbml)
    req = addParam(req, max_time)
    req = addParam(req, iters)
    result = callWebService(wsdl, req, verbose)
    return(result)    
}


isForwardSimReady = function(sid, verbose=FALSE)
{   
    wsdl = "http://basis1.ncl.ac.uk:81/BasisWSRPortablService.wsdl"

    req = setBasisMethod("isForSimReady")
    req = addParam(req, sid)
    status = callWebService(wsdl, req, verbose)
    if(status=="false")
        return(FALSE)
    else if(status=="true")
        return(TRUE)
    else
        return(status)
}
 
getForwardSimResult = function(sid, verbose=FALSE)
{
    wsdl = "http://basis1.ncl.ac.uk:81/BasisWSRPortablService.wsdl"

    req = setBasisMethod("getForSimResult")
    req = addParam(req, sid)
    result = callWebService(wsdl, req, verbose)

    return(forwardbasis2df(result))
}



