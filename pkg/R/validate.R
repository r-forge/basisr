#This function uses the SBML validator at http://sbml.org/validator/

validateSBML = function(filename, output="text", units=FALSE,
                         overall=FALSE, identifier=FALSE, MathML=FALSE, 
                         SBO=FALSE, overdetermined=FALSE, modeling=FALSE)
{
    if(!file.exists(filename))
        stop(paste("File", filename, "does not exist"))
    
    valid_outputs = c("text", "xml", "xhtml", "json")
    if(!is.element(output, valid_outputs))
        stop("output must be either: text, xml, xhtml, or json")
        
    short_tests =  c("u", "g", "i", "m", "s", "o", "p")
   
    if(!units)
        short_tests = short_tests[short_tests!="u"]
    
    if(!overall)
        short_tests = short_tests[short_tests!="g"]  

    if(!identifier)
        short_tests = short_tests[short_tests!="i"]
        
    if(!MathML)
        short_tests = short_tests[short_tests!="m"]
   
    if(!SBO)
        short_tests = short_tests[short_tests!="s"]
 
    if(!overdetermined)
        short_tests = short_tests[short_tests!="o"]
    
    if(!modeling)
        short_tests = short_tests[short_tests!="m"]
  
   
    offcheck = short_tests[1]
    for(i in 2:(length(short_tests)))
        offcheck = paste(offcheck, ",",short_tests[i],sep='')
        
    y = postForm("http://sbml.org/validator/", "file" = fileUpload(filename), output=output, offcheck=offcheck)
    return(y)
}
