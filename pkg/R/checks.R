checkModelUrn = function(model_urn)
{
    
    if(substr(model_urn, 1, 20) != "urn:basis.ncl:model:")
        stop("\nInvalid Model urn.\n Model urn should be of the form urn:basis.ncl:model:DDDD")
       
    tmp =  as.integer(substr(model_urn, 21, nchar(model_urn)))
    if(!is.integer(tmp))
        stop("\nInvalid Model urn.\n Model urn should be of the form urn:basis.ncl:model:DDDD")
}
    
