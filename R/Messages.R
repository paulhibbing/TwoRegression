message_update <- function(message_number, ...) {
    switch(message_number, cat("\n     Getting VM for variables searched on the following criteri(a/on):", 
        vm_variables, "\n"))
}
