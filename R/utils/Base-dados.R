# Base preço dos alimentos Ceasa
data <- readxl::read_excel("R/utils/Dados.xlsx", 
                                col_types = c("text", "text", "text", "numeric", "date")) |> 
  dplyr::select(id,produte,unit,date,value) |>  
  tidyr::drop_na()


### para a tabela do pacote DT
pt <- list(
    sEmptyTable = "Nenhum registro encontrado",
    sProcessing= "A processar...",
    sLengthMenu= "Mostrar _MENU_ registos",
    sZeroRecords= "Não foram encontrados resultados",
    sInfo= "Mostrando de _START_ até _END_ de _TOTAL_ registos",
    sInfoEmpty= "Mostrando de 0 até 0 de 0 registos",
    sInfoFiltered= "(filtrado de _MAX_ registos no total)",
    sInfoPostFix= "",
    sSearch= "Procurar:",
    sUrl= "",
    oPaginate= list(
      sFirst= "Primeiro",
      sPrevious= "Anterior",
      sNext= "Seguinte",
      sLast= "Último"
    ),
    oAria= list(
      sSortAscending= "= Ordenar colunas de forma ascendente",
      sSortDescending= "= Ordenar colunas de forma descendente"
    )
  )


# Função para todas as bases nescessarias






