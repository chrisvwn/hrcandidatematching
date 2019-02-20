#pdfFilename <- "pdfforms/Ricci_Questionario_para_Elaboracao_Curricular.pdf"
library(RMySQL)
extractPDFData <- function(pdfFilename)
{
  tryCatch({
    options(stringsAsFactors = F)
    
    #clientsFilename <- "clients.csv"
  
    tempPdfOutputFile <- tempfile()
   
    system(paste0('pdftk "', pdfFilename, '" dump_data_fields_utf8 output ', tempPdfOutputFile))
    
    if(!file.exists(tempPdfOutputFile))
      stop("Error: pdftk text output not found")
    
    conn <- file(tempPdfOutputFile)
    pdfTxt <- readLines(conn, encoding = 'UTF-8')
    close(conn)
  
    # if(file.exists(clientsFilename))
    #   clients <- readr::read_csv(file = clientsFilename,
    #                              col_types = paste0(rep_len('c',205), collapse=""))
    # else
    #   clients <- NULL
    
    #append a '---' line since we use it
    #to delimit lines from the same field
    # the last field does not have it
    pdfTxt <- c(pdfTxt, "---")
    
    seps <- grep("---", pdfTxt)
    
    fields <- sapply(1:(length(seps)-1), 
               function(i)
               {
                 fieldVals <- grep("FieldName|FieldValue",
                      pdfTxt[seps[i]:seps[i+1]],
                      value = T)
                 
                 if(length(fieldVals) == 1)
                   fieldVals <- c(fieldVals, "FieldValue: NA")
                 
                 cbind(fieldVals[1], fieldVals[2])
               })
    
    #fields <- t(fields)
    
    #remove the keywords
    fields[1,] <- trimws(gsub("FieldName:", "", fields[1,]))
    fields[2,] <- trimws(gsub("FieldValue:", "", fields[2,]))
    
    #name correctly
    newFieldNames <- c("Pessoal_NomeCompleto", "Pessoal_Endereco", "Pessoal_Telefone",
                         "Pessoal_Celular", "Pessoal_Email","Pessoal_Cidade","Pessoal_Estado",
                         "Pessoal_CEP","Resumo_ResumirExperienciaProfissional", "Resumo_PontosFortes",
                         "Posicao_Cargo_Interesse1","Posicao_Cargo_Interesse2","Posicao_Cargo_Interesse3",
                         "Setor_Segmento_Industrial_Desejado",  "Salario_Atual", "Salario_Pretensao",
                         "Salario_Minimo_Aceitval","Mudanca_Carreira_Yes", "Disponibilidade_Para_Viagens_Yes",
                         "Disponibilidade_Mudanca_Cidade_Yes","Mudanca_Carreira_No",
                         "Disponibilidade_Para_Viagens_No","Disponibilidade_Mudanca_Cidade_No",
                         "Certificacoes1", "Certificacoes2", "Certificacoes3", "Certificacoes4",
                         "Certificacoes5", "Formacao_01_Nome_Instituicao", "Formacao_01_Área_de_Estudo", 
                         "Formacao_01_Tipo_Formacao","Formacao_01_Período", "Formacao_01_Cidade",
                         "Formacao_01_Sigla",  "Cursos_Treinamentos_01","Cursos_Treinamentos_02",
                         "Cursos_Treinamentos_03", "Cursos_Treinamentos_04", "Formacao_02_Nome_Instituicao",
                         "Formacao_02_Área_Estudo", "Formacao_02_Tipo_Formacao", "Formacao_02_Período",
                         "Formacao_02_Cidade", "Formacao_02_Sigla", "Formacao_03_Nome_Instituicao",
                         "Formacao_03_Área_Estudo", "Formacao_03_Tipo_Formacao", "Formacao_03_Período",
                         "Formacao_03_Sigla", "TextField_27", "Organizacao_01_Nome", "Organizacao_02_Nome",
                         "Premiacao_01_Título", "Organizacao_01_Título", "Organizacao_02_Título",
                         "Organizacao_01_Período", "Organizacao_02_Período", "Premiacao_01_Emissor",
                         "Premiacao_01_Data", "Premiacao_02_Título", "Premiacao_02_Emissor", "Premiacao_02_Data",
                         "Expertises_Skills", "Check_Box_1", "Check_Box_3", "Check_Box_5", "Check_Box_6",
                         "Check_Box_8", "Check_Box_9", "Check_Box_11", "Check_Box_12", "Premiacao_01_Título_2",
                         "contratado_fazer", "contratado_fazer2", "Nomenclatura_cargo", "Cidade", "Nome_Empresa",
                         "Text_Field_11", "Formacao_01_Resultados", "Formacao_01_Dia", "Formacao_01_Departamento",
                         "Formacao_01_Equipe", "Formacao_01_Gerenciava", "Formacao_01_Melhorias",
                         "Formacao_01_Processos", "Formacao_01_Relatórios", "Formacao_01_Carteira_Clientes",
                         "Formacao_01_Treinou", "Formacao_01_Projetos", "Formacao_01_Software", "Formacao_01_Promocao",
                         "Formacao_01_Orgulho", "Formacao_01_Elogíos", "Formacao_01_Desempenho", "Formacao_01_Era_conhecido",
                         "Formacao_01_Conseguir", "Formacao_01_Desmoronar", "Formacao_01_Grupo", "Formacao_01_Clientes", 
                         "Formacao_01_Tipos_Clientes", "Formacao_01_Assegurou", "Formacao_01_Reunioes", "Formacao_01_Evento",
                         "Formacao_01_Apresentacoes", "Text_Field_97", "O_que_voce_foi_contratado_para_fazer_3",
                         "O_que_voce_foi_contratado_para_fazer_4", "Nomenclatura_cargo_2", "Cidade_2",
                         "Nome_Empresa_2", "Text_Field_70", "Formacao_01_Resultados_2", "Formacao_01_Dia_2", 
                         "Formacao_01_Departamento_2", "Formacao_01_Equipe_ 2", "Formacao_01_Gerenciava_2", 
                         "Formacao_01_Melhorias_2", "Formacao_01_Processos_2", "Formacao_01_Relatorios_2",
                         "Formacao_01_Carteira_Clientes_2", "Formacao_01_Treinou_2", "Formacao_01_Projetos_2",
                         "Formacao_01_Software_2", "Formacao_01_Promocao_2", "Formacao_01_Orgulho_2", "Formacao_01_Elogíos_2", 
                         "Formacao_01_Desempenho_2","Formacao_01_Era_conhecido_2", "Formacao_01_Conseguir_2",
                         "Formacao_01_Desmoronar_2", "Formacao_01_Grupo_2", "Formacao_Clientes_2", "Formacao_01_Tipos_Clientes_2",
                         "Formacao_01_Assegurou_2", "Formacao_01_Reunioes_2", "Formacao_01_Evento_2", "Formacao_01_Apresentacoes_2",
                         "Text_Field_98", "O que voce foi contratado para fazer_5", "O que voce foi contratado para fazer 6",
                         "Nomenclatura_cargo_3", "Cidade_3", "Nome_Empresa_3", "Text_Field_99", "Formacao_01_Resultados_3",
                         "Formacao_01_Dia_3", "Formacao_01_Departamento_3", "Formacao_01_Equipe_3", "Formacao_01_Gerenciava_3",
                         "Formacao_Melhorias_3", "Formacao_01_Processos_3", "Formacao_01_Relatórios_3",
                         "Formacao_01_Carteira_Clientes_3", "Formacao_01_Treinou_3", "Formacao_01_Projetos_3",
                         "Formacao_01_Software_3", "Formacao_01_Promocao_3", "Formacao_01_Orgulho_3", "Formacao_01_Elogíos_3",
                         "Formacao_01_Desempenho_3", "Formacao_01_Era_conhecido_3", "Formacao_01_Conseguir_3",
                         "Formacao_01_Desmoronar_3", "Formacao_01_Grupo_3", "Formacao_01_Clientes_3", "Formacao_01_Tipos_Clientes_3",
                         "Formacao_01_Assegurou_3", "Formacao_01_Reunioes_3", "Formacao_01_Evento_3", "Formacao_01_Apresentacoes_3",
                         "Text_Field_100", "O_que_voce_foi_contratado_para_fazer_7", " O_que_voce foi_contratado_para_fazer_8",
                         "Nomenclatura_cargo_4", "Cidade_4", "Nome_Empresa_4", "Text_Field_101", "Formacao_01_Resultados_4",
                         "Formacao_01_Dia_4", "Formacao_01_Departamento_4", "Formacao_Equipe_4", "Formacao_01_Gerenciava_4",
                         "Formacao_01_Melhorias_4", "Formacao_01_Processos_4", "Formacao_01_Relatorios_4",
                         "Formacao_01_Carteira_Clientes_4", "Formacao_01_Treinou_4", "Formacao_01_Projetos_4",
                         "Formacao_01_Software_4", "Formacao_01_Promocao_4", "Formacao_01_Orgulho_4", "Formacao_01_Elogíos_4",
                         "Formacao_01_Desempenho_4", "Formacao_01_Era_conhecido_4", "Formacao_01_Conseguir_4", 
                         "Formacao_01_Desmoronar_4", "Formacao_01_Grupo_4", "Formacao_01_Clientes_4", "Formacao_01_Tipos_Clientes_4",
                         "Formacao_01_Assegurou_4", "Formacao_01_Reunioes_4", "Formacao_01_Evento_4", "Formacao_01_Apresentacoes_4",
                         "Text_Field_102", "Final"
                         
    )
    
    fields[1, 1:13] <- newFieldNames[1:13]
    
    fieldsDF <- setNames(as.data.frame(t(fields[2,])), as.character(fields[1,]))
    
    if(nrow(fieldsDF) == 0)
    {
      message("No data found")
      return(FALSE)
    }
    
  #  if(is.null(clients))
  #  {
  #    clients <- fieldsDF
  #  }
  #  else
  #    clients <- rbind.data.frame(clients, fieldsDF)
    
    #readr::write_csv(clients, clientsFilename)
    dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), host=dbHost, user=dbUser, db=dbName)
    
    #save client name to clients table and retrieve clients.id
    #begin transaction
    RMariaDB::dbBegin(dbCon)
    
    res <- RMariaDB::dbExecute(dbCon, paste0('INSERT INTO Clients VALUES(0,"', fieldsDF$Pessoal_NomeCompleto,'");'))

    client <- try(RMariaDB::dbGetQuery(dbCon, paste0('SELECT Id FROM Clients WHERE Name = "', fieldsDF$Pessoal_NomeCompleto,'";')),TRUE)

    #transpose fieldsDF to change back to key-value and prepend userid col
    fields <- setNames(data.frame(t(fields)), c("Property", "Value"))
    fieldsDF <- cbind.data.frame("ClientId"=client$Id, fields)
    
    res <- RMySQL::dbWriteTable(conn = dbCon, name = 'ClientDetails', value = fieldsDF, append=T, row.names=F)

    #if we get here all is well. commit the transaction
    RMariaDB::dbCommit(dbCon)
    
    unlink(tempPdfOutputFile, force = TRUE)
    
    dbDisconnect(dbCon)
    
    return(res)
  },error=function(err)
  {
    message(attr(err, 'condition'))
    
    message("Rolling back")
    dbRollback(dbCon)
    
    message("Disconnecting from DB")
    dbDisconnect(dbCon)
    
    return(FALSE)
  })
}
