rm(list = ls())

library('class')
library('e1071')

print("################################################")
print("              INICIO DEL PROYECTO")
print("################################################")
print("")
print("")

# Training data
data = read.csv("features_all_train.csv")
# Test data
testData = read.csv("features_all_test.csv")

totalColNumber = ncol(data)
lastDataColumn = totalColNumber-1

training_data_set = data[, c(1:lastDataColumn)]
training_set_labels =  data[, c(totalColNumber:totalColNumber)]

testing_data_set = testData[, c(1:lastDataColumn)]
testing_set_labels =  testData[, c(totalColNumber:totalColNumber)]

#fases del proyecto
#hacer n iteraciones hasta que el modelo generado no mejore. En cada iteracion se realiza lo siguiente
'
  1. se obtiene un numero aleatorio que representa una columna (atributo) del dataset.
  2. La columna seleccionada, se añade al modelo y se entrena. EL modelo en caso de estar vacio, será es columna solamente.
  3. Se entrena el modelo compuesto por K columnas.
  4. Se obtiene el indice de acierto del modelo y se compara con el indice de acierto del anterio modelo. En caso de ser el primer modelo que se genere,
    El indice de acierto anterior será - 1.
    Si el indice de acierto es mayor que el anterior, la columna seleccionada se añade como atributo.
    De lo contrario, la columna seleccionada, no se añade como atributo y se descarta como seleccion para futuras iteraciones
  5 .Repetir este proceso hasta que el modelo predictivo no mejore.
'

#Esta funcion genera un indice de columna aleatorio que no haya sido descartado anteriormente
getRandomAttributeIndex = function(totalColNumber, unusedColumnList) {
  randomIndexAlreadyDiscard = TRUE
  randomIndex = -1
  while(randomIndexAlreadyDiscard){
    randomIndex = sample(1:totalColNumber, 1)
    randomIndexAlreadyDiscard = is.element(randomIndex, unusedColumnList)
  }
  return(randomIndex)
}

generateMultipleColumnsDataFrame = function(sourceDataFrame, columnList){
  #TODO añadir un poquito de magia aqui!
  print("Generating multiple columns dataframe for training...")
  newDataFrame = NaN
  if(length(selectedColumnsList)>0){
    newDataFrame = sourceDataFrame[columnList]
  }
  else{
    newDataFrame = NULL
  }
  return(newDataFrame)
}

selectedColumnsList = c()
selectedColumnsListIdx = 1
unusedColumnList = c()
unusedColumnListIdx = 1

keepImprovingModel = TRUE
previousIterationModelScore = -1
currentIteration = 0

bayesResultModel = NaN
while(keepImprovingModel){
  # establecer el numero de la iteracion actual
  currentIteration = currentIteration + 1
  print("Iteration: ")
  print(currentIteration)

  print("Used columns: ")
  print(selectedColumnsList)

  print("unused columns: ")
  print(unusedColumnList)

  datosColumnaSeleccionada = generateMultipleColumnsDataFrame(training_data_set, selectedColumnsList)
  
  # 1.2 se hace un print para comprobar que son correctos. debug
  print(datosColumnaSeleccionada)

  thisIterationTrainingLabels = training_set_labels

  # 2. Añadir la columna seleccionada al modelo
  thisIterationTrainingModel = datosColumnaSeleccionada
  thisIterationModelScore = -1

  # Se genera el modelo solamente si hay datos. De lo contrario, la function naiveBayes da error
  if(length(selectedColumnsList)>0){
    # generar el modelo de bayes con los atributos seleccionados aleatoriamente
    bayes = naiveBayes(
            thisIterationTrainingModel,
            thisIterationTrainingLabels,
            laplace = 0
          )

    # impribir el resultado del modelo. debug
    print(bayes)

    bayesResultModel = predict(
      bayes,
      testing_set_labels,
      type="class"
    )

    # form and display confusion matrix & overall accuracy
    tab <- table(bayesResultModel, testing_set_labels)
    print("Results: Confusion Matrix") 
    print(tab)
    thisIterationModelScore = sum(tab[row(tab)==col(tab)])/sum(tab)
    print("This iteration model accuracy")
    print(thisIterationModelScore)

    print("Previous iteration model accuracy")
    print(previousIterationModelScore)

    # 5 Comprobar si el modelo predictivo generado en esta iteracion es mas preciso que el anterior
    keepImprovingModel = thisIterationModelScore > previousIterationModelScore
    print("Is better model than previous one?")
    print(keepImprovingModel)

    if(keepImprovingModel){
      # 5.1 Asignar el valor actual al anterior
      previousIterationModelScore = thisIterationModelScore
      
      # Como el modelo ha resultado ser mejor que el anterior, se selecciona otra columna de forma aleatoria para la siguiente iteracion
      
      # Se elige una columna de manera aleatoria para la siguiente posible iteracion
      # 1. obtener el numero de la columna aleatoria (obtener un atributo aleatorio)
      idxColumnaAleatoria = getRandomAttributeIndex(totalColNumber, unusedColumnList)

      # 1.0 Mostrar el indice seleccionado. debug
      print(idxColumnaAleatoria)

      # guardar la columna seleccionada para usarla en la siguiente iteracion
      selectedColumnsList[selectedColumnsListIdx] = idxColumnaAleatoria
      # aumentar el indice de la lista
      selectedColumnsListIdx = selectedColumnsListIdx + 1
    }
    else{
      # si resulta que no mejora, descartar el atributo seleccionado
      # se mueve el ultimo IDX añadido de la lista selectedColumnsListIdx a la lista unusedColumnList

      # valor a mover
      blackListedColumnValue = selectedColumnsList[selectedColumnsListIdx-1];
      
      #se elimina de la lista de columnas seleccionadas
      selectedColumnsList <- selectedColumnsList[-length(selectedColumnsList)]

      # se añade a la lista de las columnas blacklisted
      unusedColumnList[unusedColumnListIdx] = blackListedColumnValue
      # se incrementa el index de la lista blacklisted
      unusedColumnListIdx = unusedColumnListIdx + 1
    }
  }
  else{
    # se ejecuta la primera vez. cuando el modelo esta vacio
    print("Seleccionando datos de partida para la siguiente iteracion usando una columna aleatoria...")
    
    #se elige una columna de manera aleatoria para la siguiente posible iteracion
    # 1. obtener el numero de la columna aleatoria (obtener un atributo aleatorio)
    idxColumnaAleatoria = getRandomAttributeIndex(totalColNumber, unusedColumnList)

    # 1.0 Mostrar el indice seleccionado. debug
    print(idxColumnaAleatoria)

    # guardar la columna seleccionada para usarla en la siguiente iteracion
    selectedColumnsList[selectedColumnsListIdx] = idxColumnaAleatoria
    # aumentar el indice de la lista
    selectedColumnsListIdx = selectedColumnsListIdx + 1
  }
  print("Unused column list for next iteration")
  print(unusedColumnList)
}

print("################################################")
print("               FIN DEL PROYECTO")
print("################################################")
