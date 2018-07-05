mylist2 <-as.list(c(1,0)) 
Server <- (function(input, output,session) {
  

 
  output$Xv = renderUI({
    selectInput('Xval', 'Select X1 value', mylist2,multiple = FALSE)
  })
  output$Yv = renderUI({
    selectInput('Yval', 'Select X2 value', mylist2,multiple = FALSE)
  })
  # 
  observeEvent({input$Xval
                input$Yval} , {
                  
# sigmoid activation function
    sigmoid = function(x) {
      1 / (1 + exp(-x))
    }
# binary step activation function    
    binary_step_activation = function(x){
      if(x<0){
        return(0)
      }else{
        return(1)
      }
    }
# function to depict the input to a neuron    
    matixMult = function(x,y){
      return(x %*% y);
    }
    
#formation of matrix    
  MartixFormation <- function(x,y){
    # print(input$Xval);
    return(matrix(c(as.numeric(x),as.numeric(y),1), nrow=3, ncol=1))
  }
  # h1 neuron 
  h1_neuron = function(x){
    eq1<-matixMult(input1,x)
    return(sigmoid(eq1[1][1]))
  }
  # h2 neuron    
  h2_neuron = function(x){
    eq2<-matixMult(input2,x)
    return(sigmoid(eq2[1][1]))
  }
  # Y neuron 
  y_neuron = function(x){
    eq3<-matixMult(input3,x)
    print(eq3)
    y<-binary_step_activation(eq3[1][1])
  }
  input1 <- matrix(c(20,20,-10), nrow=1, ncol=3)
  input2 <- matrix(c(-20,-20,30), nrow=1, ncol=3)
  input3 <- matrix(c(20,20,-30), nrow=1, ncol=3)

    print(input$Yval);
    inputMartix<-MartixFormation(input$Xval,input$Yval)
    h1<- h1_neuron(inputMartix)
    h2<- h2_neuron(inputMartix)
    hMartix<- MartixFormation(h1,h2)
   result<-y_neuron(hMartix)

#Create the Neural  network graph  
    eq1<-matixMult(input1,inputMartix)
   eq2<-matixMult(input2,inputMartix)
   edges <- data.frame(from = c('X1','X2','X1','X2','h1','h2'), to = c('h1','h1','h2','h2','Y','Y'))
   nodes <- data.frame(id = c('X1','X2','h1','h2','Y'),label =c(input$Xval,input$Yval,eq1[1][1],eq2[1][1],result), group = c(rep("X", 2), rep("h", 2),"Y"))
   output$plottest<- renderVisNetwork({visNetwork( nodes,edges, height = "100%", width = "100%")%>%
       visEdges(arrows = "to") %>%
       visOptions(
         highlightNearest = list(enabled = TRUE, degree = 1,hover = TRUE)
       )%>%    visIgraphLayout( layout = "layout_with_sugiyama", physics = TRUE,
                                   smooth = FALSE, type = "full", randomSeed = NULL,
                                   layoutMatrix = NULL)%>%visPhysics(stabilization = FALSE)%>%visLegend(position='right',main = "neuron groups")
   })
   # Summary of the result
   Summary21<- paste("Summary: 
    XOR output for inputs X1 = ",input$Xval)
   Summary21<- paste(Summary21,"and X2 =")
   Summary21<- paste(Summary21,input$Yval)
   Summary21<- paste(Summary21," is equal to")
   Summary21<- paste(Summary21,result)
   output$Summary1<-renderText(Summary21)
  })

})