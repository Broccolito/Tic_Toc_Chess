###########################
#    Tic Toc Chess
#    Created by Broccolito
#    Sep 23 2018
###########################

tic_toc = function(){
  #Matrix to store chess data
  initialize_chessmat = function(){
    chess_mat <<- matrix(rep(0,9),
                         nrow = 3, 
                         ncol = 3)
  }
  #Chessboard generating function
  chessboard = function(){
    add_line = function(cord1, cord2){
      plot(cord1,
           cord2,
           type = "l", 
           xlim = c(-0.5, 2.5),
           ylim = c(-0.5, 2.5), 
           xaxt = "n",
           yaxt = "n",
           ann = FALSE,
           bty = "n")
    }
    x_1 = c(0.5, 0.5)
    y_1 = c(-0.5, 2.5)
    x_2 = c(1.5, 1.5)
    y_2 = c(-0.5, 2.5)
    x_3 = c(-0.5, 2.5)
    y_3 = c(0.5, 0.5)
    x_4 = c(-0.5, 2.5)
    y_4 = c(1.5, 1.5)
    x_5 = c(-0.5,-0.5)
    y_5 = c(-0.5, 2.5)
    x_6 = c(2.5, 2.5)
    y_6 = c(-0.5, 2.5)
    x_7 = c(-0.5, 2.5)
    y_7 = c(-0.5, -0.5)
    x_8 = c(-0.5, 2.5)
    y_8 = c(2.5, 2.5)
    graphics.off()
    add_line(x_1, y_1)
    par(new = TRUE)
    add_line(x_2, y_2)
    par(new = TRUE)
    add_line(x_3, y_3)
    par(new = TRUE)
    add_line(x_4, y_4)
    par(new = TRUE)
    add_line(x_5, y_5)
    par(new = TRUE)
    add_line(x_6, y_6)
    par(new = TRUE)
    add_line(x_7, y_7)
    par(new = TRUE)
    add_line(x_8, y_8)
    par(new = FALSE)
  }
  update_chessboard = function(){
    chessboard()
    for(i in 0:2){
      for(j in 0:2){
        if(chess_mat[i+1,j+1] == 1){
          points(i,j, cex = 10)
        }else if(chess_mat[i+1,j+1] == 2){
          points(i,j, pch = 16, cex = 10, col = "black")
        }
      }
    }
  }
  play_white = function(){
    cat("
        7 # 8 # 9
        #   #
        ###########
        4 # 5 # 6
        #   #
        ###########
        1 # 2 # 3
        #   #
        \n")
    cat("It is white's turn!\n")
    cat("Where do you want to play?\n")
    spot = as.numeric(readline("  "))
    #Check to see if input is out of range
    while(any(spot < 1, spot > 9)){
      cat("Input is out of range. \n")
      cat("Please input again... \n")
      spot = as.numeric(readline("  "))
    }
    #Check to see if the spot has been occupied
    while(any(chess_mat[spot] == c(1,2))){
      cat("Input spot is already occupied. \n")
      cat("Please input again... \n")
      spot = as.numeric(readline("  "))
    }
    chess_mat[spot] <<- 1
    update_chessboard()
  }
  play_black = function(){
    cat("
        7 # 8 # 9
        #   #
        ###########
        4 # 5 # 6
        #   #
        ###########
        1 # 2 # 3
        #   #
        \n")
    cat("It is black's turn!\n")
    cat("Where do you want to play?\n")
    spot = as.numeric(readline("  "))
    #Check to see if input is out of range
    while(any(spot < 1, spot > 9)){
      cat("Input is out of range. \n")
      cat("Please input again... \n")
      spot = as.numeric(readline("  "))
    }
    #Check to see if the spot has been occupied
    while(any(chess_mat[spot] == c(1,2))){
      cat("Input spot is already occupied. \n")
      cat("Please input again... \n")
      spot = as.numeric(readline("  "))
    }
    chess_mat[spot] <<- 2
    update_chessboard()
  }
  computer_play_white = function(){
    available_spots = which(chess_mat == 0)
    get_spot_white = function(){
      #Offence
      if(
        all(sum(chess_mat[1:3] == 1) == 2, sum(chess_mat[1:3] == 2) == 0)
      ){
        return(available_spots[available_spots <= 3])
      }
      if(
        all(sum(chess_mat[4:6] == 1) == 2, sum(chess_mat[4:6] == 2) == 0)
      ){
        return(available_spots[available_spots >= 4 | available_spots <= 6])
      }
      if(
        all(sum(chess_mat[7:9] == 1) == 2, sum(chess_mat[7:9] == 2) == 0)
      ){
        return(available_spots[available_spots >= 7 | available_spots <= 9])
      }
      if(
        all(sum(chess_mat[c(1,4,7)] == 1) == 2, sum(chess_mat[c(1,4,7)] == 2) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 4 | available_spots == 7])
      }
      if(
        all(sum(chess_mat[c(2,5,8)] == 1) == 2, sum(chess_mat[c(2,5,8)] == 2) == 0)
      ){
        return(available_spots[available_spots == 2 | available_spots == 5 | available_spots == 8])
      }
      if(
        all(sum(chess_mat[c(3,6,9)] == 1) == 2, sum(chess_mat[c(3,6,9)] == 2) == 0)
      ){
        return(available_spots[available_spots == 3 | available_spots == 6 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(1,5,9)] == 1) == 2, sum(chess_mat[c(1,5,9)] == 2) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 5 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(7,5,3)] == 1) == 2, sum(chess_mat[c(7,5,3)] == 2) == 0)
      ){
        return(available_spots[available_spots == 7 | available_spots == 5 | available_spots == 3])
      }
      #Defence
      if(
        all(sum(chess_mat[1:3] == 2) == 2, sum(chess_mat[1:3] == 1) == 0)
      ){
        return(available_spots[available_spots <= 3])
      }
      if(
        all(sum(chess_mat[4:6] == 2) == 2, sum(chess_mat[4:6] == 1) == 0)
      ){
        return(available_spots[available_spots >= 4 | available_spots <= 6])
      }
      if(
        all(sum(chess_mat[7:9] == 2) == 2, sum(chess_mat[7:9] == 1) == 0)
      ){
        return(available_spots[available_spots >= 7 | available_spots <= 9])
      }
      if(
        all(sum(chess_mat[c(1,4,7)] == 2) == 2, sum(chess_mat[c(1,4,7)] == 1) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 4 | available_spots == 7])
      }
      if(
        all(sum(chess_mat[c(2,5,8)] == 2) == 2, sum(chess_mat[c(2,5,8)] == 1) == 0)
      ){
        return(available_spots[available_spots == 2 | available_spots == 5 | available_spots == 8])
      }
      if(
        all(sum(chess_mat[c(3,6,9)] == 2) == 2, sum(chess_mat[c(3,6,9)] == 1) == 0)
      ){
        return(available_spots[available_spots == 2 | available_spots == 6 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(1,5,9)] == 2) == 2, sum(chess_mat[c(1,5,9)] == 1) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 5 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(7,5,3)] == 2) == 2, sum(chess_mat[c(7,5,3)] == 1) == 0)
      ){
        return(available_spots[available_spots == 7 | available_spots == 5 | available_spots == 2])
      }
      #Build up advantage
      if(any(available_spots == 5)){
        return(5)
      }else if(length(available_spots[available_spots == 2 | 
                                      available_spots == 4 | 
                                      available_spots == 6 | 
                                      available_spots == 8]) > 0){
        return(sample(available_spots[available_spots == 2 | 
                                        available_spots == 4 | 
                                        available_spots == 6 | 
                                        available_spots == 8], 1))
      }else if(length(available_spots[available_spots == 2 | 
                                      available_spots == 4 | 
                                      available_spots == 6 | 
                                      available_spots == 8]) > 0){
        return(sample(available_spots[available_spots == 1 | 
                                        available_spots == 3 | 
                                        available_spots == 7 | 
                                        available_spots == 9], 1))
      }
    }
    if(length(available_spots) >= 1){
      chess_mat[get_spot_white()[1]] <<- 1
      update_chessboard()
    }
  }
  computer_play_black = function(){
    available_spots = which(chess_mat == 0)
    get_spot_black = function(){
      #Offence
      if(
        all(sum(chess_mat[1:3] == 2) == 2, sum(chess_mat[1:3] == 1) == 0)
      ){
        return(available_spots[available_spots <= 3])
      }
      if(
        all(sum(chess_mat[4:6] == 2) == 2, sum(chess_mat[4:6] == 1) == 0)
      ){
        return(available_spots[available_spots >= 4 | available_spots <= 6])
      }
      if(
        all(sum(chess_mat[7:9] == 2) == 2, sum(chess_mat[7:9] == 1) == 0)
      ){
        return(available_spots[available_spots >= 7 | available_spots <= 9])
      }
      if(
        all(sum(chess_mat[c(1,4,7)] == 2) == 2, sum(chess_mat[c(1,4,7)] == 1) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 4 | available_spots == 7])
      }
      if(
        all(sum(chess_mat[c(2,5,8)] == 2) == 2, sum(chess_mat[c(2,5,8)] == 1) == 0)
      ){
        return(available_spots[available_spots == 2 | available_spots == 5 | available_spots == 8])
      }
      if(
        all(sum(chess_mat[c(3,6,9)] == 2) == 2, sum(chess_mat[c(3,6,9)] == 1) == 0)
      ){
        return(available_spots[available_spots == 2 | available_spots == 6 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(1,5,9)] == 2) == 2, sum(chess_mat[c(1,5,9)] == 1) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 5 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(7,5,3)] == 2) == 2, sum(chess_mat[c(7,5,3)] == 1) == 0)
      ){
        return(available_spots[available_spots == 7 | available_spots == 5 | available_spots == 2])
      }
      #Defence
      if(
        all(sum(chess_mat[1:3] == 1) == 2, sum(chess_mat[1:3] == 2) == 0)
      ){
        return(available_spots[available_spots <= 3])
      }
      if(
        all(sum(chess_mat[4:6] == 1) == 2, sum(chess_mat[4:6] == 2) == 0)
      ){
        return(available_spots[available_spots >= 4 | available_spots <= 6])
      }
      if(
        all(sum(chess_mat[7:9] == 1) == 2, sum(chess_mat[7:9] == 2) == 0)
      ){
        return(available_spots[available_spots >= 7 | available_spots <= 9])
      }
      if(
        all(sum(chess_mat[c(1,4,7)] == 1) == 2, sum(chess_mat[c(1,4,7)] == 2) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 4 | available_spots == 7])
      }
      if(
        all(sum(chess_mat[c(2,5,8)] == 1) == 2, sum(chess_mat[c(2,5,8)] == 2) == 0)
      ){
        return(available_spots[available_spots == 2 | available_spots == 5 | available_spots == 8])
      }
      if(
        all(sum(chess_mat[c(3,6,9)] == 1) == 2, sum(chess_mat[c(3,6,9)] == 2) == 0)
      ){
        return(available_spots[available_spots == 3 | available_spots == 6 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(1,5,9)] == 1) == 2, sum(chess_mat[c(1,5,9)] == 2) == 0)
      ){
        return(available_spots[available_spots == 1 | available_spots == 5 | available_spots == 9])
      }
      if(
        all(sum(chess_mat[c(7,5,3)] == 1) == 2, sum(chess_mat[c(7,5,3)] == 2) == 0)
      ){
        return(available_spots[available_spots == 7 | available_spots == 5 | available_spots == 3])
      }
      #Build up advantage
      if(any(available_spots == 5)){
        return(5)
      }else if(length(available_spots[available_spots == 2 | 
                                      available_spots == 4 | 
                                      available_spots == 6 | 
                                      available_spots == 8]) > 0){
        return(sample(available_spots[available_spots == 2 | 
                                        available_spots == 4 | 
                                        available_spots == 6 | 
                                        available_spots == 8], 1))
      }else if(length(available_spots[available_spots == 2 | 
                                      available_spots == 4 | 
                                      available_spots == 6 | 
                                      available_spots == 8]) > 0){
        return(sample(available_spots[available_spots == 1 | 
                                        available_spots == 3 | 
                                        available_spots == 7 | 
                                        available_spots == 9], 1))
      }
    }
    if(length(available_spots) >= 1){
      chess_mat[get_spot_black()[1]] <<- 2
      update_chessboard()
    }
  }
  white_win = function(){
    return(
      any(
        all(chess_mat[1:3] == 1),
        all(chess_mat[4:6] == 1),
        all(chess_mat[7:9] == 1),
        all(chess_mat[c(1,4,7)] == 1),
        all(chess_mat[c(2,5,8)] == 1),
        all(chess_mat[c(3,6,9)] == 1),
        all(chess_mat[c(1,5,9)] == 1),
        all(chess_mat[c(7,5,3)] == 1)
      )
    )
  }
  black_win = function(){
    return(
      any(
        all(chess_mat[1:3] == 2),
        all(chess_mat[4:6] == 2),
        all(chess_mat[7:9] == 2),
        all(chess_mat[c(1,4,7)] == 2),
        all(chess_mat[c(2,5,8)] == 2),
        all(chess_mat[c(3,6,9)] == 2),
        all(chess_mat[c(1,5,9)] == 2),
        all(chess_mat[c(7,5,3)] == 2)
      )
    )
  }
  draw = function(){
    if(any(white_win(), black_win())){
      return(FALSE)
    }
    return(!any(chess_mat == 0))
  }
  critical = function(){
    return(
      any(
        white_win(),
        black_win(),
        draw())
    )
  }
  play_game_pvp = function(){
    initialize_chessmat()
    update_chessboard()
    n = 1
    while(!critical()){
      if(n %% 2 == 1){
        play_black()
      }else{
        play_white()
      }
      n = n + 1
    }
    if(white_win()){
      cat("White wins! \n")
    }else if(black_win()){
      cat("Black wins! \n")
    }else if(draw()){
      cat("It is a draw. \n")
    }
    cat("\nEnd of Game.\n")
  }
  play_game_pvc = function(){
    cat("Would you like to play White or Black? \n")
    cat("
        White: 1  ##  Black: 2
        ")
    choice = as.numeric(readline("  "))
    while(!any(choice == 1, choice == 2)){
      cat("Please choose to be White or Black... \n")
      cat("
          White: 1  ##  Black: 2
          ")
      choice = as.numeric(readline("  "))
    }
    if(choice == 1){
      initialize_chessmat()
      update_chessboard()
      n = 1
      while(!critical()){
        if(n %% 2 == 1){
          computer_play_black()
        }else{
          play_white()
        }
        n = n + 1
      }
      if(white_win()){
        cat("White wins! \n")
      }else if(black_win()){
        cat("Black wins! \n")
      }else if(draw()){
        cat("It is a draw. \n")
      }
      cat("\nEnd of Game.\n")
    }else if(choice == 2){
      initialize_chessmat()
      update_chessboard()
      n = 1
      while(!critical()){
        if(n %% 2 == 1){
          play_black()
        }else{
          computer_play_white()
        }
        n = n + 1
      }
      if(white_win()){
        cat("White wins! \n")
      }else if(black_win()){
        cat("Black wins! \n")
      }else if(draw()){
        cat("It is a draw. \n")
      }
      cat("\nEnd of Game.\n")
    }
  }
  start_game = function(){
    again = TRUE
    while(again){
      cat("Do you want to play with players or computer? \n")
      cat("
          Player: 1  ##  Computer: 2
          ")
      choice = as.numeric(readline("  "))
      while(!any(choice == 1, choice == 2)){
        cat("Please choose whom do you want to play with...\n")
        cat("
            Player: 1  ##  Computer: 2
            ")
        choice = as.numeric(readline("  "))
      }
      if(choice == 1){
        play_game_pvp()
      }else if(choice == 2){
        play_game_pvc()
      }
      cat("\n")
      cat("Do you want to play this game again? \n")
      cat("
          Yes: 1  ##  No: 2 
          ")
      choice = as.numeric(readline("  "))
      while(!any(choice == 1, choice == 2)){
        cat("Do you want to play this game again? \n")
        cat("
            Player: 1  ##  Computer: 2
            ")
        choice = as.numeric(readline("  "))
      }
      if(choice == 1){
        again = TRUE
      }else if(choice == 2){
        again = FALSE
      }
    }
    cat("
        ##################
        Thanks for playing!
        ##################
        ")
  }
  start_game()
  graphics.off()
}
tic_toc()