subcells4 <- list(c(1,2,1,2),c(1,2,3,4),c(3,4,1,2),c(3,4,3,4))
subcells9 <- list(c(1,3,1,3), c(1,3,4,6), c(1,3,7,9), c(4,6,1,3),c(4,6,4,6), c(4,6,7,9),c(7,9,1,3),c(7,9,4,6), c(7,9,7,9))
passes <<- 0
incomplete <<- NA

solve_row <- function(puzzle,row_num)
{
    pl <- sqrt(length(puzzle))
    Num_Range <- c(1:pl)
    completed <- FALSE
    for(col_num in Num_Range)
    {
        for(cell in subcells9)
        {
            if((row_num>=cell[1])&(row_num<=cell[2])&(col_num>=cell[3])&(col_num<=cell[4]))
            {
               pos_nums <- Num_Range
               sub_square <-(puzzle[cell[1]:cell[2],cell[3]:cell[4]])
               prow<-(puzzle[row_num,])
               pcol<-(puzzle[,col_num])
               taken_nums <- c(sub_square,prow,pcol)
               for(tn in taken_nums)
               {
                   pos_nums <- pos_nums[!pos_nums %in% tn]
               }
               if(length(pos_nums)==0)
               {
                       if(is.na(puzzle[row_num,pl]))
                       {
                           passes<<-passes+1
                           if(passes>20)
                           {
                               for(Num in Num_Range)
                               {                       
                                   puzzle[row_num,Num] <- NA
                                   puzzle[row_num-1,Num] <- NA
                               }
                               passes<<- 0
                               puzzle<-solve_row(puzzle,row_num-1)
                               puzzle<-solve_row(puzzle,row_num)
                               break                                 
                           }
                           else
                           {
                               for(Num in Num_Range)
                               {                       
                                   puzzle[row_num,Num] <- NA
                               }
                               puzzle<-solve_row(puzzle,row_num)
                               break
                           }
                       }                   
               }
               else if(length(pos_nums)==1)
               {
                   puzzle[row_num,col_num] <- pos_nums
                   if(col_num==pl)
                   {
                       return(puzzle)
                       break
                   }
               }
               else
               {
                   puzzle[row_num,col_num] <- sample(pos_nums,1)
               }
            }
            
        }
        
    }
    passes<<-0
    return(puzzle)
}

make_puzzle<- function(side_length,clues)
{
    puzzle<-matrix(nrow=side_length,ncol=side_length)
    row_range <-c(1:side_length)
    for(row in row_range)
    {
        puzzle <- solve_row(puzzle,row)
    }
    num_blanks <- (length(puzzle)-clues)
    while(num_blanks>0)
    {
        ran_col <- sample(1:side_length,1)
        ran_row <- sample(1:side_length,1)
        ran_cell <- puzzle[ran_row,ran_col]
        if(!is.na(ran_cell))
        {
            puzzle[ran_row,ran_col] <- NA
            num_blanks <- num_blanks -1
        }
    }
    incomplete <<- puzzle    
    return(puzzle)
}

sodoku <- function(puzzle)
{
    pl = sqrt(length(puzzle))
    i = 0
    solved <- TRUE
    solvable <- FALSE
    for (n in puzzle) 
    {        
        i = i+1
        srow = i%%pl
        if (srow==0)srow<-pl
        scol = ceiling(i/pl)
        pos_vals<- c(1:pl)
        prow<-(puzzle[srow,])
        pcol<-(puzzle[,scol])
        nums <- c()
        if (is.na(n))
        {
            for (cell in subcells9)
            {
                if((srow>=cell[1])&(srow<=cell[2])&(scol>=cell[3])&(scol<=cell[4]))
                {
                   square <-(puzzle[cell[1]:cell[2],cell[3]:cell[4]])
                   prow<-(puzzle[srow,])
                   pcol<-(puzzle[,scol])
                   nums <- c(square,prow,pcol)
                }
            }
            for(num in nums)
            {
                pos_vals <- pos_vals[!pos_vals %in% num]
            }
            if(length(pos_vals)==1)
            {
                puzzle[srow,scol] = pos_vals[1]
                solvable <-TRUE
            }
            else 
            {
                solved<- FALSE
            }
        
        }
    }
    if(solved == TRUE)
    {
        return(puzzle)
    }
    else
    {
        if(solvable)
        {
            sodoku(puzzle)
        }
        else
        {
            sodoku(make_puzzle(9,31))
        }
    }
}  
puzzle <- make_puzzle(9,31)
puzzle <- sodoku(puzzle)
print(incomplete)
print(puzzle)
