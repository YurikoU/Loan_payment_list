 IDENTIFICATION DIVISION.                                         
 PROGRAM-ID. PROG1.                                               
 AUTHOR. TIM PATRICK.                                             
 * LAB EXERCISE 3.                                                
 ENVIRONMENT DIVISION.                                            
 CONFIGURATION SECTION.                                           
 INPUT-OUTPUT SECTION.                                            
 FILE-CONTROL.                                                    
     SELECT INPUT-FILE   ASSIGN TO DA-S-INPUT.                    
     SELECT PRNT-FILE    ASSIGN TO UR-S-PRNT.                     
 DATA DIVISION.                                                   
 FILE SECTION.                                                    
 FD INPUT-FILE                                                    
     BLOCK CONTAINS 0 RECORDS                                     
     LABEL RECORDS ARE STANDARD.                                  
 01 INPUT-REC            PIC X(80).                               
 FD PRNT-FILE                                                     
     LABEL RECORDS ARE OMITTED.                                   
 01 PRNT-REC             PIC X(125).                              
 WORKING-STORAGE SECTION.                                         
***********************************************************       
*      LAYOUT FOR THE INPUT FILE                                  
***********************************************************       
 01 INPUT-DATA.                                                   
     03 I-NAME           PIC X(20).                               
     03 I-LOAN           PIC 99999V99.                            
     03 I-PAID1          PIC 9999V99.                             
     03 I-PAID2          PIC 9999V99.                             
     03 I-PAID3          PIC 9999V99.                             
     03 I-PAID4          PIC 9999V99.                             
     03 FILLER           PIC X(19).                               
***********************************************************       
*      LAYOUT FOR THE 1ST DATA LINE OF REPORT PRINTING            
***********************************************************       
 01 PRNT-DATA1.                                                   
     03 L-NAME           PIC X(20).                               
     03 FILLER           PIC X(1)    VALUE SPACES.                
     03 L-LOAN           PIC ZZZZ9.99.                            
     03 FILLER           PIC X(2)    VALUE SPACES.                
     03 L-PAID1          PIC ZZZ9.99.                             
     03 FILLER           PIC X(2)    VALUE SPACES.                
     03 L-PAID2          PIC ZZZ9.99.                             
     03 FILLER           PIC X(2)    VALUE SPACES.                
     03 L-PAID3          PIC ZZZ9.99.                             
     03 FILLER           PIC X(2)    VALUE SPACES.                
     03 L-PAID4          PIC ZZZ9.99.                             
     03 FILLER           PIC X(2)    VALUE SPACES.                
     03 L-TOTALPAID      PIC ZZZZ9.99.                            
     03 FILLER           PIC X(2)    VALUE SPACES.                
     03 L-BALANCE        PIC $ZZZZ9.99.                           
************************************************************      
*      LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING          
************************************************************      
 01 PRNT-HEADING1.                                                
     03                  PIC X(4)    VALUE 'NAME'.                
     03                  PIC X(17)   VALUE SPACES.                
     03                  PIC X(4)    VALUE 'LOAN'.                
     03                  PIC X(6)    VALUE SPACES.                
     03                  PIC X(5)    VALUE 'PAID1'.               
     03                  PIC X(4)    VALUE SPACES.                
     03                  PIC X(5)    VALUE 'PAID2'.               
     03                  PIC X(4)    VALUE SPACES.                
     03                  PIC X(5)    VALUE 'PAID3'.               
     03                  PIC X(4)    VALUE SPACES.                
     03                  PIC X(5)    VALUE 'PAID4'.               
     03                  PIC X(4)    VALUE SPACES.                
     03                  PIC X(8)    VALUE 'TOT PAID'.            
     03                  PIC X(2)    VALUE SPACES.                
     03                  PIC X(7)    VALUE 'BALANCE'.             
 01 MISC.                                                         
************************************************************      
*      END OF FILE (EOF) SWITCHES        *                        
*    0=NOT AT EOF       1=AT EOF        *                         
************************************************************      
     03 EOF-I            PIC 9        VALUE 0.                    
     03 TOTAL            PIC 99999V99.                            
     03 BALANCE          PIC 99999V99.                            
************************************************************      
*      START OF PROCEDURE DIVISION                                
************************************************************      
 PROCEDURE DIVISION.                                              
************************************************************      
* THE MAINLINE IS RESPONSIBILE FOR THE FLOW OF THE LOGIC          
* ALL MAIN PROCEDURES SHOULD BE CALLED FROM THE MAIN              
* EVERY PROCEDURE (PARAGRAPH) MUST BE DOCUMENTED                  
************************************************************      
 000-MAINLINE.                                                    
     OPEN INPUT INPUT-FILE                                        
          OUTPUT PRNT-FILE.                                       
     PERFORM 9000-READ-INPUT.                                     
     PERFORM 5000-PRINT-HEAD.                                     
     PERFORM 1000-LOOP                                            
         UNTIL EOF-I = 1.                                         
     CLOSE INPUT-FILE                                             
         PRNT-FILE.                                               
     STOP RUN.                                                    
************************************************************      
*      1000-LOOP CALLS 1600-PRINT-NAMES WHICH IS                  
*      RESPONSIBLE FOR MOVING DATA TO PRINT LINE                  
*      AND THEN PRINTING                                          
*      IT NEXT CALLS 9000-READ-INPUT WHICH WILL READ              
*      THE NEXT RECORD INTO THE STRUCTURE FOR PROCESSING          
************************************************************      
 1000-LOOP.                                                       
     PERFORM 1600-PRINT-NAMES.                                    
     PERFORM 9000-READ-INPUT.                                     
************************************************************      
*      1600-PRINT-NAMES WILL MOVE NECESSARY FIELDS TO THE         
*      PRINT STRUCTURE IN WORKING-STORAGE ASD THEN IT WILL        
*      PRINT THE INFORMATION                                      
************************************************************      
 1600-PRINT-NAMES.                                                
     MOVE I-NAME          TO L-NAME.                              
     MOVE I-LOAN          TO L-LOAN.                              
     MOVE I-PAID1         TO L-PAID1.                             
     MOVE I-PAID2         TO L-PAID2.                             
     MOVE I-PAID3         TO L-PAID3.                             
     MOVE I-PAID4         TO L-PAID4.                             
     COMPUTE TOTAL = I-PAID1 + I-PAID2 + I-PAID3 + I-PAID4.       
     MOVE TOTAL           TO L-TOTALPAID.                         
     COMPUTE BALANCE = I-LOAN - TOTAL.                            
     MOVE BALANCE         TO L-BALANCE.                           
     WRITE PRNT-REC FROM PRNT-DATA1                               
           AFTER ADVANCING 1 LINE.                                
************************************************************      
*      5000-PRINT-HEAD PRINTS A HEADER LINE AFTER IT MOVES        
*      TO A NEW PAGE                                              
************************************************************      
 5000-PRINT-HEAD.                                                 
     WRITE PRNT-REC FROM PRNT-HEADING1                            
           AFTER ADVANCING PAGE.                                  
     MOVE SPACES TO PRNT-REC.                                     
     WRITE PRNT-REC                                               
           AFTER ADVANCING 1 LINE.                                
************************************************************      
*      9000-READ-INPUT READS A RECORD AT A TIME                   
*      THE RECORD IS READ INTO THE STRUCTURE SET UP IN            
*      WORKING STORAGE                                            
************************************************************      
 9000-READ-INPUT.                                                 
     READ INPUT-FILE INTO INPUT-DATA                              
          AT END MOVE 1 TO EOF-I.                                 