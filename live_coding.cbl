       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-READER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    "ORGANIZATION IS LINE SEQUENTIAL" specifies that the FILE
      *    IS a STANDARD TEXT FILE organized BY LINES.
      *    "ACCESS MODE IS SEQUENTIAL" means that the FILE IS READ
      *    line by line.
           SELECT EMP-FILE ASSIGN TO 'emp_file.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *File description used by COBOL to refer to the assigned file.
       FD EMP-FILE
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS EMP-RECORD.
      *EMP-RECORD will hold the line after reading it.
       01 EMP-RECORD             PIC X(100).

       WORKING-STORAGE SECTION.
      *88 is a special level that defines a condition that is TRUE WHEN
      *the preceeding variable (here WS-EOF-FLAG) CONTAINS the specified
      *value.
       01 WS-EOF-FLAG            PIC X(1) VALUE 'N'.                  
           88 EOF-REACHED                 VALUE 'Y'.
       01 WS-RECORD-COUNT        PIC 9(3) VALUE 0.

       01 EMP-DATA.
           02 EMP-ID             PIC 9(3).
           02 EMP-NAME           PIC A(10).
           02 EMP-SALARY         PIC 9(5).

       01 WS-TOTAL-SALARY        PIC 9(8) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY '*** STARTING FILE PROCESSING ***'

           OPEN INPUT EMP-FILE.

      *    The same as "PERFORM UNTIL WS-EOF-FLAG IS EQUAL TO 'Y'".
           PERFORM UNTIL EOF-REACHED
               READ EMP-FILE
                   AT END 
                       SET EOF-REACHED TO TRUE                      
                   NOT AT END
                       PERFORM PROCESS-RECORD 
               END-READ
           END-PERFORM.                        

           CLOSE EMP-FILE.                                          

           DISPLAY ' '.
           DISPLAY '*** FILE PROCESSING COMPLETE ***'.
           DISPLAY 'Total Records Processed: ' WS-RECORD-COUNT.
           DISPLAY 'Total salary paid: ' WS-TOTAL-SALARY.

           STOP RUN.

       PROCESS-RECORD.
           PERFORM UNSTRING-RECORD.

           DISPLAY 'ID: ' EMP-ID ' | Name: ' EMP-NAME ' | Salary: ' 
               EMP-SALARY.

           ADD 1 TO WS-RECORD-COUNT.                        
           ADD EMP-SALARY TO WS-TOTAL-SALARY.

       UNSTRING-RECORD.
      *    "ALL SPACE" means that COBOL treats one OR more instances
      *    of the SPACE character as one delimiter.
           UNSTRING EMP-RECORD DELIMITED BY ALL SPACE INTO
               EMP-ID, EMP-NAME, EMP-SALARY
           END-UNSTRING.
