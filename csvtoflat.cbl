      ******************************************************************
      * csv record parsing field extraction program COBOL version
      * converts comma delimited .csv format file into a fixed width
      * field flatfile - web version
      * CopyLeft 2015-2016 Josh Roybal - all wrongs reserved
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ECHOFILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE 
               ASSIGN TO WS-FILENAME
               ORGANIZATION LINE SEQUENTIAL.
           SELECT OUTPUT-FILE 
               ASSIGN TO "flatfile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD INPUT-FILE.
       01 INPUT-RECORD                  PIC X(512).
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD                 PIC X(512).
 
       WORKING-STORAGE SECTION.
       77 WS-FILENAME                   PIC X(50).
       77 WS-MAX-FLDS                   PIC 99 VALUE 20.
       77 WS-NO-FLDS                    PIC 99 VALUE 1.
       77 WS-FLD-IDX                    PIC 99.
       77 WS-FLD-NO                     PIC 99.
       77 WS-REC-IDX                    PIC 999.
       77 WS-NO-RECS                    PIC ZZZZZZ9.
       77 WS-NO-RECS-STR                PIC X(7) VALUE SPACES.
       77 WS-NO-OF-DIGITS               PIC 9. 
       77 WS-REC-LEN                    PIC 999.
       77 WS-MAX-LEN                    PIC 999.
      *****************************************************************
      * WS-COUNTER and WS-FLD-LEN are for use in the FIND-FIELD-LENGTHS
      * section.
      *****************************************************************
       77 WS-COUNTER                    PIC 99.
       77 WS-FLD-LEN                    PIC 99.
      *****************************************************************
      * WS-START, WS-FINISH, and WS-REC-NO are for use in the 
      * WRITE-FIELDS-TO-BUFFER section.
      *****************************************************************
       77 WS-START                      PIC 999.
       77 WS-FINISH                     PIC 999.
       77 WS-REC-NO                     PIC 9(7) VALUE ZERO.
       01 NO-MORE-RECORDS               PIC X(1) VALUE SPACE. 
       01 WS-RECORD-FIELDS.
           05 WS-RECORD-FIELD           PIC X(50) OCCURS 20 TIMES.
       01 WS-FIELD-LENGTHS.
           05 WS-FIELD-LENGTH           PIC 99 OCCURS 20 TIMES.
       01 WS-MAX-LENGTHS.
           05 WS-MAX-LENGTH             PIC 99 OCCURS 20 TIMES.

       PROCEDURE DIVISION.
           ACCEPT WS-FILENAME FROM COMMAND-LINE
           MOVE 0 TO WS-REC-NO
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO INPUT-RECORD
           PERFORM UNTIL NO-MORE-RECORDS = 'Y'
               MOVE SPACES TO INPUT-RECORD
               READ INPUT-FILE INTO INPUT-RECORD
               AT END 
               MOVE 'Y' TO NO-MORE-RECORDS
               NOT AT END
               IF INPUT-RECORD NOT = SPACES
                   PERFORM EXTRACT-FIELDS
                   PERFORM FIND-FIELD-LENGTHS
                   SET WS-REC-NO UP BY 1
               END-IF
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE
           MOVE WS-REC-NO TO WS-NO-RECS
           MOVE WS-NO-RECS TO WS-NO-RECS-STR
      * find the number of digits that we shall have to accomodate for
      * the record no. field subtracting the leadings blanks, if any, 
      * from 7.
           MOVE 1 TO WS-COUNTER
           PERFORM UNTIL WS-NO-RECS-STR(WS-COUNTER:1) NOT = SPACE
               SET WS-COUNTER UP BY 1
           END-PERFORM
           SET WS-COUNTER DOWN BY 1
           SUBTRACT WS-COUNTER FROM 7 GIVING WS-NO-OF-DIGITS
      ******************************************************************
      * now that we have the maximum field lengths data we make second
      * pass through the csv and write out the flat file
      ******************************************************************
           MOVE SPACES TO OUTPUT-RECORD
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           MOVE 'N' TO NO-MORE-RECORDS
           PERFORM COMPUTE-MAX-LEN
      ******************************************************************
      * main write to output file loop
      ******************************************************************
          MOVE 0 TO WS-REC-NO
          PERFORM UNTIL NO-MORE-RECORDS = 'Y'
               MOVE SPACES TO INPUT-RECORD
               READ INPUT-FILE INTO INPUT-RECORD
               AT END 
               MOVE 'Y' TO NO-MORE-RECORDS
               NOT AT END 
               PERFORM EXTRACT-FIELDS
               PERFORM WRITE-FIELDS-TO-BUFFER
               END-READ
      * do not write/rewrite records when the line is blank/empty
               IF INPUT-RECORD NOT EQUAL TO SPACES 
                   WRITE OUTPUT-RECORD BEFORE ADVANCING 0 LINE
               SET WS-REC-NO UP BY 1
           END-PERFORM
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

      ******************************************************************
      * extract field(s) from comma delimited sequential file record
      ******************************************************************
       EXTRACT-FIELDS SECTION.
           MOVE 1 TO WS-REC-IDX
           MOVE 1 TO WS-FLD-IDX
           MOVE 1 TO WS-FLD-NO
           PERFORM FIND-RECORD-LENGTH
           PERFORM UNTIL WS-FLD-NO > WS-MAX-FLDS 
           OR WS-REC-IDX > WS-REC-LEN
               MOVE SPACES TO WS-RECORD-FIELD(WS-FLD-NO)
               PERFORM UNTIL INPUT-RECORD(WS-REC-IDX:1) = ","
                   OR WS-REC-IDX > WS-REC-LEN
                   IF INPUT-RECORD(WS-REC-IDX:1) NOT = '"'
                       MOVE INPUT-RECORD(WS-REC-IDX:1) 
                       TO WS-RECORD-FIELD(WS-FLD-NO)(WS-FLD-IDX:1)
                       SET WS-REC-IDX UP BY 1
                       SET WS-FLD-IDX UP BY 1
      ******************************************************************
      * step through any double quoted substrings and adjust the indices
      * accordingly
      ******************************************************************
                   ELSE
                       SET WS-REC-IDX UP BY 1 
                       PERFORM UNTIL INPUT-RECORD(WS-REC-IDX:1) = '"'
                       OR WS-FLD-IDX > 50
                           MOVE INPUT-RECORD(WS-REC-IDX:1) 
                           TO WS-RECORD-FIELD(WS-FLD-NO)(WS-FLD-IDX:1)
                           SET WS-FLD-IDX UP BY 1
                           SET WS-REC-IDX UP BY 1
                       END-PERFORM
                       SET WS-REC-IDX UP BY 1
                   END-IF
               END-PERFORM
               SET WS-REC-IDX UP BY 1
               MOVE 1 TO WS-FLD-IDX
               SET WS-FLD-NO UP BY 1
           END-PERFORM
           SET WS-FLD-NO DOWN BY 1
           MOVE WS-FLD-NO TO WS-NO-FLDS.

      ******************************************************************
      * never trust intrinsic functions until you know what you're doing
      * hand crafted string length fetching subroutine just like I've 
      * been writing since 1980
      ******************************************************************
       FIND-RECORD-LENGTH SECTION.
           MOVE 512 TO WS-REC-LEN
           PERFORM UNTIL INPUT-RECORD(WS-REC-LEN:1) NOT EQUAL TO SPACE
               OR WS-REC-LEN IS EQUAL TO ZERO
               SET WS-REC-LEN DOWN BY 1
           END-PERFORM.

      ******************************************************************
      * subroutine finds the lengths of the fields and stores them in
      * the lengths of fields array/table
      ******************************************************************
       FIND-FIELD-LENGTHS SECTION.
           MOVE 1 TO WS-COUNTER
           PERFORM UNTIL WS-COUNTER > WS-NO-FLDS
               MOVE 50 TO WS-FIELD-LENGTH(WS-COUNTER)
               PERFORM UNTIL 
           WS-RECORD-FIELD(WS-COUNTER)(WS-FIELD-LENGTH(WS-COUNTER):1) 
           NOT = ' ' OR WS-FIELD-LENGTH(WS-COUNTER) = 0
                   SET WS-FIELD-LENGTH(WS-COUNTER) DOWN BY 1
               END-PERFORM
               IF WS-FIELD-LENGTH(WS-COUNTER) >
                   WS-MAX-LENGTH(WS-COUNTER) 
                       MOVE WS-FIELD-LENGTH(WS-COUNTER)
                       TO WS-MAX-LENGTH(WS-COUNTER)
               END-IF
               SET WS-COUNTER UP BY 1
           END-PERFORM.

      ******************************************************************
      * subroutine writes fields to subsections of the output record 
      * buffer as determined by the maximum field lengths
      ******************************************************************
       WRITE-FIELDS-TO-BUFFER SECTION.
           MOVE SPACES TO OUTPUT-RECORD
           MOVE 1 TO WS-COUNTER
           MOVE 1 TO WS-START
           MOVE WS-REC-NO(7 - WS-NO-OF-DIGITS + 1:7) 
           TO OUTPUT-RECORD(WS-START:WS-NO-OF-DIGITS)
           SET WS-START UP BY WS-NO-OF-DIGITS
           PERFORM UNTIL WS-COUNTER > WS-NO-FLDS
               MOVE WS-MAX-LENGTH(WS-COUNTER) TO WS-FINISH
               MOVE WS-RECORD-FIELD(WS-COUNTER) 
               TO OUTPUT-RECORD(WS-START:WS-FINISH)
               SET WS-START UP BY WS-MAX-LENGTH(WS-COUNTER)
               SET WS-COUNTER UP BY 1
           END-PERFORM
      ******************************************************************
      * I don't know if CR+LF plays nice with Apple, and I don't care.
      ******************************************************************
           MOVE X"0D" TO OUTPUT-RECORD(WS-MAX-LEN + 1:WS-MAX-LEN + 1)
           MOVE X"0A" TO OUTPUT-RECORD(WS-MAX-LEN + 2:WS-MAX-LEN + 2).

      ******************************************************************
      * subroutine computes the invarian maximum record length
      ******************************************************************
       COMPUTE-MAX-LEN SECTION.
           MOVE 1 TO WS-COUNTER
           MOVE WS-NO-OF-DIGITS TO WS-MAX-LEN
           PERFORM UNTIL WS-COUNTER IS GREATER THAN WS-NO-FLDS
               ADD WS-MAX-LENGTH(WS-COUNTER) TO WS-MAX-LEN
               SET WS-COUNTER UP BY 1
           END-PERFORM.           
