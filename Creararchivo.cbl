       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREARCLIENTES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES
               ASSIGN TO "clientes.dat"
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTES.
       01  CLIENTE-REC.
           05  CR-TIPO-REG            PIC X(01).
           05  CR-ID-CLIENTE          PIC 9(10) COMP-3.
           05  CR-SUCURSAL            PIC 9(5)  COMP-3.
           05  CR-NOMBRE              PIC X(30).
           05  CR-FECHA-ALTA          PIC 9(8)  COMP-3.
           05  CR-ESTADO              PIC X(01).
           05  CR-SALARIO             PIC S9(9)V9(2) COMP-3.
           05  CR-CATEGORIA           PIC 9(1)  COMP-3.
           05  CR-PUNTOS              PIC 9(5)  COMP-3.
           05  CR-FILLER              PIC X(24).

       PROCEDURE DIVISION.
       MAIN.
           OPEN OUTPUT CLIENTES

           MOVE 'D'            TO CR-TIPO-REG
           MOVE 1234567890     TO CR-ID-CLIENTE
           MOVE 100            TO CR-SUCURSAL
           MOVE 'JUAN PEREZ'   TO CR-NOMBRE
           MOVE 20240101       TO CR-FECHA-ALTA
           MOVE 'A'            TO CR-ESTADO
           MOVE 1234567.89     TO CR-SALARIO
           MOVE 1              TO CR-CATEGORIA
           MOVE 250            TO CR-PUNTOS
           MOVE SPACES         TO CR-FILLER

           WRITE CLIENTE-REC

           CLOSE CLIENTES
           STOP RUN.

