# COBOL2GO
**Diego Fraiese**

An example demonstrating how to convert COBOL to Go.

---

## Overview

**COBOL2GO** is a small, educational project that demonstrates how classic COBOL batch data and control-flow concepts can be interpreted and executed using Go.

The goal is not to replace COBOL, but to **understand, decode, and reason about legacy COBOL systems** outside the mainframe by preserving:
- the original data layout (copybook-style)
- sequential file processing
- familiar COBOL control structures

This project serves as a **bridge example** between traditional COBOL batch processing and modern Go-based tooling.

---

## How this example works

This example shows how a classic COBOL batch file can be processed outside the mainframe using Go, while preserving both the **data layout** and the **procedural style** familiar to COBOL developers.

### 1. COBOL record structure (copybook-style layout)

The binary file processed by this example is defined using a traditional COBOL record layout:

```cobol
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
Because the layout contains COMP-3 (packed decimal) fields, the resulting file (clientes.dat) is a binary, fixed-length sequential file, not plain text.

This structure is typical of many real-world COBOL batch systems.

2. Generating the data in COBOL
A small COBOL program (Creararchivo.cbl) writes records using this layout and produces clientes.dat.

From the COBOL side, this is a standard batch flow:

OPEN OUTPUT

MOVE values into the record

WRITE CLIENTE-REC

CLOSE

No special handling is required on the COBOL side.

3. Interpreting the same layout in Go
In the Go program, the same record layout is embedded as a string and parsed at runtime:

go
Copiar código
const copyInline = `
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
`
At startup, the Go runtime:

parses this layout

computes field offsets and sizes

validates the total record length (LRECL)

determines how each field must be decoded (X(n) vs COMP-3)

This mirrors the role of a COBOL copybook as a strict data contract.

4. Writing COBOL-style logic inside Go
Instead of rewriting the logic directly in idiomatic Go, the program executes a minimal COBOL-like script embedded in Go:

go
Copiar código
const script = `
OPEN INPUT CLIENTES-IN FROM "clientes.dat".
PERFORM UNTIL EOF CLIENTES-IN
    READ CLIENTES-IN INTO CLIENTE-REC.
    IF NOT EOF CLIENTES-IN
        DISPLAY "TIPO=".
        DISPLAY FIELD CLIENTE-REC CR-TIPO-REG.
        DISPLAY "NOMBRE=".
        DISPLAY FIELD CLIENTE-REC CR-NOMBRE.
        DISPLAY "SALARIO=".
        DISPLAY FIELD CLIENTE-REC CR-SALARIO.
    END-IF.
END-PERFORM.
CLOSE CLIENTES-IN.
DISPLAY "FIN".
`
This shows how COBOL batch logic can be expressed almost verbatim:

sequential file handling

explicit READ

PERFORM UNTIL EOF

conditional processing

simple reporting via DISPLAY

For a COBOL developer, the execution model is immediately recognizable.

5. COMP-3 decoding and output
When a record is read:

packed decimal fields are decoded nibble by nibble

the sign nibble is interpreted

implied decimals (V9(n)) are applied logically

The resulting output is:

text
Copiar código
TIPO=
D
NOMBRE=
JUAN PEREZ
SALARIO=
1234567.89
FIN

Build and run
Generate the data file (COBOL)

bash
Copiar código
cobc -x Creararchivo.cbl
./Creararchivo

Build and run the Go program
bash

Copiar código
go build -o cobol2GO cobol2GO.go
./cobol2GO

Purpose and scope
This project is intentionally limited in scope.

It is not:

a full COBOL compiler
a production-ready runtime
a replacement for existing COBOL systems

Its purpose is to illustrate how COBOL data formats and batch-processing concepts can be decoded, reasoned about, and executed using Go, providing a concrete foundation for modernization discussions.

License
MIT License

Copyright (c) 2025 Diego Fraiese
