# COBOL2GO

An example demonstrating how to convert COBOL to Go.

## Overview

**COBOL2GO** is a small, educational project that shows how legacy COBOL data and control structures can be interpreted and processed using modern Go code.

The project focuses on:
- Reading a binary COBOL sequential file
- Parsing a COBOL copybook layout
- Decoding packed decimal fields (COMP-3)
- Executing a minimal COBOL-like control flow (`OPEN`, `READ`, `PERFORM UNTIL EOF`, `IF`, `DISPLAY`) implemented in Go

This is **not a full COBOL compiler or runtime**.  
It is a **conceptual and practical example** intended to illustrate how COBOL concepts map to Go.

---

## What this project demonstrates

- How a COBOL record layout can be expressed and interpreted in Go
- How COMP-3 (packed decimal) fields work at byte and nibble level
- How sequential file processing in COBOL maps to iterative logic in Go
- How legacy batch-style logic can be reasoned about outside the mainframe

---

## Project structure


