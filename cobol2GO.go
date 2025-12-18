package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"regexp"
	"strconv"
	"strings"
)

//
// ========================
// Minimal COBOL-like runtime in Go
// ========================
// Supports statements (one per line):
//   OPEN INPUT  <LOGICAL> FROM "path".
//   CLOSE <LOGICAL>.
//   PERFORM UNTIL EOF <LOGICAL>
//       READ <LOGICAL> INTO <RECNAME>.
//       IF NOT EOF <LOGICAL>
//           DISPLAY "text".
//           DISPLAY FIELD <RECNAME> <FIELDNAME>.
//       END-IF.
//   END-PERFORM.
//   DISPLAY "text".
//

type Usage int

const (
	UsageDisplay Usage = iota
	UsageComp3
)

type PicSpec struct {
	AlphaLen   int
	DigitsInt  int
	DigitsFrac int
	Signed     bool
}

type FieldSpec struct {
	Name   string
	Pic    PicSpec
	Usage  Usage
	Offset int
	Size   int
}

type cobFile struct {
	path string
	mode string // INPUT / OUTPUT
	f    *os.File
	eof  bool
}

type Cobol struct {
	LRECL   int
	Fields  []FieldSpec
	files   map[string]*cobFile
	records map[string][]byte
}

var reField = regexp.MustCompile(`(?i)^\s*\d+\s+([A-Z0-9\-]+)\s+PIC\s+(.+?)\s*\.\s*$`)

func NewCobol(copyInline string, lrecl int) (*Cobol, error) {
	fields, computed, err := parseCopybook(copyInline)
	if err != nil {
		return nil, err
	}
	if computed != lrecl {
		return nil, fmt.Errorf("layout size mismatch: computed=%d lrecl=%d", computed, lrecl)
	}
	return &Cobol{
		LRECL:   lrecl,
		Fields:  fields,
		files:   map[string]*cobFile{},
		records: map[string][]byte{},
	}, nil
}

//
// ========================
// Interpreter
// ========================
//

type frameKind int

const (
	framePerform frameKind = iota
	frameIf
)

type frame struct {
	kind     frameKind
	startPC  int
	endPC    int
	logical  string // for EOF condition
	ifNotEOF bool
}

func (c *Cobol) Run(script string) error {
	lines := normalizeLines(script)
	endMap, err := buildEndMap(lines)
	if err != nil {
		return err
	}

	var st []frame
	pc := 0

	for pc < len(lines) {
		ln := strings.TrimSpace(lines[pc])
		if ln == "" || strings.HasPrefix(ln, "*") || strings.HasPrefix(ln, "*>") {
			pc++
			continue
		}

		toks := tokenize(ln)
		if len(toks) == 0 {
			pc++
			continue
		}

		switch strings.ToUpper(toks[0]) {

		case "OPEN":
			// OPEN INPUT <LOGICAL> FROM "path"
			if len(toks) != 5 || !strings.EqualFold(toks[3], "FROM") {
				return fmt.Errorf("OPEN syntax: OPEN INPUT <logical> FROM \"path\" (line %d)", pc+1)
			}
			mode := strings.ToUpper(toks[1])
			logical := strings.ToUpper(toks[2])
			path := unquote(toks[4])
			if err := c.open(mode, logical, path); err != nil {
				return fmt.Errorf("OPEN error (line %d): %w", pc+1, err)
			}
			pc++

		case "CLOSE":
			if len(toks) != 2 {
				return fmt.Errorf("CLOSE syntax: CLOSE <logical> (line %d)", pc+1)
			}
			logical := strings.ToUpper(toks[1])
			if err := c.close(logical); err != nil {
				return fmt.Errorf("CLOSE error (line %d): %w", pc+1, err)
			}
			pc++

		case "PERFORM":
			// PERFORM UNTIL EOF <LOGICAL>
			if len(toks) != 4 || !strings.EqualFold(toks[1], "UNTIL") || !strings.EqualFold(toks[2], "EOF") {
				return fmt.Errorf("PERFORM syntax: PERFORM UNTIL EOF <logical> (line %d)", pc+1)
			}
			logical := strings.ToUpper(toks[3])
			endPC, ok := endMap[pc]
			if !ok {
				return fmt.Errorf("PERFORM without matching END-PERFORM (line %d)", pc+1)
			}
			f := c.files[logical]
			if f == nil || f.f == nil {
				return fmt.Errorf("PERFORM: file not opened: %s (line %d)", logical, pc+1)
			}
			if f.eof {
				pc = endPC + 1
				continue
			}
			st = append(st, frame{
				kind:    framePerform,
				startPC: pc + 1,
				endPC:   endPC,
				logical: logical,
			})
			pc++

		case "END-PERFORM":
			if len(st) == 0 || st[len(st)-1].kind != framePerform {
				return fmt.Errorf("END-PERFORM without PERFORM (line %d)", pc+1)
			}
			top := st[len(st)-1]
			f := c.files[top.logical]
			if f == nil || f.f == nil {
				return fmt.Errorf("END-PERFORM: file not opened: %s (line %d)", top.logical, pc+1)
			}
			if f.eof {
				st = st[:len(st)-1]
				pc++
			} else {
				pc = top.startPC
			}

		case "IF":
			// IF NOT EOF <LOGICAL>
			// IF EOF <LOGICAL>
			if len(toks) < 4 {
				return fmt.Errorf("IF syntax: IF [NOT] EOF <logical> (line %d)", pc+1)
			}
			i := 1
			not := false
			if strings.EqualFold(toks[1], "NOT") {
				not = true
				i = 2
			}
			if !strings.EqualFold(toks[i], "EOF") || len(toks) != i+2 {
				return fmt.Errorf("IF syntax: IF [NOT] EOF <logical> (line %d)", pc+1)
			}
			logical := strings.ToUpper(toks[i+1])

			endPC, ok := endMap[pc]
			if !ok {
				return fmt.Errorf("IF without matching END-IF (line %d)", pc+1)
			}

			f := c.files[logical]
			if f == nil || f.f == nil {
				return fmt.Errorf("IF: file not opened: %s (line %d)", logical, pc+1)
			}

			cond := f.eof
			if not {
				cond = !cond
			}

			if !cond {
				pc = endPC + 1
				continue
			}

			st = append(st, frame{
				kind:     frameIf,
				startPC:  pc + 1,
				endPC:    endPC,
				logical:  logical,
				ifNotEOF: not,
			})
			pc++

		case "END-IF":
			if len(st) == 0 || st[len(st)-1].kind != frameIf {
				return fmt.Errorf("END-IF without IF (line %d)", pc+1)
			}
			st = st[:len(st)-1]
			pc++

		case "READ":
			// READ <LOGICAL> INTO <RECNAME>
			if len(toks) != 4 || !strings.EqualFold(toks[2], "INTO") {
				return fmt.Errorf("READ syntax: READ <logical> INTO <recname> (line %d)", pc+1)
			}
			logical := strings.ToUpper(toks[1])
			rec := strings.ToUpper(toks[3])
			if err := c.read(logical, rec); err != nil {
				return fmt.Errorf("READ error (line %d): %w", pc+1, err)
			}
			pc++

		case "DISPLAY":
			// DISPLAY "text"
			// DISPLAY FIELD <REC> <FIELD>
			if len(toks) == 2 && isQuoted(toks[1]) {
				fmt.Println(unquote(toks[1]))
				pc++
				continue
			}
			if len(toks) == 4 && strings.EqualFold(toks[1], "FIELD") {
				rec := strings.ToUpper(toks[2])
				field := strings.ToUpper(toks[3])
				s, err := c.displayField(rec, field)
				if err != nil {
					return fmt.Errorf("DISPLAY error (line %d): %w", pc+1, err)
				}
				fmt.Println(s)
				pc++
				continue
			}
			return fmt.Errorf("DISPLAY syntax: DISPLAY \"text\" | DISPLAY FIELD <rec> <field> (line %d)", pc+1)

		default:
			return fmt.Errorf("unknown statement %q (line %d)", toks[0], pc+1)
		}
	}

	return nil
}

//
// ========================
// File ops
// ========================
//

func (c *Cobol) open(mode, logical, path string) error {
	switch mode {
	case "INPUT":
		f, err := os.Open(path)
		if err != nil {
			return err
		}
		c.files[logical] = &cobFile{path: path, mode: "INPUT", f: f, eof: false}
		return nil
	default:
		return fmt.Errorf("unsupported OPEN mode: %s (only INPUT implemented)", mode)
	}
}

func (c *Cobol) close(logical string) error {
	f := c.files[logical]
	if f == nil || f.f == nil {
		return fmt.Errorf("file not opened: %s", logical)
	}
	err := f.f.Close()
	f.f = nil
	return err
}

func (c *Cobol) read(logical, rec string) error {
	f := c.files[logical]
	if f == nil || f.f == nil {
		return fmt.Errorf("file not opened: %s", logical)
	}
	if f.mode != "INPUT" {
		return fmt.Errorf("READ requires INPUT file")
	}

	buf := make([]byte, c.LRECL)
	_, err := io.ReadFull(f.f, buf)
	if err != nil {
		if errors.Is(err, io.EOF) || errors.Is(err, io.ErrUnexpectedEOF) {
			f.eof = true
			return nil
		}
		return err
	}
	c.records[rec] = buf
	return nil
}

func (c *Cobol) displayField(recName, fieldName string) (string, error) {
	rec := c.records[recName]
	if len(rec) != c.LRECL {
		return "", fmt.Errorf("record not loaded: %s", recName)
	}
	fs, ok := c.findField(fieldName)
	if !ok {
		return "", fmt.Errorf("unknown field: %s", fieldName)
	}
	raw := rec[fs.Offset : fs.Offset+fs.Size]

	switch fs.Usage {
	case UsageDisplay:
		return strings.TrimRight(string(raw), " "), nil
	case UsageComp3:
		totalDigits := fs.Pic.DigitsInt + fs.Pic.DigitsFrac
		return decodeCOMP3(raw, totalDigits, fs.Pic.DigitsFrac, fs.Pic.Signed)
	default:
		return "", fmt.Errorf("unsupported usage")
	}
}

func (c *Cobol) findField(name string) (FieldSpec, bool) {
	n := strings.ToUpper(name)
	for _, f := range c.Fields {
		if f.Name == n {
			return f, true
		}
	}
	return FieldSpec{}, false
}

//
// ========================
// Copybook parsing
// ========================
//

func parseCopybook(copy string) ([]FieldSpec, int, error) {
	var fields []FieldSpec
	offset := 0

	sc := bufio.NewScanner(strings.NewReader(copy))
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" || strings.HasPrefix(line, "*") || strings.HasPrefix(line, "*>") {
			continue
		}

		m := reField.FindStringSubmatch(line)
		if m == nil {
			continue
		}

		name := strings.ToUpper(strings.TrimSpace(m[1]))
		right := strings.ToUpper(strings.TrimSpace(m[2]))

		usage := UsageDisplay
		if strings.Contains(right, "COMP-3") {
			usage = UsageComp3
			right = strings.ReplaceAll(right, "COMP-3", "")
		}
		right = strings.TrimSpace(right)

		pic, err := parsePIC(right)
		if err != nil {
			return nil, 0, fmt.Errorf("PIC for %s: %w", name, err)
		}

		sz, err := computeSize(pic, usage)
		if err != nil {
			return nil, 0, fmt.Errorf("size for %s: %w", name, err)
		}

		fields = append(fields, FieldSpec{
			Name:   name,
			Pic:    pic,
			Usage:  usage,
			Offset: offset,
			Size:   sz,
		})
		offset += sz
	}
	if err := sc.Err(); err != nil {
		return nil, 0, err
	}
	return fields, offset, nil
}

func parsePIC(picText string) (PicSpec, error) {
	t := strings.ToUpper(strings.ReplaceAll(picText, " ", ""))
	spec := PicSpec{}

	// Signed
	if strings.HasPrefix(t, "S") {
		spec.Signed = true
		t = strings.TrimPrefix(t, "S")
	}

	// Alphanumeric: X(n)
	if strings.HasPrefix(t, "X(") && strings.HasSuffix(t, ")") {
		nStr := strings.TrimSuffix(strings.TrimPrefix(t, "X("), ")")
		n, err := strconv.Atoi(nStr)
		if err != nil || n < 0 {
			return PicSpec{}, fmt.Errorf("invalid X(n): %s", picText)
		}
		spec.AlphaLen = n
		return spec, nil
	}

	// Numeric: 9(n) + optional V9(m)
	// Examples: 9(10), 9(5), 9(8), 9(9)V9(2)
	if !strings.HasPrefix(t, "9(") {
		return PicSpec{}, fmt.Errorf("unsupported PIC: %s", picText)
	}

	closeIdx := strings.Index(t, ")")
	if closeIdx < 0 {
		return PicSpec{}, fmt.Errorf("invalid 9(n): %s", picText)
	}
	nStr := t[2:closeIdx]
	n, err := strconv.Atoi(nStr)
	if err != nil || n < 0 {
		return PicSpec{}, fmt.Errorf("invalid 9(n): %s", picText)
	}
	spec.DigitsInt = n
	t = t[closeIdx+1:]

	// Optional V9(m)
	if strings.HasPrefix(t, "V9(") {
		closeIdx = strings.Index(t, ")")
		if closeIdx < 0 {
			return PicSpec{}, fmt.Errorf("invalid V9(m): %s", picText)
		}
		mStr := t[3:closeIdx]
		m, err := strconv.Atoi(mStr)
		if err != nil || m < 0 {
			return PicSpec{}, fmt.Errorf("invalid V9(m): %s", picText)
		}
		spec.DigitsFrac = m
	}

	return spec, nil
}

func computeSize(pic PicSpec, usage Usage) (int, error) {
	if pic.AlphaLen > 0 {
		return pic.AlphaLen, nil
	}
	totalDigits := pic.DigitsInt + pic.DigitsFrac
	if totalDigits <= 0 {
		return 0, errors.New("numeric PIC must have digits")
	}

	switch usage {
	case UsageDisplay:
		// not used in your layout for numerics
		return totalDigits, nil

	case UsageComp3:
		// Correct packed decimal size:
		// bytes = ceil((digits + 1 sign nibble) / 2) = ceil((digits+1)/2)
		// integer form: (digits + 2) / 2
		return (totalDigits + 2) / 2, nil

	default:
		return 0, fmt.Errorf("unknown usage")
	}
}

//
// ========================
// COMP-3 decoding
// ========================
//

func decodeCOMP3(b []byte, totalDigits, fracDigits int, signed bool) (string, error) {
	if len(b) == 0 {
		return "", errors.New("empty COMP-3")
	}

	// Expand into nibbles
	var nibbles []byte
	for _, by := range b {
		nibbles = append(nibbles, (by>>4)&0x0F, by&0x0F)
	}

	// last nibble is sign
	signNib := nibbles[len(nibbles)-1]
	digitNibs := nibbles[:len(nibbles)-1]

	neg := false
	if signed {
		switch signNib {
		case 0x0D, 0x0B:
			neg = true
		case 0x0C, 0x0A, 0x0E, 0x0F:
			neg = false
		default:
			return "", fmt.Errorf("invalid COMP-3 sign nibble: 0x%X", signNib)
		}
	}

	// Build digits
	var digits bytes.Buffer
	for _, d := range digitNibs {
		if d > 9 {
			return "", fmt.Errorf("invalid COMP-3 digit nibble: 0x%X", d)
		}
		digits.WriteByte('0' + d)
	}
	s := digits.String()

	// enforce totalDigits length (keep rightmost)
	if len(s) > totalDigits {
		s = s[len(s)-totalDigits:]
	} else if len(s) < totalDigits {
		s = strings.Repeat("0", totalDigits-len(s)) + s
	}

	// decimal point
	if fracDigits > 0 {
		if fracDigits >= len(s) {
			s = "0." + strings.Repeat("0", fracDigits-len(s)) + s
		} else {
			s = s[:len(s)-fracDigits] + "." + s[len(s)-fracDigits:]
		}
	}

	// trim leading zeros (keep one)
	if fracDigits == 0 {
		s = strings.TrimLeft(s, "0")
		if s == "" {
			s = "0"
		}
	} else {
		parts := strings.SplitN(s, ".", 2)
		intPart := strings.TrimLeft(parts[0], "0")
		if intPart == "" {
			intPart = "0"
		}
		s = intPart + "." + parts[1]
	}

	if neg && s != "0" && s != "0.0" {
		s = "-" + s
	}
	return s, nil
}

//
// ========================
// Script helpers
// ========================
//

func normalizeLines(s string) []string {
	s = strings.ReplaceAll(s, "\r\n", "\n")
	s = strings.ReplaceAll(s, "\r", "\n")
	raw := strings.Split(s, "\n")
	out := make([]string, 0, len(raw))
	for _, ln := range raw {
		out = append(out, strings.TrimRight(ln, " \t"))
	}
	return out
}

func buildEndMap(lines []string) (map[int]int, error) {
	type stk struct {
		kind string
		pc   int
	}
	var st []stk
	endMap := map[int]int{}

	for i := 0; i < len(lines); i++ {
		ln := strings.TrimSpace(lines[i])
		if ln == "" || strings.HasPrefix(ln, "*") || strings.HasPrefix(ln, "*>") {
			continue
		}
		toks := tokenize(ln)
		if len(toks) == 0 {
			continue
		}
		switch strings.ToUpper(toks[0]) {
		case "PERFORM":
			st = append(st, stk{kind: "PERFORM", pc: i})
		case "IF":
			st = append(st, stk{kind: "IF", pc: i})
		case "END-PERFORM":
			if len(st) == 0 || st[len(st)-1].kind != "PERFORM" {
				return nil, fmt.Errorf("END-PERFORM without PERFORM (line %d)", i+1)
			}
			top := st[len(st)-1]
			st = st[:len(st)-1]
			endMap[top.pc] = i
		case "END-IF":
			if len(st) == 0 || st[len(st)-1].kind != "IF" {
				return nil, fmt.Errorf("END-IF without IF (line %d)", i+1)
			}
			top := st[len(st)-1]
			st = st[:len(st)-1]
			endMap[top.pc] = i
		}
	}
	if len(st) != 0 {
		top := st[len(st)-1]
		return nil, fmt.Errorf("unclosed block %s starting at line %d", top.kind, top.pc+1)
	}
	return endMap, nil
}

func tokenize(line string) []string {
	var toks []string
	var cur strings.Builder
	inQuote := false

	for i := 0; i < len(line); i++ {
		ch := line[i]
		if ch == '"' {
			inQuote = !inQuote
			cur.WriteByte(ch)
			continue
		}
		if !inQuote && (ch == ' ' || ch == '\t') {
			if cur.Len() > 0 {
				toks = append(toks, cur.String())
				cur.Reset()
			}
			continue
		}
		cur.WriteByte(ch)
	}
	if cur.Len() > 0 {
		toks = append(toks, cur.String())
	}

	// Strip trailing '.' from tokens
	for i := range toks {
		toks[i] = strings.TrimSuffix(toks[i], ".")
	}
	return toks
}

func isQuoted(s string) bool { return len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' }
func unquote(s string) string { return strings.TrimSuffix(strings.TrimPrefix(s, `"`), `"` ) }

//
// ========================
// Demo: your layout + read clients.dat
// ========================
//

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

func main() {
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

	// Correct LRECL for this layout = 80
	c, err := NewCobol(copyInline, 80)
	if err != nil {
		fmt.Fprintf(os.Stderr, "init error: %v\n", err)
		os.Exit(1)
	}
	if err := c.Run(script); err != nil {
		fmt.Fprintf(os.Stderr, "runtime error: %v\n", err)
		os.Exit(1)
	}
}
