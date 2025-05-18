export enum TOKEN_TYPE {
  // Utility tokens
  /** A token corresponding to end of input. */
  EOF,
  /** A token corresponding to an error. */
  ERROR,
  /** A token corresponding to nothingness. */
  EMPTY,

  // Paired delimiters.
  /** A token corresponding to `(`. */
  LEFT_PAREN,
  /** A token corresponding to `)`. */
  RIGHT_PAREN,
  /** A token corresponding to `[` */
  LEFT_BRACKET,
  /** A token corresponding to `]` */
  RIGHT_BRACKET,
  /** A token corresponding to `{` */
  LEFT_BRACE,
  /** A token corresponding to `}` */
  RIGHT_BRACE,

  // Single-character delimiters
  /** A token corresponding to `;` */
  SEMICOLON,
  /** A token corresponding to `:` */
  COLON,
  /** A token corresponding to `.` */
  DOT,
  /** A token corresponding to `COMMA` */
  COMMA,

  // Operator delimiters 
  /** A token corresponding to `+` */
  PLUS,
  /** A token corresponding to `-` */
  MINUS,
  /** A token corresponding to `*` */
  STAR,
  /** A token corresponding to `/` */
  SLASH,
  /** A token corresponding to `^` */
  CARET,
  /** A token corresponding to `%` */
  PERCENT,
  /** A token corresponding to `!` */
  BANG,
  /** A token corresponding to `&` */
  AMPERSAND,
  /** A token corresponding to `~` */
  TILDE,
  /** A token corresponding to `|` */
  VBAR,
  /** A token corresponding to `=` */
  EQUAL,
  /** A token corresponding to `<` */
  LESS,
  /** A token corresponding to `>` */
  GREATER,
  /** A token corresponding to `<=` */
  LESS_EQUAL,
  /** A token corresponding to `>=` */
  GREATER_EQUAL,
  /** A token corresponding to `!=` */
  BANG_EQUAL,
  /** A token corresponding to `==` */
  EQUAL_EQUAL,
  /** A token corresponding to `++` */
  PLUS_PLUS,
  /** A token corresponding to `--` */
  MINUS_MINUS,
  /** A token corresponding to `**` */
  STAR_STAR,
  
  // Vector Operators
  /** A token corresponding to `.+` */
  DOT_ADD,
  /** A token corresponding to `.*` */
  DOT_STAR,
  /** A token corresponding to `.-` */
  DOT_MINUS,
  /** A token corresponding to `.^` */
  DOT_CARET,
  /** A token corresponding to `@` */
  AT,

  // Matrix Operators
  /** A token corresponding to `#+` */
  POUND_PLUS,
  /** A token corresponding to `#-` */
  POUND_MINUS,
  /** A token corresponding to `#*` */
  POUND_STAR,

  // Literals
  /** A token corresponding to an integer */
  INTEGER,
  /** A token corresponding to a big integer */
  BIG_INTEGER,
  /** A token corresponding to an floating point number. */
  FLOAT,
  /** A token corresponding to a fraction. */
  FRACTION,
  /**
   * A token corresponding to a big floating point number
   * (a number written in scientific notation).
   */
  BIG_FLOAT,
  /** A token corresponding to a symbol. */
  SYMBOL,
  /** A token corresponding to a string. */
  STRING,
  /** A token corresponding to a Boolean literal. */
  BOOLEAN,
  /** A token corresponding to NaN (Not a Number) */
  NAN,
  /** A token corresponding to Inf (Infinity). */
  INF,
  /** A token corresponding to nil. */
  NIL,

  // Keyword Tokens
  AND,
  OR,
  NOT,
  NAND,
  XOR,
  XNOR,
  NOR,
  IF,
  ELSE,
  FN,
  LET,
  VAR,
  RETURN,
  WHILE,
  FOR,
  CLASS,
  PRINT,
  SUPER,
  THIS,
  REM,
  MOD,
  DIV,

  LIST,

  NUMERIC_CONSTANT,
  NATIVE_FUNCTION,
}