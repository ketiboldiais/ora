import { Option, option } from "./fp_utils";

// Utility Functions
export function isUnsafe(x: unknown): x is undefined | null {
  return x === undefined || x === null;
}

enum EXPR {
  INTEGER,
  BIG_INTEGER,
  FLOAT,
  BIG_FLOAT,
  FRACTION,
  BIG_FRACTION,
  COMPLEX,
  NIL,
}

/** An object corresponding to an Expression. */
abstract class Expression {
  abstract kind(): EXPR;
}

/** An object corresponding to an Integer. */
class Integer extends Expression {
  kind(): EXPR {
    return EXPR.INTEGER;
  }
  /** The value of this integer. */
  _value: number;
  constructor(value: number) {
    super();
    this._value = Math.floor(value);
  }
}

/** Returns a new Integer. */
export function int(value: number) {
  return new Integer(value);
}

/**
 * Returns true, and type-asserts, only if
 * the given expression `expr` is
 * an Integer.
 */
export function isInt(expr: Expression): expr is Integer {
  return !isUnsafe(expr) && expr.kind() === EXPR.INTEGER;
}

/** An object corresponding to a Big Integer. */
export class BigInteger extends Expression {
  kind(): EXPR {
    return EXPR.BIG_INTEGER;
  }
  /** The value of this big integer. */
  _value: bigint;
  constructor(value: bigint) {
    super();
    this._value = value;
  }
}

/** Returns a new big integer. */
export function bigint(value: bigint) {
  return new BigInteger(value);
}

/**
 * Returns true, and type-asserts, only if the
 * given Expression `expr` is a BigInteger.
 */
export function isBigInt(expr: Expression): expr is BigInteger {
  return !isUnsafe(expr) && expr.kind() === EXPR.BIG_INTEGER;
}

/** An object corresponding to a floating point number. */
export class Float extends Expression {
  kind(): EXPR {
    return EXPR.FLOAT;
  }
  /** The value of this real number. */
  _value: number;
  constructor(value: number) {
    super();
    this._value = value;
  }
}

/** Returns a new Real. */
export function float(value: number) {
  return new Float(value);
}

/** Returns a new Big Real number. */
export class BigFloat extends Expression {
  kind(): EXPR {
    return EXPR.BIG_FLOAT;
  }
  /** This big real's significand. */
  _m: number | string;
  /** This big real's exponent. This must be an integer. */
  _n: number = 0;
  constructor(m: number | string, n: number) {
    super();
    this._m = m;
    this._n = Math.floor(n);
  }
}

/** Returns a new big real number. */
export function bigfloat(m: number | string, n: number = 0) {
  return new BigFloat(m, n);
}

/** An object corresponding to a Fraction. */
export class Fraction extends Expression {
  kind(): EXPR {
    return EXPR.FRACTION;
  }
  /** This fraction's numerator. */
  _n: Integer;
  /** This fraction's denominator. */
  _d: Integer;
  constructor(n: Integer, d: Integer) {
    super();
    this._n = n;
    this._d = d;
  }
}

/** Returns a new Fraction. */
export function frac(
  numerator: number | Integer,
  denominator: number | Integer
) {
  return new Fraction(
    typeof numerator === "number" ? int(numerator) : numerator,
    typeof denominator === "number" ? int(denominator) : denominator
  );
}

/**
 * Represents a fraction--that is,
 * a number of the form `a/b`, where `a` and `b`
 * are bigints.
 */
export class BigFraction extends Expression {
  kind(): EXPR {
    return EXPR.BIG_FRACTION;
  }
  _n: bigint;
  _d: bigint;
  constructor(n: bigint, d: bigint) {
    super();
    this._n = n;
    this._d = d;
  }
}

/** Returns a new BigFraction. */
export function bigfrac(n: bigint, d: bigint) {
  return new BigFraction(n, d);
}

/**
 * A union of the numeric types Integer, BigInteger, Float,
 * BigFloat, Fraction, and BigFraction.
 */
type Real = Integer | BigInteger | Float | BigFloat | Fraction | BigFraction;

/**
 * An object corresponding to a Complex number.
 */
export class Complex extends Expression {
  kind(): EXPR {
    return EXPR.COMPLEX;
  }
  _re: Real;
  _im: Real;
  constructor(re: Real, im: Real) {
    super();
    this._re = re;
    this._im = im;
  }
}

/**
 * Returns a new Complex number.
 */
export function complex(re: Real, im: Real) {
  return new Complex(re, im);
}

export class Nil extends Expression {
  kind(): EXPR {
    return EXPR.NIL;
  }
  constructor() {
    super();
  }
}

export function nil() {
  return new Nil();
}

/**
 * Represents an error generally.
 */
abstract class ERROR extends Error {
  constructor(message: string) {
    super(message);
  }
}

enum TOKEN_TYPE {
  // Utility tokens
  /** A token corresponding to end of input. */
  END,
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
  // Oerator 
}

/**
 * Represents a token.
 */
export class Token {
  /** This token's lexeme. */
  _lexeme: string;
  /** The line where this token was found. */
  _line: number;
  /** The literal corresponding to this token's lexeme. */
  _literal: Expression | null = null;

  /** Sets this token's lexeme. */
  lexeme(lexeme: string) {
    this._lexeme = lexeme;
    return this;
  }

  /** Sets the line where this token was found. */
  line(line: number) {
    this._line = line;
    return this;
  }

  literal(literal: Expression) {
    this._literal = literal;
    return this;
  }
  constructor(lexeme: string, line: number) {
    this._lexeme = lexeme;
    this._line = line;
  }
}

/**
 * Represents a syntax error (an error
 * that occurs during syntax analysis,
 * i.e., parsing).
 */
export class SYNTAX_ERROR extends ERROR {
  _line: number;
  constructor(message: string, line: number) {
    super(`Syntax error on line ${line}: "${message}"`);
    this._line = line;
  }
}

/**
 * Returns a new syntax error.
 * @param message - The message accompanying this error.
 * @param line - The line this syntax error occurred on.
 */
export function syntaxError(message: string, line: number) {
  return new SYNTAX_ERROR(message, line);
}

/**
 * Represents a lexical error (an error
 * that occurs during lexical analysis,
 * i.e., scanning).
 */
export class LEXICAL_ERROR extends ERROR {
  _line: number;
  constructor(message: string, line: number) {
    super(`Lexical error on line ${line}: "${message}"`);
    this._line = line;
  }
}

function lexicalAnalyzer(code: string) {
  /**
   * A variable corresponding to the current
   * line the scanner's on.
   */
  let _line: number = 1;
  /**
   * A pointer to the first character of the lexeme
   * currently being scanned.
   */
  let _start: number = 0;
  /**
   * A pointer to the character currently
   * being scanned.
   */
  let _current: number = 0;
  /**
   * Error indicator defaulting to null.
   * If initialized, then the scanning
   * halts.
   */
  let _error: ERROR | null = null;

  /**
   * Returns true if the scanner has reached the end
   * of the source.
   */
  const atEnd = (): boolean => _current >= code.length || _error !== null;

  /**
   * Consumes and returns the next character in the
   * source.
   */
  const tick = (): Option<string> => option(code[_current++]);

  /**
   * Returns the source substring _start to _current.
   */
  const slice = (): string => code.slice(_start, _current);

  
}
