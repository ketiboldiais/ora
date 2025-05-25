import { left, right } from "./utils_fp";
import {
  isDigit,
  isHexDigit,
  isOctalDigit,
  isValidNameChar,
} from "./utils_parsing";

// Utility Functions
export function isUnsafe(x: unknown): x is undefined | null {
  return x === undefined || x === null;
}

export enum EXPR {
  INTEGER,
  BIG_INTEGER,
  FLOAT,
  BIG_FLOAT,
  FRACTION,
  BIG_FRACTION,
  COMPLEX,
  NIL,
  SYM,
  BOOL,
  NUMERIC_CONSTANT,
  STRING,
  INF,
  NAN,
  UNDEFINED,
  RELATION,
  EQUATION,
  SUM,
  DIFFERENCE,
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

type Numeric = Real | NAN | Inf;

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

/**
 * An object representing a symbol.
 */
export class Sym extends Expression {
  _sym: string;
  kind() {
    return EXPR.SYM;
  }
  constructor(sym: string) {
    super();
    this._sym = sym;
  }
}

/**
 * Returns a new Sym expression.
 */
export function sym(symbol: string) {
  return new Sym(symbol);
}

/**
 * An object representing a numeric constant.
 */
export class NumConst extends Expression {
  _sym: string;
  _value: number;
  kind(): EXPR {
    return EXPR.NUMERIC_CONSTANT;
  }
  constructor(sym: string, value: number) {
    super();
    this._sym = sym;
    this._value = value;
  }
}

/**
 * Returns a new numeric constant.
 */
export function numConst(symbol: string, value: number) {
  return new NumConst(symbol, value);
}

/**
 * An object representing Infinity.
 */
export class Inf extends Expression {
  kind(): EXPR {
    return EXPR.INF;
  }
  _sign: "+" | "-";
  constructor(sign: "+" | "-") {
    super();
    this._sign = sign;
  }
}

/** Returns a new Inf (Infinity) object. */
export function inf(sign: "+" | "-") {
  return new Inf(sign);
}

/** An object corresponding to nan (not a number). */
export class NAN extends Expression {
  kind(): EXPR {
    return EXPR.NAN;
  }
  _value: number = NaN;
  constructor() {
    super();
  }
}

export function nan() {
  return new NAN();
}

/** An object representing the global symbol "undefined". */
export class Undefined extends Expression {
  kind(): EXPR {
    return EXPR.UNDEFINED;
  }
  _value = "undefined" as const;
  constructor() {
    super();
  }
}

export const UNDEFINED = () => (
  new Undefined()
)

/*
 * An object representing a Boolean value.
 */
export class Bool extends Expression {
  _value: boolean;
  kind(): EXPR {
    return EXPR.BOOL;
  }
  constructor(value: boolean) {
    super();
    this._value = value;
  }
}

/**
 * Returns a new Bool.
 */
export function bool(value: boolean) {
  return new Bool(value);
}

/**
 * An object representing a string.
 */
export class Str extends Expression {
  kind(): EXPR {
    return EXPR.STRING;
  }
  _string: string;
  _algebraic: boolean = false;
  constructor(str: string) {
    super();
    this._string = str;
  }
  algebraic(value: boolean = true) {
    this._algebraic = value;
    return this;
  }
}

export function str(string: string) {
  return new Str(string);
}

/**
 * An object representing the value nil.
 */
export class Nil extends Expression {
  kind(): EXPR {
    return EXPR.NIL;
  }
  constructor() {
    super();
  }
}

/**
 * Returns a new nil.
 */
export function nil() {
  return new Nil();
}

/**
 * The operators recognized as relation operators
 * in symbolic strings. Note that the `=` sign in
 * a symbolic string is seen as a relation operator
 * rather than assignment, since assignment is handled
 * by the Praxis interpreter.
 */
type RelationOperator = "=" | "<" | ">" | "<=" | ">=" | "!=";

/** An object representing a relation. */
export class Relation extends Expression {
  kind(): EXPR {
    return EXPR.RELATION;
  }
  _op: RelationOperator;
  _args: Expression[];
  constructor(op: RelationOperator, args: Expression[]) {
    super();
    this._op = op;
    this._args = args;
  }
}

/** Returns a new Relation expression. */
export function relate(op: RelationOperator, args: Expression[]) {
  return new Relation(op, args)
}

/**
 * An object representing an equation.
 */
export class Equation extends Expression {
  kind(): EXPR {
    return EXPR.EQUATION;
  }
  _args: [Expression, Expression];
  constructor(left: Expression, right: Expression) {
    super();
    this._args = [left, right];
  }
}

/**
 * Returns a new Equation object.
 */
export function equate(left: Expression, right: Expression) {
  return new Equation(left, right);
}

/**
 * An object representing a sum.
 */
export class Sum extends Expression {
  kind(): EXPR {
    return EXPR.SUM;
  }
  _args: Expression[];
  constructor(args: Expression[]) {
    super();
    this._args = args;
  }
}

/**
 * Returns a new Sum expression.
 */
export function sum(...args: Expression[]) {
  return new Sum(args);
}

/**
 * An object representing a difference.
 */
export class Difference extends Expression {
  kind(): EXPR {
    return EXPR.DIFFERENCE;
  }
  _args: EXPR[];
  constructor(args: EXPR[]) {
    super();
    this._args = args;
  }
}

/**
 * Returns a new Difference expression.
 */
export function diff(...args: EXPR[]) {
  return new Difference(args);
}


/**
 * Represents an error generally.
 */
abstract class ERROR extends Error {
  constructor(message: string) {
    super(message);
  }
}

export enum TOKEN {
  // Utility tokens
  /** A token corresponding to end of input. */
  EOF,
  /** A token corresponding to an error. */
  ERROR,
  /** A token corresponding to nothingness. */
  EMPTY,

  // Paired delimiters.
  // These are delimiters that, when encountered,
  // we can expect to see another, related lexeme.

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

  // Single-character delimiters.
  // These are delimiters that can stand
  // on their own.

  /** A token corresponding to `;` */
  SEMICOLON,
  /** A token corresponding to `:` */
  COLON,
  /** A token corresponding to `.` */
  DOT,
  /** A token corresponding to `COMMA` */
  COMMA,

  // Operator delimiters.
  // These are a subset of the single-character
  // delimiters. They are associated with certain
  // operations.

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

  // Vector Operators.
  // These are operators associated with
  // vectors.

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
  /** A token corresponding to `#` */
  POUND,

  // Matrix Operators
  // These are operators associated with
  // matrices.

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
  /** A token corresponding to a big fraction. */
  BIG_FRACTION,
  /** A token corresponding to a symbol. */
  SYMBOL,
  /** A token corresponding to a string. */
  STRING,
  /** A token corresponding to a symbolic string. */
  SYM_STRING,
  /** A token corresponding to a Boolean literal. */
  BOOLEAN,
  /** A token corresponding to the literal "NaN" (Not a Number) */
  NAN,
  /** A token corresponding to the literal "Inf" (Infinity). */
  INF,
  /** A token corresponding to the literal "Nil". */
  NIL,
  /** A token corresponding to the literal "Undefined". */
  UNDEFINED,

  // Keyword Tokens
  /** A token corresponding to the keyword "and". */
  AND,
  /** A token corresponding to the keyword "or". */
  OR,
  /** A token corresponding to the keyword "not". */
  NOT,
  /** A token corresponding to the keyword "nand". */
  NAND,
  /** A token corresponding to the keyword "xor". */
  XOR,
  /** A token corresponding to the keyword "xnor". */
  XNOR,
  /** A token corresponding to the keyword "nor". */
  NOR,
  /** A token corresponding to the keyword "if". */
  IF,
  /** A token corresponding to the keyword "else". */
  ELSE,
  /** A token corresponding to the keyword "fn". */
  FN,
  /** A token corresponding to the keyword "let". */
  LET,
  /** A token corresponding to the keyword "var". */
  VAR,
  /** A token corresponding to the keyword "return". */
  RETURN,
  /** A token corresponding to the keyword "while". */
  WHILE,
  /** A token corresponding to the keyword "for". */
  FOR,
  /** A token corresponding to the keyword "class". */
  CLASS,
  /** A token corresponding to the keyword "print". */
  PRINT,
  /** A token corresponding to the keyword "super". */
  SUPER,
  /** A token corresponding to the keyword "this". */
  THIS,
  /** A token corresponding to the keyword "rem". */
  REM,
  /** A token corresponding to the keyword "mod". */
  MOD,
  /** A token corresponding to the keyword "div". */
  DIV,
  /** A token corresponding to some numeric constant. */
  NUMERIC_CONSTANT,
  /** A token corresponding to some native function name. */
  NATIVE_FUNCTION,
}

type Keyword =
  | "nan"
  | "inf"
  | "nil"
  | 'undefined'
  | "and"
  | "or"
  | "not"
  | "nand"
  | "xor"
  | "xnor"
  | "nor"
  | "if"
  | "else"
  | "fn"
  | "let"
  | "var"
  | "return"
  | "while"
  | "for"
  | "class"
  | "print"
  | "super"
  | "this"
  | "rem"
  | "mod"
  | "div"
  | "false"
  | "true";

type NumberTokenType =
  | TOKEN.INTEGER
  | TOKEN.BIG_INTEGER
  | TOKEN.FLOAT
  | TOKEN.FRACTION
  | TOKEN.BIG_FLOAT;

/**
 * Represents a token.
 */
export class TokenObj {
  /** This token's lexeme. */
  _lexeme: string;

  /** The line where this token was found. */
  _line: number;

  /** The literal corresponding to this token's lexeme. */
  _literal: Expression | null = null;

  /** This token's corresponding token. */
  _token: TOKEN;

  /** Sets this token's lexeme. */
  lexeme(lexeme: string) {
    this._lexeme = lexeme;
    return this;
  }

  /** Sets the line where this token was found. */
  line(line: number) {
    this._line = Math.floor(line);
    return this;
  }

  literal(literal: Expression) {
    this._literal = literal;
    return this;
  }

  /**
   * Sets this token's type.
   */
  token(token: TOKEN) {
    this._token = token;
    return this;
  }

  toString() {
    return `{_token: ${TOKEN[this._token]}, _lexeme: ${this._lexeme}, _line: ${this._line}}`;
  }

  constructor(type: TOKEN, lexeme: string, line: number) {
    this._token = type;
    this._lexeme = lexeme;
    this._line = line;
  }
}

/**
 * Returns a new token.
 */
export function token(type: TOKEN, lexeme: string, line: number) {
  return new TokenObj(type, lexeme, line);
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

/** Returns a new lexical error. */
export function lexicalError(message: string, line: number) {
  return new LEXICAL_ERROR(message, line);
}

type NativeUnary =
  | "ceil"
  | "floor"
  | "sin"
  | "cos"
  | "cosh"
  | "tan"
  | "lg"
  | "ln"
  | "log"
  | "arcsin"
  | "arccos"
  | "arcsinh"
  | "arctan"
  | "sinh"
  | "sqrt"
  | "tanh"
  | "gcd"
  | "avg"
  | "arccosh";

/** A native function that takes more than 1 argument. */
type NativePolyAry = "max" | "min";

type NativeFn = NativeUnary | NativePolyAry;

type NativeConstants =
  | "e"
  | "pi"
  | "ln2"
  | "ln10"
  | "log10e"
  | "log2e"
  | "sqrt2";

export function lexicalAnalyzer(code: string) {
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
  const tick = (): string => code[_current++] ?? "";

  /**
   * Returns the source substring _start to _current.
   */
  const slice = (): string => code.slice(_start, _current);

  /**
   * Returns a new token.
   */
  const tkn = (tokentype: TOKEN, lexeme: string = "") =>
    token(tokentype, lexeme ? lexeme : slice(), _line);

  /**
   * Returns an error token. If this function is called,
   * the mutable variable `_error` is set, halting all
   * scanning.
   */
  const errorToken = (message: string) => {
    _error = lexicalError(message, _line);
    return tkn(TOKEN.ERROR, "").line(_line);
  };

  /**
   * Returns the current character being scanned without
   * moving the scanner forward.
   */
  const peek = () => (atEnd() ? "" : (code[_current] ?? ""));

  /**
   * Returns the character just ahead of the current
   * character without moving the scanner forward.
   */
  const peekNext = (): string => (atEnd() ? "" : (code[_current + 1] ?? ""));

  /**
   * Returns the character `n` places
   * ahead of the current character without
   * moving the scanner forward.
   */
  const lookup = (n: number): string =>
    atEnd() ? "" : (code[_current + n] ?? "");

  /**
   * If the given expected character `c` matches,
   * return true and increment the `_current`
   * variable (the scanner moves forward).
   * Otherwise, return false without
   * incrementing (the scanner doesn't move
   * forward).
   */
  const match = (c: string): boolean => {
    if (atEnd()) return false;
    if (code[_current] !== c) return false;
    _current++;
    return true;
  };

  /**
   * Returns true if the current character pointed
   * at by `_current` matches the provided character c.
   * False otherwise.
   */
  const peekIs = (c: string): boolean => peek() === c;

  /**
   * Consumes all whitespace while moving
   * the scanner's `_current` pointer forward.
   */
  const skipWhitespace = (): void => {
    while (!atEnd()) {
      const char: string = peek();
      switch (char) {
        case " ":
        case "\r":
        case "\t":
          tick();
          break;
        case "\n":
          _line++;
          tick();
          break;
        default:
          return;
      }
    }
  };

  const numConsts: Record<NativeConstants, () => TokenObj> = {
    e: () => tkn(TOKEN.NUMERIC_CONSTANT).literal(numConst("e", Math.E)),
    pi: () => tkn(TOKEN.NUMERIC_CONSTANT).literal(numConst("pi", Math.PI)),
    ln10: () =>
      tkn(TOKEN.NUMERIC_CONSTANT).literal(numConst("ln10", Math.LN10)),
    ln2: () => tkn(TOKEN.NUMERIC_CONSTANT).literal(numConst("ln2", Math.LN2)),
    log10e: () =>
      tkn(TOKEN.NUMERIC_CONSTANT).literal(numConst("log10e", Math.LOG10E)),
    log2e: () =>
      tkn(TOKEN.NUMERIC_CONSTANT).literal(numConst("log2e", Math.LOG2E)),
    sqrt2: () =>
      tkn(TOKEN.NUMERIC_CONSTANT).literal(numConst("sqrt2", Math.SQRT2)),
  };

  /**
   * Record of native functions. Each key corresponds
   * to the native function name. The number mapped to
   * by the key is the function’s arity (the number
   * of arguments the function takes).
   */
  const nativeFunctions: Record<NativeFn, number> = {
    avg: 1,
    gcd: 1,
    sqrt: 1,
    ceil: 1,
    tanh: 1,
    floor: 1,
    sinh: 1,
    cosh: 1,
    sin: 1,
    cos: 1,
    tan: 1,
    lg: 1,
    ln: 1,
    log: 1,
    arctan: 1,
    arccos: 1,
    arccosh: 1,
    arcsin: 1,
    arcsinh: 1,
    max: 1,
    min: 1,
  };

  /** Dictionary of keywords to tokens. */
  const dictionary: Record<Keyword, () => TokenObj> = {
    this: () => tkn(TOKEN.THIS),
    super: () => tkn(TOKEN.SUPER),
    class: () => tkn(TOKEN.CLASS),
    false: () => tkn(TOKEN.BOOLEAN).literal(bool(false)),
    true: () => tkn(TOKEN.BOOLEAN).literal(bool(true)),
    nan: () => tkn(TOKEN.NAN).literal(nan()),
    inf: () => tkn(TOKEN.INF).literal(inf("+")),
    undefined: () => tkn(TOKEN.UNDEFINED).literal(UNDEFINED()),
    return: () => tkn(TOKEN.RETURN),
    while: () => tkn(TOKEN.WHILE),
    for: () => tkn(TOKEN.FOR),
    let: () => tkn(TOKEN.LET),
    var: () => tkn(TOKEN.VAR),
    fn: () => tkn(TOKEN.FN),
    if: () => tkn(TOKEN.IF),
    else: () => tkn(TOKEN.ELSE),
    print: () => tkn(TOKEN.PRINT),
    rem: () => tkn(TOKEN.REM),
    mod: () => tkn(TOKEN.MOD),
    div: () => tkn(TOKEN.DIV),
    nil: () => tkn(TOKEN.NIL),
    and: () => tkn(TOKEN.AND),
    or: () => tkn(TOKEN.OR),
    nor: () => tkn(TOKEN.NOR),
    xor: () => tkn(TOKEN.XOR),
    xnor: () => tkn(TOKEN.XNOR),
    not: () => tkn(TOKEN.NOT),
    nand: () => tkn(TOKEN.NAND),
  };

  /** Generates a word token. */
  const wordToken = () => {
    while ((isValidNameChar(peek()) || isDigit(peek())) && !atEnd()) {
      tick();
    }
    const word = slice();
    const native = nativeFunctions[word as NativeFn];
    if (native) {
      return tkn(TOKEN.NATIVE_FUNCTION);
    } else if (dictionary[word as Keyword]) {
      return dictionary[word as Keyword]();
    } else if (numConsts[word as NativeConstants]) {
      return numConsts[word as NativeConstants]();
    } else {
      return tkn(TOKEN.SYMBOL);
    }
  };

  const decimalToken = (
    numberString: string,
    type: NumberTokenType,
    hasSeparators: boolean
  ) => {
    const numstr = hasSeparators
      ? numberString.replaceAll("_", "")
      : numberString;
    switch (type) {
      // handle integers
      case TOKEN.INTEGER: {
        const n = Number.parseInt(numstr);
        if (n > Number.MAX_SAFE_INTEGER) {
          return tkn(TOKEN.BIG_INTEGER).literal(bigint(BigInt(n)));
        } else {
          return tkn(TOKEN.INTEGER).literal(int(n));
        }
      }
      // handle floats
      case TOKEN.FLOAT: {
        const n = Number.parseFloat(numstr);
        // handle very big floats
        if (n > Number.MAX_VALUE) {
          const exponential = n.toExponential();
          const [M, N] = exponential.split("e");
          const mantissa = Number.parseFloat(M ?? "0");
          const exponent = Number.parseFloat(N ?? "0");
          return tkn(TOKEN.BIG_FLOAT).literal(bigfloat(mantissa, exponent));
        } else {
          return tkn(TOKEN.FLOAT).literal(float(n));
        }
      }
      // handle fractions
      case TOKEN.FRACTION: {
        const [numerator_as_string, denominator_as_string] = numstr.split("|");
        const numerator_as_number = Number.parseInt(numerator_as_string ?? "0");
        const denominator_as_number = Number.parseInt(
          denominator_as_string ?? "0"
        );
        if (
          numerator_as_number > Number.MAX_SAFE_INTEGER ||
          denominator_as_number > Number.MAX_SAFE_INTEGER
        ) {
          return tkn(TOKEN.BIG_FRACTION).literal(
            bigfrac(BigInt(numerator_as_number), BigInt(denominator_as_number))
          );
        } else {
          return tkn(TOKEN.FRACTION).literal(
            frac(numerator_as_number, denominator_as_number)
          );
        }
      }
    }
    return errorToken("Unrecognized decimal");
  };

  /**
   * Scans a decimal token.
   */
  const scanDecimal = (initialType: NumberTokenType) => {
    let type = initialType;
    // Flag indicating whether this token has separators.
    let hasSeparators = false;
    // Keep moving forward as long as we encounter a digit.
    while (isDigit(peek()) && !atEnd()) {
      tick();
    }
    // We got as many digits as we could get.
    // Now we check if there's a separator.
    if (peekIs("_") && isDigit(peekNext())) {
      // We have a separator, so set the flag.
      hasSeparators = true;
      tick(); // Consume the separator "_"
      // Counter for how many digits follow the separator.
      let digits = 0;
      // As long as we encounter digits, keep moving forward.
      while (isDigit(peek()) && !atEnd()) {
        tick();
        // And as we keep consuming digits, keep the digit count.
        digits++;
        // Now, if we see a separator ahead AND
        // there's a digit following it, we must
        // check if the formatting is correct.
        if (peekIs("_") && isDigit(peekNext())) {
          // We require 3 digits after a separator, always.
          if (digits === 3) {
            // If we have 3 digits, move forward.
            tick();
            // Reset the digit counter.
            digits = 0;
          } else {
            // If we don't have 3 digits following the separator,
            // return an error token.
            return errorToken(
              `Expected 3 ASCII digits after the separator "_".`
            );
          }
        }
      }
      // It could be the case that we never entered
      // the while-loop from earlier. I.e., we never
      // saw digits after the separator. That's a problem.
      // We require 3 ASCII digits after the "_". So, we
      // make that check here.
      if (digits !== 3) {
        return errorToken(`Expected 3 ASCII digits after the "_".`);
      }
    }
    // Let's handle floating point numbers.
    // If the next character is a dot followed by,
    // a digit, then we have a floating point number.
    if (peekIs(".") && isDigit(peekNext())) {
      // Consume the "."
      tick();
      // Change the current type.
      type = TOKEN.FLOAT;
      // As long as we keep seeing digits,
      while (isDigit(peek()) && !atEnd()) {
        // move forward
        tick();
      }
    }
    // Now let's handle fractions.
    if (peekIs("|")) {
      if (type !== TOKEN.INTEGER) {
        return errorToken(`Expected an integer before "|"`);
      }
      type = TOKEN.FRACTION;
      tick();
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
      return decimalToken(slice(), type, hasSeparators);
    }
    // Handle raw big floats.
    // These are floating point numbers written in
    // scientific notation.

    return decimalToken(slice(), type, hasSeparators);
  };

  const stringToken = () => {
    while (peek() !== `"` && !atEnd()) {
      if (peek() !== `\n`) {
        _line++;
      }
      tick();
    }
    if (atEnd()) {
      return errorToken(`Unterminated string.`);
    }
    tick();
    const lex = slice().slice(1, -1);
    return tkn(TOKEN.STRING, lex).literal(str(lex));
  };

  const algebraStringToken = () => {
    while (peek() !== `'` && !atEnd()) {
      if (peek() !== `\n`) {
        _line++;
      }
      tick();
    }
    if (atEnd()) {
      return errorToken(`Unterminated algebraic string`);
    }
    tick();
    const s = slice().replaceAll(`'`, "");
    return tkn(TOKEN.SYM_STRING).literal(str(s).algebraic(true));
  };

  const scan = (): TokenObj => {
    // We start by skipping whitespace.
    skipWhitespace();

    // Set the _start and _current pointers to
    // point at the same character.
    _start = _current;

    // If we've reached the end of the input source,
    // immediately return an END token.
    if (atEnd()) {
      return tkn(TOKEN.EOF, "EOF");
    }

    // Now we get the current character
    // and move the scannar forward.
    const char = tick();

    // If the character is a valid name starter (a Latin
    // or Greek character, a unicode math symbol,
    // an underscore, or a `$`), returns a word token.
    if (isValidNameChar(char)) {
      return wordToken();
    }

    // If the character is `#` then we might
    // have a matrix operator.
    if (char === "#") {
      if (match("+")) {
        return tkn(TOKEN.POUND_PLUS);
      } else if (match("-")) {
        return tkn(TOKEN.POUND_MINUS);
      } else if (match("*")) {
        return tkn(TOKEN.POUND_STAR);
      } else {
        return tkn(TOKEN.POUND);
      }
    }

    // If the character is a digit,
    // then we have a number token.
    if (isDigit(char)) {
      // Scan binary number
      if (char === "0" && match("b")) {
        if (!(peekIs("0") || peekIs("1"))) {
          return errorToken(`Expected binary digits after "0b"`);
        }
        while ((peekIs("0") || peekIs("1")) && !atEnd()) {
          tick();
        }
        const numberString = slice().replace("0b", "0");
        const integerValue = Number.parseInt(numberString, 2);
        return tkn(TOKEN.INTEGER).literal(int(integerValue));
      }
      // Scan octadecimal number token
      else if (char === "0" && match("o")) {
        if (isOctalDigit(peek())) {
          return errorToken(`Expected octal digits after "0o"`);
        }
        while (isOctalDigit(peek()) && !atEnd()) {
          tick();
        }
        const numberString = slice().replace("0o", "");
        const integerValue = Number.parseInt(numberString, 0);
        return tkn(TOKEN.INTEGER).literal(int(integerValue));
      }
      // Scan hexadecimal number
      else if (char === "0" && match("x")) {
        if (!isHexDigit(peek())) {
          return errorToken(`Expected hexadecimals after "0x"`);
        }
        while (isHexDigit(peek()) && !atEnd()) {
          tick();
        }
        const numberString = slice().replace("0x", "");
        const integerValue = Number.parseInt(numberString, 16);
        return tkn(TOKEN.INTEGER).literal(int(integerValue));
      } else {
        return scanDecimal(TOKEN.INTEGER);
      }
    }

    // Now we handle the individual characters
    switch (char) {
      case "@":
        return tkn(TOKEN.AT);
      case ":":
        return tkn(TOKEN.COLON);
      case "&":
        return tkn(TOKEN.AMPERSAND);
      case "~":
        return tkn(TOKEN.TILDE);
      case "|":
        return tkn(TOKEN.VBAR);
      case "(":
        return tkn(TOKEN.LEFT_PAREN);
      case ")":
        return tkn(TOKEN.RIGHT_PAREN);
      case "[":
        return tkn(TOKEN.LEFT_BRACKET);
      case "]":
        return tkn(TOKEN.RIGHT_BRACKET);
      case "{":
        return tkn(TOKEN.LEFT_BRACE);
      case "}":
        return tkn(TOKEN.RIGHT_BRACE);
      case ",":
        return tkn(TOKEN.COMMA);
      case "*":
        return tkn(match("*") ? TOKEN.STAR_STAR : TOKEN.STAR);
      case ";":
        return tkn(TOKEN.SEMICOLON);
      case "%":
        return tkn(TOKEN.PERCENT);
      case "/":
        return tkn(TOKEN.SLASH);
      case "^":
        return tkn(TOKEN.CARET);
      case "!":
        return tkn(match("=") ? TOKEN.BANG_EQUAL : TOKEN.BANG);
      case "<":
        return tkn(match("=") ? TOKEN.LESS_EQUAL : TOKEN.LESS);
      case ">":
        return tkn(match("=") ? TOKEN.GREATER_EQUAL : TOKEN.GREATER);
      case "+":
        return tkn(match("+") ? TOKEN.PLUS_PLUS : TOKEN.PLUS);
      case ".": {
        if (match("+")) {
          return tkn(TOKEN.DOT_ADD);
        } else if (match("-")) {
          return tkn(TOKEN.DOT_MINUS);
        } else if (match("*")) {
          return tkn(TOKEN.DOT_STAR);
        } else if (match("^")) {
          return tkn(TOKEN.DOT_CARET);
        } else {
          return tkn(TOKEN.DOT);
        }
      }
      // special handling of "-" for inline comments
      case "-": {
        if (peek() === "-" && peekNext() === "-") {
          while (peek() !== "\n" && !atEnd()) {
            tick();
          }
          return tkn(TOKEN.EMPTY);
        } else {
          return tkn(match("-") ? TOKEN.MINUS_MINUS : TOKEN.MINUS);
        }
      }
      // special handling of "=" for block comments
      case "=": {
        if (peek() === "=" && peekNext() === "=") {
          while (peek() === "=") {
            tick();
          }
          while (!atEnd()) {
            tick();
            if (peek() === "=" && peekNext() === "=" && lookup(2) === "=") {
              break;
            }
          }
          if (atEnd()) {
            return errorToken(`Unterminated block comment.`);
          }
          while (peek() === "=") {
            tick();
          }
          return tkn(TOKEN.EMPTY);
        } else {
          return tkn(match("=") ? TOKEN.EQUAL_EQUAL : TOKEN.EQUAL);
        }
      }
      case '"':
        return stringToken();
      case `'`:
        return algebraStringToken();
    }
    return errorToken(`Unrecognized token: ${char}`);
  };
  const stream = () => {
    const out: TokenObj[] = [];
    let prev = token(TOKEN.EMPTY, "EMPTY", -1);
    let now = scan();
    if (now._token !== TOKEN.EMPTY) {
      out.push(now);
    } else if (_error !== null) {
      return left(_error);
    }
    let peek = scan();
    if (_error !== null) {
      return left(_error);
    }
    while (!atEnd()) {
      prev = now;
      now = peek;
      const k = scan();
      if (_error !== null) {
        return left(_error);
      }
      if (k._token === TOKEN.EMPTY) {
        continue;
      } else {
        peek = k;
      }
      if (
        (prev._token === TOKEN.RIGHT_PAREN ||
          prev._token === TOKEN.RIGHT_BRACE ||
          prev._token === TOKEN.RIGHT_BRACKET) &&
        now._token === TOKEN.COMMA &&
        (peek._token === TOKEN.RIGHT_PAREN ||
          peek._token === TOKEN.RIGHT_BRACE ||
          peek._token === TOKEN.RIGHT_BRACKET)
      ) {
        continue;
      }
      out.push(now);
    }
    out.push(peek);
    return right(out);
  };
  return {
    scan,
    stream,
  };
}

/**
 * The binding power of a given operator.
 * Values of type `bp` are used the parsers
 * to determinate operator precedence.
 */
enum BP {
  NIL,
  LOWEST,
  STRINGOP,
  ASSIGN,
  ATOM,
  OR,
  NOR,
  AND,
  NAND,
  XOR,
  XNOR,
  NOT,
  EQ,
  REL,
  SUM,
  DIFFERENCE,
  PRODUCT,
  QUOTIENT,
  IMUL,
  POWER,
  DOT_PRODUCT,
  POSTFIX,
  CALL,
}

export function syntaxAnalysis() {}

