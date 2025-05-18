import { isDigit, isValidNameChar } from "./parsing_utils";
import { TOKEN_TYPE } from "./token_type";

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
 * Represents an error generally.
 */
abstract class ERROR extends Error {
  constructor(message: string) {
    super(message);
  }
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

  /** This token's type. */
  _type: TOKEN_TYPE;

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

  /**
   * Sets this token's type.
   */
  type(type: TOKEN_TYPE) {
    this._type = type;
    return this;
  }

  constructor(type: TOKEN_TYPE, lexeme: string, line: number) {
    this._type = type;
    this._lexeme = lexeme;
    this._line = line;
  }
}

/**
 * Returns a new token.
 */
export function token(type: TOKEN_TYPE, lexeme: string, line: number) {
  return new Token(type, lexeme, line);
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
  const tick = (): string => code[_current++] ?? "";

  /**
   * Returns the source substring _start to _current.
   */
  const slice = (): string => code.slice(_start, _current);

  /**
   * Returns a new token.
   */
  const tkn = (tokentype: TOKEN_TYPE, lexeme: string = "") =>
    token(tokentype, lexeme ? lexeme : slice(), _line);

  /**
   * Returns an error token. If this function is called,
   * the mutable variable `_error` is set, halting all
   * scanning.
   */
  const errorToken = (message: string) => {
    _error = lexicalError(message, _line);
    return tkn(TOKEN_TYPE.ERROR, "").line(_line);
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

  const numConsts: Record<NativeConstants, () => Token> = {
    e: () => tkn(TOKEN_TYPE.NUMERIC_CONSTANT).literal(numConst("e", Math.E)),
    pi: () => tkn(TOKEN_TYPE.NUMERIC_CONSTANT).literal(numConst("pi", Math.PI)),
    ln10: () =>
      tkn(TOKEN_TYPE.NUMERIC_CONSTANT).literal(numConst("ln10", Math.LN10)),
    ln2: () =>
      tkn(TOKEN_TYPE.NUMERIC_CONSTANT).literal(numConst("ln2", Math.LN2)),
    log10e: () =>
      tkn(TOKEN_TYPE.NUMERIC_CONSTANT).literal(numConst("log10e", Math.LOG10E)),
    log2e: () =>
      tkn(TOKEN_TYPE.NUMERIC_CONSTANT).literal(numConst("log2e", Math.LOG2E)),
    sqrt2: () =>
      tkn(TOKEN_TYPE.NUMERIC_CONSTANT).literal(numConst("sqrt2", Math.SQRT2)),
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
  const dictionary: Record<string, () => Token> = {
    this: () => tkn(TOKEN_TYPE.THIS),
    super: () => tkn(TOKEN_TYPE.SUPER),
    class: () => tkn(TOKEN_TYPE.CLASS),
    false: () => tkn(TOKEN_TYPE.BOOLEAN).literal(bool(false)),
    true: () => tkn(TOKEN_TYPE.BOOLEAN).literal(bool(true)),
    NaN: () => tkn(TOKEN_TYPE.NAN).literal(numConst("NaN", NaN)),
    Inf: () => tkn(TOKEN_TYPE.INF).literal(numConst("Inf", Infinity)),
    return: () => tkn(TOKEN_TYPE.RETURN),
    while: () => tkn(TOKEN_TYPE.WHILE),
    for: () => tkn(TOKEN_TYPE.FOR),
    let: () => tkn(TOKEN_TYPE.LET),
    var: () => tkn(TOKEN_TYPE.VAR),
    fn: () => tkn(TOKEN_TYPE.FN),
    if: () => tkn(TOKEN_TYPE.IF),
    else: () => tkn(TOKEN_TYPE.ELSE),
    print: () => tkn(TOKEN_TYPE.PRINT),
    rem: () => tkn(TOKEN_TYPE.REM),
    mod: () => tkn(TOKEN_TYPE.MOD),
    div: () => tkn(TOKEN_TYPE.DIV),
    nil: () => tkn(TOKEN_TYPE.NIL),
    and: () => tkn(TOKEN_TYPE.AND),
    or: () => tkn(TOKEN_TYPE.OR),
    nor: () => tkn(TOKEN_TYPE.NOR),
    xor: () => tkn(TOKEN_TYPE.XOR),
    xnor: () => tkn(TOKEN_TYPE.XNOR),
    not: () => tkn(TOKEN_TYPE.NOT),
    nand: () => tkn(TOKEN_TYPE.NAND),
    list: () => tkn(TOKEN_TYPE.LIST),
  };

  /** Generates a word token. */
  const wordToken = () => {
    while ((isValidNameChar(peek()) || isDigit(peek())) && !atEnd()) {
      tick();
    }
    const word = slice();
    const native = nativeFunctions[word as NativeFn];
    if (native) {
      return tkn(TOKEN_TYPE.NATIVE_FUNCTION);
    } else if (dictionary[word]) {
      return dictionary[word]();
    } else if (numConsts[word as NativeConstants]) {
      return numConsts[word as NativeConstants]();
    } else {
      return tkn(TOKEN_TYPE.SYMBOL);
    }
  };

  const scan = () => {
    // We start by skipping whitespace.
    skipWhitespace();

    // Set the _start and _current pointers to
    // point at the same character.
    _start = _current;

    // If we've reached the end of the input source,
    // immediately return an END token.
    if (atEnd()) {
      return tkn(TOKEN_TYPE.EOF, "EOF");
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
  };
}
