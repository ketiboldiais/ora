// Utility Functions
export function isUnsafe(x: unknown): x is undefined | null  {
  return (x === undefined) || (x === null) 
}

enum EXPR {
  INTEGER,
  BIG_INTEGER,
  FLOAT,
  BIG_FLOAT,
  FRACTION,
  BIG_FRACTION,
  COMPLEX,
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
 * Returns true, and asserts, only if
 * the given expression `expr` is
 * an Integer.
 */
export function isInt(expr: Expression): expr is Integer {
  return (!isUnsafe(expr)) && expr.kind() === EXPR.INTEGER;
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
 * An object corresponding to a fraction. That is,
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

type Real = Integer | BigInteger | Float | BigFloat | Fraction | BigFraction;

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

export function complex(re: Real, im: Real) {
  return new Complex(re, im);
}
