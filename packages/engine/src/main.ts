/* eslint-disable @typescript-eslint/no-explicit-any */
import { Either, left, right } from "./utils_fp";
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
  // BIG_FLOAT,
  FRACTION,
  BIG_FRACTION,
  COMPLEX,
  NIL,
  SYMBOL,
  BOOL,
  NUMERIC_CONSTANT,
  STRING,
  SYMSTRING,
  INF,
  NAN,
  UNDEFINED,
  RELATION,
  EQUATION,
  SUM,
  DIFFERENCE,
  PRODUCT,
  QUOTIENT,
  POWER,
  FUNC,
  CORE_FN,
}

/** An object corresponding to an Expression. */
abstract class Expression {
  abstract kind(): EXPR;
  abstract toString(): string;
  abstract sexpr(): $EXPRESSION;
  abstract isAtomic(): this is Atomic;
  abstract isCompound(): this is Compound;
  abstract equals(other: Expression): boolean;
}

abstract class Atomic extends Expression {
  isAtomic(): this is Atomic {
    return true;
  }
  isCompound(): this is Compound {
    return false;
  }
}

abstract class Compound extends Expression {
  isAtomic(): this is Atomic {
    return true;
  }
  isCompound(): this is Compound {
    return false;
  }
  equals(other: Expression): boolean {
    return (
      other.isCompound() &&
      other._op === this._op &&
      this._args.length === other._args.length &&
      this._args.reduce(
        (prev, curr, i) => prev && curr.equals(other._args[i] ?? nil()),
        true
      )
    );
  }
  _op: string;
  _args: Expression[];
  constructor(op: string, args: Expression[]) {
    super();
    this._op = op;
    this._args = args;
  }
}

abstract class Numeric extends Atomic {}

abstract class Real extends Numeric {
  abstract sign(): -1 | 0 | 1;
  abstract toExponential(): string;
  // abstract get _exponent(): number;
}

type $EXPRESSION =
  | $INTEGER
  | $BIG_INTEGER
  | $FLOAT
  // | $BIG_FLOAT
  | $FRACTION
  | $BIG_FRACTION
  | $COMPLEX
  | $SYMBOL
  | $NUMERIC_CONSTANT
  | $INF
  | $NAN
  | $UNDEFINED
  | $BOOL
  | $STRING
  | $SYMSTRING
  | $NIL
  | $RELATION
  | $EQUATION
  | $SUM
  | $DIFFERENCE
  | $PRODUCT
  | $QUOTIENT
  | $POWER
  | $FUNC;

type $INTEGER = ["INTEGER", number];
type $BIG_INTEGER = ["BIG_INTEGER", bigint];
type $FLOAT = ["FLOAT", number];
// type $BIG_FLOAT = ["BIG_FLOAT", string];
type $FRACTION = ["FRACTION", string];
type $BIG_FRACTION = ["BIG_FRACTION", string];
type $COMPLEX = ["COMPLEX", string];
type $SYMBOL = ["SYMBOL", string];
type $NUMERIC_CONSTANT = ["NUMERIC_CONSTANT", [string, number]];
type $INF = ["INF", `${"+" | "-"}inf`];
type $UNDEFINED = ["UNDEFINED", "undefined"];
type $BOOL = ["BOOL", boolean];
type $NAN = ["NAN", "nan"];
type $STRING = ["STRING", string];
type $SYMSTRING = ["SYMSTRING", string];
type $NIL = ["NIL", "nil"];
type $RELATION = [RelationOperator, $EXPRESSION[]];
type $EQUATION = ["==", [$EXPRESSION, $EXPRESSION]];
type $SUM = ["+", $EXPRESSION[]];
type $DIFFERENCE = ["-", $EXPRESSION[]];
type $PRODUCT = ["*", $EXPRESSION[]];
type $QUOTIENT = ["/", $EXPRESSION[]];
type $POWER = ["^", $EXPRESSION[]];
type $FUNC = [`𝑓`, string, $EXPRESSION[]];

export function sexprString(expression: $EXPRESSION): string {
  const type = expression[0];
  switch (type) {
    case "INTEGER":
      return `(int ${expression[1]})`;
    case "BIG_INTEGER":
      return `(big_integer ${expression[1]})`;
    case "FLOAT":
      return `(float ${expression[1]})`;
    // case "BIG_FLOAT":
    //   return `(big_float ${expression[1]})`;
    case "FRACTION":
      return `(fraction ${expression[1]})`;
    case "BIG_FRACTION":
      return `(big_fraction ${expression[1]})`;
    case "COMPLEX":
      return `(complex ${expression[1]})`;
    case "SYMBOL":
      return `(symbol ${expression[1]})`;
    case "NUMERIC_CONSTANT":
      return `(numeric_constant ${expression[1][0]} ${expression[1][1]})`;
    case "INF":
      return `${expression[1]}`;
    case "UNDEFINED":
      return `undefined`;
    case "BOOL":
      return `(bool ${expression[1]})`;
    case "NAN":
      return `nan`;
    case "STRING":
      return `(string "${expression[1]}")`;
    case "SYMSTRING":
      return `(symstring '${expression[1]}')`;
    case "NIL":
      return `nil`;
    case "+":
      return `(+ ${expression[1].map((x) => sexprString(x)).join(" ")})`;
    case "-":
      return `(- ${expression[1].map((x) => sexprString(x)).join(" ")})`;
    case "*":
      return `(* ${expression[1].map((x) => sexprString(x)).join(" ")})`;
    case "/":
      return `(/ ${expression[1].map((x) => sexprString(x)).join(" ")})`;
    case "^":
      return `(^ ${expression[1].map((x) => sexprString(x)).join(" ")})`;
    case "𝑓":
      return `(𝑓 ${expression[1]} ${expression[2].map((x) => sexprString(x)).join(" ")})`;
    default:
      return `(${expression[0]} ${expression[1].map((x) => sexprString(x)).join(" ")})`;
  }
}

/** An object corresponding to an Integer. */
class Integer extends Real {
  /** The value of this integer. */
  _value: number;

  toExponential(): string {
    return `${this._value}E+0`;
  }

  sign(): -1 | 0 | 1 {
    return this._value < 0 ? -1 : this._value > 0 ? 1 : 0;
  }

  kind(): EXPR {
    return EXPR.INTEGER;
  }

  toString(): string {
    return `${this._value}`;
  }

  sexpr(): $EXPRESSION {
    return ["INTEGER", this._value];
  }

  equals(other: Expression): boolean {
    return isInt(other) && this._value === other._value;
  }

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
export function isInt(expr: any): expr is Integer {
  return !isUnsafe(expr) && expr.kind() === EXPR.INTEGER;
}

/** An object corresponding to a Big Integer. */
export class BigInteger extends Real {
  kind(): EXPR {
    return EXPR.BIG_INTEGER;
  }

  toExponential(): string {
    let value = this._value;
    if (typeof value !== "bigint")
      throw new Error(
        "Argument must be a bigint, but a " + typeof value + " was supplied."
      );

    //

    const isNegative = value < 0;
    if (isNegative) value = -value; // Using the absolute value for the digits.

    const str = value.toString();

    const exp = str.length - 1;
    if (exp == 0) return (isNegative ? "-" : "") + str + "E+0";

    const mantissaDigits = str.replace(/(0+)$/, ""); // Remove any mathematically insignificant zeroes.

    // Use the single first digit for the integral part of the mantissa, and all following digits for the fractional part (if any).
    let mantissa = mantissaDigits.charAt(0);
    if (mantissaDigits.length > 1) {
      mantissa += "." + mantissaDigits.substring(1);
    }

    return (isNegative ? "-" : "") + mantissa + "E+" + exp.toString();
  }

  sign(): -1 | 0 | 1 {
    return this._value < 0 ? -1 : this._value > 0 ? 1 : 0;
  }

  toString(): string {
    return `${this._value}`;
  }

  sexpr(): $EXPRESSION {
    return ["BIG_INTEGER", this._value];
  }

  equals(other: Expression): boolean {
    return isBigInt(other) && other._value === this._value;
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
export class Float extends Real {
  kind(): EXPR {
    return EXPR.FLOAT;
  }
  toExponential(): string {
    const v = this._value.toExponential().replace('e', 'E');
    return v;
    
  }

  sign(): -1 | 0 | 1 {
    return this._value < 0 ? -1 : this._value > 0 ? 1 : 0;
  }

  toString(): string {
    return `${this._value}`;
  }

  sexpr(): $EXPRESSION {
    return ["FLOAT", this._value];
  }

  equals(other: Expression): boolean {
    return isFloat(other) && other._value === this._value;
  }

  /** The value of this real number. */
  _value: number;
  constructor(value: number) {
    super();
    this._value = value;
  }
}

/** Returns a new float. */
export function float(value: number) {
  return new Float(value);
}

export function isFloat(x: Expression): x is Float {
  return !isUnsafe(x) && x.kind() === EXPR.FLOAT;
}

/** Returns a new Big Real number. */
// export class BigFloat extends Real {
//   kind(): EXPR {
//     return EXPR.BIG_FLOAT;
//   }

//   sign(): -1 | 0 | 1 {
//     return this._m < 0 ? -1 : this._m > 0 ? 1 : 0;
//   }

//   toExponential(): string {
//     return this.toString();
//   }

//   toString(): string {
//     const m = `${this._m}`;
//     const n = `${this._n}`;
//     return `${m}E${this._n < 0 ? "" : "+"}${n}`;
//   }

//   sexpr(): $EXPRESSION {
//     return ["BIG_FLOAT", this.toString()];
//   }

//   equals(other: Expression): boolean {
//     return isBigFloat(other) && this._m === other._m && this._n === other._n;
//   }

//   /** This big real's significand. */
//   _m: number;
//   /** This big real's exponent. This must be an integer. */
//   _n: number = 0;
//   constructor(m: number, n: number) {
//     super();
//     this._m = m;
//     this._n = Math.floor(n);
//   }
// }

// /** Returns a new big real number. */
// export function bigfloat(m: number, n: number = 0) {
//   return new BigFloat(m, n);
// }

// export function isBigFloat(x: Expression): x is BigFloat {
//   return !isUnsafe(x) && x.kind() === EXPR.BIG_FLOAT;
// }

/** An object corresponding to a Fraction. */
export class Fraction extends Real {
  kind(): EXPR {
    return EXPR.FRACTION;
  }

  sign(): -1 | 0 | 1 {
    return this._value < 0 ? -1 : this._value > 0 ? 1 : 0;
  }

  toString(): string {
    return `${this._n.toString()}|${this._d.toString()}`;
  }

  toExponential(): string {
    return float(this._value).toExponential();
  }

  sexpr(): $EXPRESSION {
    return ["FRACTION", this.toString()];
  }

  equals(other: Expression): boolean {
    return isFrac(other) && this._n === other._n && this._d === other._d;
  }

  get _value() {
    return this._n._value / this._d._value;
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

export function isFrac(x: Expression): x is Fraction {
  return !isUnsafe(x) && x.kind() === EXPR.FRACTION;
}

/**
 * Represents a fraction--that is,
 * a number of the form `a/b`, where `a` and `b`
 * are bigints.
 */
export class BigFraction extends Real {
  kind(): EXPR {
    return EXPR.BIG_FRACTION;
  }

  sign(): -1 | 0 | 1 {
    return this._value < 0 ? -1 : this._value > 0 ? 1 : 0;
  }

  toString(): string {
    return `${this._n}|${this._d}`;
  }

  sexpr(): $EXPRESSION {
    return ["BIG_FRACTION", this.toString()];
  }

  equals(other: Expression): boolean {
    return isBigFrac(other) && other._n === this._n && other._d === this._d;
  }

  toExponential(): string {
    throw new Error(`BigFraction.toExponential not implemented`)
  }

  get _value() {
    return this._n / this._d;
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

export function isBigFrac(x: Expression): x is BigFraction {
  return !isUnsafe(x) && x.kind() === EXPR.BIG_FRACTION;
}

/**
 * An object corresponding to a Complex number.
 */
export class Complex extends Numeric {
  kind(): EXPR {
    return EXPR.COMPLEX;
  }
  toString(): string {
    return `${this._re} ${this._im.sign() < 0 ? "-" : "+"} ${this._im}i`;
  }
  sexpr(): $EXPRESSION {
    return ["COMPLEX", this.toString()];
  }
  equals(other: Expression): boolean {
    return isComplex(other) && other._re === this._re && other._im === this._im;
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

export function isComplex(u: any): u is Complex {
  return !isUnsafe(u) && u.kind() === EXPR.COMPLEX;
}

/**
 * An object representing a symbol.
 */
export class Sym extends Atomic {
  _sym: string;
  kind() {
    return EXPR.SYMBOL;
  }
  toString(): string {
    return this._sym;
  }
  sexpr(): $EXPRESSION {
    return ["SYMBOL", this._sym];
  }
  equals(other: Expression): boolean {
    return isSym(other) && other._sym === this._sym;
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
 * Returns true if the given expression `u` is a Sym.
 */
export function isSym(u: any): u is Sym {
  return u instanceof Sym;
}

/**
 * An object representing a numeric constant.
 */
export class NumConst extends Atomic {
  _sym: string;
  _value: number;
  kind(): EXPR {
    return EXPR.NUMERIC_CONSTANT;
  }
  toString(): string {
    return this._sym;
  }
  sexpr(): $EXPRESSION {
    return ["NUMERIC_CONSTANT", [this._sym, this._value]];
  }
  equals(other: Expression): boolean {
    return (
      isNumConst(other) &&
      other._sym === this._sym &&
      other._value === this._value
    );
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

/** Returns true if `u` is a `NumConst` expression. */
export function isNumConst(u: any): u is NumConst {
  return u instanceof NumConst;
}

/**
 * An object representing Infinity.
 */
export class Inf extends Atomic {
  kind(): EXPR {
    return EXPR.INF;
  }
  _sign: "+" | "-";
  toString(): string {
    return this._sign + "inf";
  }
  sexpr(): $EXPRESSION {
    return ["INF", this.toString()] as $INF;
  }
  equals(other: Expression): boolean {
    return isInf(other) && other._sign === this._sign;
  }
  constructor(sign: "+" | "-") {
    super();
    this._sign = sign;
  }
}

/** Returns a new Inf (Infinity) object. */
export function inf(sign: "+" | "-") {
  return new Inf(sign);
}

/** Returns true if `u` is an `Inf` expression. */
export function isInf(u: any) {
  return u instanceof Inf;
}

/** An object corresponding to nan (not a number). */
export class NAN extends Atomic {
  kind(): EXPR {
    return EXPR.NAN;
  }
  toString(): string {
    return "nan";
  }
  sexpr(): $EXPRESSION {
    return ["NAN", "nan"];
  }
  equals(other: Expression): boolean {
    return isNAN(other);
  }
  _value: number = NaN;
  constructor() {
    super();
  }
}

/** Returns a new `NAN` expression. */
export function nan() {
  return new NAN();
}

/** Returns true if `u` is a `NAN` expression. */
export function isNAN(u: any): u is NAN {
  return u instanceof NAN;
}

/** An object representing the global symbol "undefined". */
export class Undefined extends Atomic {
  kind(): EXPR {
    return EXPR.UNDEFINED;
  }
  toString(): string {
    return this._value;
  }
  sexpr(): $EXPRESSION {
    return ["UNDEFINED", "undefined"];
  }
  equals(other: Expression): boolean {
    return isUndefined(other);
  }
  _value = "undefined" as const;
  constructor() {
    super();
  }
}

export function UNDEFINED() {
  return new Undefined();
}

/**
 * Returns true if the given expression
 * `u` is the global expression `Undefined`.
 */
export function isUndefined(u: any): u is Undefined {
  return u instanceof Undefined;
}

/*
 * An object representing a Boolean value.
 */
export class Bool extends Atomic {
  _value: boolean;
  kind(): EXPR {
    return EXPR.BOOL;
  }
  toString(): string {
    return `${this._value}`;
  }
  sexpr(): $EXPRESSION {
    return ["BOOL", this._value];
  }
  equals(other: Expression): boolean {
    return isBool(other) && other._value === this._value;
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
 * Returns true if the given expression
 * `u` is a Boolean expression.
 */
export function isBool(u: any): u is Bool {
  return u instanceof Bool;
}

/**
 * An object representing a string.
 */
export class Str extends Atomic {
  kind(): EXPR {
    return EXPR.STRING;
  }
  toString(): string {
    return this._value;
  }
  sexpr(): $EXPRESSION {
    return ["STRING", this._value];
  }
  equals(other: Expression): boolean {
    return isStr(other) && other._value === this._value;
  }
  _value: string;
  constructor(str: string) {
    super();
    this._value = str;
  }
}

/** Returns a new string expression. */
export function str(string: string): Str {
  return new Str(string);
}

/**
 * Returns `true` if the given expression
 * `u` is a Str (string) expression.
 */
export function isStr(u: any): u is Str {
  return u instanceof Str;
}

export class SymStr extends Atomic {
  kind(): EXPR {
    return EXPR.SYMSTRING;
  }
  toString(): string {
    return this._value;
  }
  sexpr(): $EXPRESSION {
    return ["SYMSTRING", this._value];
  }
  equals(other: Expression): boolean {
    return isSymStr(other) && other._value === this._value;
  }
  _value: string;
  constructor(string: string) {
    super();
    this._value = string;
  }
}

/**
 * Returns a new SymStr (Symbolic String) expression.
 */
export function symstring(string: string): SymStr {
  return new SymStr(string);
}

/**
 * Returns `true` if `u` is a SymStr (Symbolic String)
 * expression.
 */
export function isSymStr(u: any): u is SymStr {
  return u instanceof SymStr;
}

/**
 * An object representing the value nil.
 */
export class Nil extends Atomic {
  kind(): EXPR {
    return EXPR.NIL;
  }
  toString(): string {
    return "nil";
  }
  sexpr(): $EXPRESSION {
    return ["NIL", "nil"];
  }
  _value = "nil" as const;
  equals(other: Expression): boolean {
    return isNil(other);
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
 * Returns true if `u` is a Nil expression.
 */
export function isNil(u: any): u is Nil {
  return u instanceof Nil;
}

/**
 * The operators recognized as relation operators.
 */
type RelationOperator = "=" | "<" | ">" | "<=" | ">=" | "!=" | "~";

/** An object representing a relation. */
export class Relation extends Compound {
  kind(): EXPR {
    return EXPR.RELATION;
  }
  toString(): string {
    const result = this._args.map((y) => y.toString()).join(this._op);
    return result;
  }
  sexpr(): $EXPRESSION {
    return [this._op, this._args.map((arg) => arg.sexpr())];
  }
  _op: RelationOperator;
  _args: Expression[];
  constructor(op: RelationOperator, args: Expression[]) {
    super(op, args);
    this._op = op;
    this._args = args;
  }
}

/** Returns a new Relation expression. */
export function relate(op: RelationOperator, args: Expression[]) {
  return new Relation(op, args);
}

/**
 * An object representing an equation.
 */
export class Equation extends Compound {
  kind(): EXPR {
    return EXPR.EQUATION;
  }
  toString(): string {
    const left = this._left.toString();
    const right = this._right.toString();
    return `${left} = ${right}`;
  }
  sexpr(): $EXPRESSION {
    return ["=", [this._left.sexpr(), this._right.sexpr()]];
  }
  get _left() {
    return this._args[0];
  }
  get _right() {
    return this._args[1];
  }
  _args: [Expression, Expression];
  constructor(left: Expression, right: Expression) {
    super("=", [left, right]);
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
export class Sum extends Compound {
  kind(): EXPR {
    return EXPR.SUM;
  }
  toString(): string {
    const result = this._args.map((x) => x.toString()).join("+");
    return result;
  }
  sexpr(): $EXPRESSION {
    return ["+", this._args.map((x) => x.sexpr())];
  }
  _args: Expression[];
  constructor(args: Expression[]) {
    super("+", args);
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
export class Difference extends Compound {
  kind(): EXPR {
    return EXPR.DIFFERENCE;
  }
  toString(): string {
    const result = this._args.map((x) => x.toString()).join("-");
    return result;
  }
  sexpr(): $EXPRESSION {
    return ["-", this._args.map((x) => x.sexpr())];
  }
  _args: Expression[];
  constructor(args: Expression[]) {
    super("-", args);
    this._args = args;
  }
}

/**
 * Returns a new Difference expression.
 */
export function diff(...args: Expression[]) {
  return new Difference(args);
}

/**
 * An object representing a product expression.
 */
export class Product extends Compound {
  kind(): EXPR {
    return EXPR.PRODUCT;
  }
  toString(): string {
    const result = this._args.map((x) => x.toString()).join("*");
    return result;
  }
  sexpr(): $EXPRESSION {
    return ["*", this._args.map((x) => x.sexpr())];
  }
  _args: Expression[];
  constructor(args: Expression[]) {
    super("*", args);
    this._args = args;
  }
}

/** Returns a new Product expression. */
export function product(...args: Expression[]) {
  return new Product(args);
}

/**
 * An object representing a quotient.
 */
export class Quotient extends Compound {
  kind(): EXPR {
    return EXPR.QUOTIENT;
  }
  toString(): string {
    const result = this._args.map((x) => x.toString()).join("/");
    return result;
  }
  sexpr(): $EXPRESSION {
    return ["/", this._args.map((x) => x.sexpr())];
  }
  _args: Expression[];
  constructor(args: Expression[]) {
    super("/", args);
    this._args = args;
  }
}

/** Returns a new Quotient expression. */
export function quotient(...args: Expression[]) {
  return new Quotient(args);
}

/**
 * An object representing a power expression.
 */
export class Power extends Compound {
  kind(): EXPR {
    return EXPR.POWER;
  }
  toString(): string {
    const result = this._args.map((x) => x.toString()).join("^");
    return result;
  }
  sexpr(): $EXPRESSION {
    return ["^", this._args.map((x) => x.sexpr())];
  }
  _args: Expression[];
  constructor(base: Expression, exponent: Expression) {
    super("^", [base, exponent]);
    this._args = [base, exponent];
  }
}

/**
 * Returns a new Power expression.
 */
export function power(base: Expression, exponent: Expression) {
  return new Power(base, exponent);
}

/** An object corresponding to a function expression. */
export class Func extends Compound {
  kind(): EXPR {
    return EXPR.FUNC;
  }
  toString(): string {
    const args = this._args.map((x) => x.toString()).join(",");
    const result = `${this._op}(${args})`;
    return result;
  }
  sexpr(): $EXPRESSION {
    return ["𝑓", this._op, this._args.map((x) => x.sexpr())];
  }
  _op: string;
  _args: Expression[];
  constructor(op: string, args: Expression[]) {
    super(op, args);
    this._op = op;
    this._args = args;
  }
}

/** Returns a new function expression. */
export function func(op: string, args: Expression[]) {
  return new Func(op, args);
}

type CORE_FUNCTION_NAME = "%" | "div" | "rem";

/** An expression corresponding to a core function. */
export class CoreFn extends Compound {
  kind(): EXPR {
    return EXPR.CORE_FN;
  }
  toString(): string {
    return `${this._op}(${this._args.map((x) => x.toString()).join(",")})`;
  }
  sexpr(): $EXPRESSION {
    return ["𝑓", this._op, this._args.map((x) => x.sexpr())];
  }
  _op: CORE_FUNCTION_NAME;
  _args: Expression[];
  constructor(op: CORE_FUNCTION_NAME, args: Expression[]) {
    super(op, args);
    this._op = op;
    this._args = args;
  }
}

/** Returns a new core function object. */
function corefn(op: CORE_FUNCTION_NAME, args: Expression[]) {
  return new CoreFn(op, args);
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

  /** A token corresponding to `:=` (assigment) */
  COLON_EQUAL,

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

  // Vector Operators.
  // These are operators associated with
  // vectors.

  /** A token corresponding to `.+` */
  DOT_PLUS,
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

  /** A token corresponding to `*+` */
  STAR_PLUS,
  /** A token corresponding to `*-` */
  STAR_MINUS,
  /** A token corresponding to `**` */
  STAR_STAR,

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
  // BIG_FLOAT,
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
  LOOP,
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
  /** A token corresponding to the keyword "div". */
  DIV,
  /** A token corresponding to the keyword "begin". */
  BEGIN,
  /** A token corresponding to the keyword "end". */
  END,
  /** A token corresponding to some numeric constant. */
  NUMERIC_CONSTANT,
  /** A token corresponding to some native function name. */
  NATIVE_FUNCTION,
}

type Keyword =
  | "nan"
  | "inf"
  | "nil"
  | "undefined"
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
  | "div"
  | "begin"
  | "end"
  | "false"
  | "true";

type NumberTokenType =
  | TOKEN.INTEGER
  | TOKEN.BIG_INTEGER
  | TOKEN.FLOAT
  | TOKEN.FRACTION
  // | TOKEN.BIG_FLOAT;

/**
 * Represents a token.
 */
export class TokenObj {
  /** This token's lexeme. */
  _lexeme: string;

  /** The line where this token was found. */
  _line: number;

  /** The literal corresponding to this token's lexeme. */
  _literal: Expression = nil();

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

  isToken(token: TOKEN) {
    return this._token === token;
  }

  static empty: TokenObj = new TokenObj(TOKEN.EMPTY, "", -1);

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

export function lexical(code: string) {
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
    while: () => tkn(TOKEN.LOOP),
    for: () => tkn(TOKEN.FOR),
    let: () => tkn(TOKEN.LET),
    var: () => tkn(TOKEN.VAR),
    fn: () => tkn(TOKEN.FN),
    if: () => tkn(TOKEN.IF),
    else: () => tkn(TOKEN.ELSE),
    print: () => tkn(TOKEN.PRINT),
    rem: () => tkn(TOKEN.REM),
    div: () => tkn(TOKEN.DIV),
    nil: () => tkn(TOKEN.NIL),
    and: () => tkn(TOKEN.AND),
    or: () => tkn(TOKEN.OR),
    nor: () => tkn(TOKEN.NOR),
    xor: () => tkn(TOKEN.XOR),
    xnor: () => tkn(TOKEN.XNOR),
    not: () => tkn(TOKEN.NOT),
    nand: () => tkn(TOKEN.NAND),
    begin: () => tkn(TOKEN.BEGIN),
    end: () => tkn(TOKEN.END),
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
          return tkn(TOKEN.BIG_INTEGER).literal(bigint(BigInt(numstr)));
        } else {
          return tkn(TOKEN.INTEGER).literal(int(n));
        }
      }
      // handle scientific numbers
      // case TOKEN.BIG_FLOAT: {
      //   const [a, b] = numstr.split("E");
      //   const base = Number.parseFloat(a ?? "");
      //   const exponent = Number.parseInt(b ?? "");
      //   return tkn(TOKEN.BIG_FLOAT).literal(bigfloat(base, exponent));
      // }
      // handle floats
      case TOKEN.FLOAT: {
        const n = Number.parseFloat(numstr);
        // handle very big floats
        // if (n > Number.MAX_VALUE) {
        //   const exponential = n.toExponential();
        //   const [M, N] = exponential.split("E");
        //   const mantissa = Number.parseFloat(M ?? "0");
        //   const exponent = Number.parseFloat(N ?? "0");
        //   return tkn(TOKEN.BIG_FLOAT).literal(bigfloat(mantissa, exponent));
        // }
        return tkn(TOKEN.FLOAT).literal(float(n));
        
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

    // if (peekIs("E")) {
    //   if (isDigit(peekNext())) {
    //     type = TOKEN.BIG_FLOAT;
    //     tick();
    //     while (isDigit(peek())) {
    //       tick();
    //     }
    //   } else if (
    //     (peekNext() === "+" || peekNext() === "-") &&
    //     isDigit(lookup(2))
    //   ) {
    //     type = TOKEN.BIG_FLOAT;
    //     tick();
    //     tick();
    //     while (isDigit(peek())) {
    //       tick();
    //     }
    //   }
    // }
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
    return tkn(TOKEN.SYM_STRING).literal(symstring(s));
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
        return tkn(match("=") ? TOKEN.COLON_EQUAL : TOKEN.EQUAL);
      case "&":
        return tkn(TOKEN.AMPERSAND);
      case "~":
        return tkn(TOKEN.TILDE);
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
      case "*": {
        if (match("+")) {
          return tkn(TOKEN.STAR_PLUS);
        } else if (match("-")) {
          return tkn(TOKEN.STAR_MINUS);
        } else if (match("*")) {
          return tkn(TOKEN.STAR_STAR);
        } else {
          return tkn(TOKEN.STAR);
        }
      }
      case ";":
        return tkn(TOKEN.SEMICOLON);
      case "%":
        return tkn(TOKEN.PERCENT);
      case "/":
        return tkn(TOKEN.SLASH);
      case "^":
        return tkn(TOKEN.CARET);
      case "#":
        return tkn(TOKEN.POUND);
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
          return tkn(TOKEN.DOT_PLUS);
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

enum AST_NODE_TYPE {
  ASSIGNMENT,
  BINEX,
  CALL,
  PROP_READ,
  PROP_WRITE,
  PAREND,
  UNEX,
  BRANCH,
  TERNARY,
  LIT,
  BLOCK,
  EXPRESSION,
  CONDITIONAL,
  PRINT,
  FUNCTION_DECLARATION,
  VARIABLE_DECLARATION,
  LOOP,
  VARIABLE,
}

interface Visitor<T> {
  assignment(node: Assignment): T;
  binex(node: Binex): T;
  call(node: Call): T;
  propRead(node: PropRead): T;
  propWrite(node: PropWrite): T;
  parend(node: Parend): T;
  unex(node: Unex): T;
  branch(node: Branch): T;
  ternary(node: Ternary): T;
  variable(node: Variable): T;
  lit(node: Lit): T;
  BLOCK(node: BLOCK): T;
  EXPRESSION(node: EXPRESSION): T;
  CONDITIONAL(node: CONDITIONAL): T;
  PRINT(node: PRINT): T;
  FUNCTION_DECLARATION(node: FUNCTION_DECLARATION): T;
  VARIABLE_DECLARATION(node: VARIABLE_DECLARATION): T;
  LOOP(node: LOOP): T;
}

abstract class ASTNode {
  abstract accept<T>(visitor: Visitor<T>): T;
  abstract toString(): string;
  abstract isStatementNode(): this is StatementNode;
  abstract isExpressionNode(): this is ExpressionNode;
  abstract kind(): AST_NODE_TYPE;
}

abstract class StatementNode extends ASTNode {
  isStatementNode(): this is StatementNode {
    return true;
  }
  isExpressionNode(): this is ExpressionNode {
    return false;
  }
}

abstract class ExpressionNode extends ASTNode {
  isStatementNode(): this is StatementNode {
    return false;
  }
  isExpressionNode(): this is ExpressionNode {
    return true;
  }
}

/** An object representing an Assignment AST node. */
class Assignment extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.assignment(this);
  }
  toString(): string {
    const _name = this._name._lexeme;
    const _value = this._value.toString();
    return `${_name} = ${_value}`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.ASSIGNMENT;
  }
  _name: TokenObj;
  _value: ExpressionNode;
  constructor(name: TokenObj, value: ExpressionNode) {
    super();
    this._name = name;
    this._value = value;
  }
}

/** Returns a new Assignment node. */
function assignment(name: TokenObj, value: ExpressionNode) {
  return new Assignment(name, value);
}

/** An object corresponding to a binary expression node. */
class Binex extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.binex(this);
  }
  toString(): string {
    const left = this._left.toString();
    const op = this._op._lexeme;
    const right = this._right.toString();
    return `${left}${op}${right}`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.BINEX;
  }
  _left: ExpressionNode;
  _op: TokenObj;
  _right: ExpressionNode;
  constructor(left: ExpressionNode, op: TokenObj, right: ExpressionNode) {
    super();
    this._left = left;
    this._op = op;
    this._right = right;
  }
}

/** Returns a new Binex node. */
function binex(left: ExpressionNode, op: TokenObj, right: ExpressionNode) {
  return new Binex(left, op, right);
}

/**
 * An object representing a function call AST node.
 */
class Call extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.call(this);
  }
  toString(): string {
    throw new Error("Method not implemented.");
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.CALL;
  }
  _callee: ExpressionNode;
  _paren: TokenObj;
  _args: ExpressionNode[];
  constructor(callee: ExpressionNode, paren: TokenObj, args: ExpressionNode[]) {
    super();
    this._callee = callee;
    this._paren = paren;
    this._args = args;
  }
}

/** Returns a new Call node. */
function call(callee: ExpressionNode, paren: TokenObj, args: ExpressionNode[]) {
  return new Call(callee, paren, args);
}

/** An object representing a "property read" expression. */
class PropRead extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.propRead(this);
  }
  toString(): string {
    const obj = this._object.toString();
    const name = this._name._lexeme;
    return `${obj}.${name}`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.PROP_READ;
  }
  _object: ExpressionNode;
  _name: TokenObj;
  constructor(object: ExpressionNode, name: TokenObj) {
    super();
    this._object = object;
    this._name = name;
  }
}

/**
 * Returns a new "property read" expression node.
 */
function propRead(object: ExpressionNode, name: TokenObj) {
  return new PropRead(object, name);
}

/**
 * An object representing a "property write" expression.
 * @example
 * ~~~ts
 * circle.radius = 10;
 * ~~~
 */
class PropWrite extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.propWrite(this);
  }
  toString(): string {
    throw new Error("Method not implemented.");
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.PROP_WRITE;
  }
  _object: ExpressionNode;
  _name: TokenObj;
  _value: TokenObj;
  constructor(object: ExpressionNode, name: TokenObj, value: TokenObj) {
    super();
    this._object = object;
    this._name = name;
    this._value = value;
  }
}

/** Returns a new `property write` expression. */
export function propWrite(
  object: ExpressionNode,
  name: TokenObj,
  value: TokenObj
) {
  return new PropWrite(object, name, value);
}

/**
 * An object representing a parenthesized expression.
 */
class Parend extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.parend(this);
  }
  toString(): string {
    const expression = this._expression.toString();
    return `(${expression})`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.PAREND;
  }
  _expression: ExpressionNode;
  constructor(expression: ExpressionNode) {
    super();
    this._expression = expression;
  }
}

/** Returns a new parenthesized expression node. */
function parend(expression: ExpressionNode) {
  return new Parend(expression);
}

/** An object representing as unary expression. */
class Unex extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.unex(this);
  }
  toString(): string {
    const op = this._op._lexeme;
    const arg = this._arg.toString();
    if (this._fix) {
      return `(${arg})${op}`;
    } else {
      return `${op}(${arg})`;
    }
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.UNEX;
  }
  /**
   * A flag indicating whether this unary
   * expression is a unary prefix or unary postfix
   * expression.
   *
   * `0` = unary prefix.
   *
   * `1` = unary postfix.
   */
  _fix: 0 | 1;
  /** This unary expression's operator. */
  _op: TokenObj;
  /** This unary expression's argument. */
  _arg: ExpressionNode;
  constructor(operator: TokenObj, arg: ExpressionNode, fix: 0 | 1) {
    super();
    this._op = operator;
    this._arg = arg;
    this._fix = fix;
  }
}

/** Returns a new unary prefix expression node. */
function unaryPrefix(op: TokenObj, arg: ExpressionNode) {
  return new Unex(op, arg, 0);
}

/** Returns a new unary postfix expression node. */
function unaryPostfix(op: TokenObj, arg: ExpressionNode) {
  return new Unex(op, arg, 1);
}

/** An object representing a branch expression node. */
class Branch extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.branch(this);
  }
  toString(): string {
    const pairings = this._mappings
      .map(([a, b]) => {
        const condition = a.toString();
        const result = b.toString();
        return `${condition} -> ${result}`;
      })
      .join(",");
    return `branch(${pairings})`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.BRANCH;
  }
  _mappings: [ExpressionNode, ExpressionNode][];
  constructor(mappings: [ExpressionNode, ExpressionNode][]) {
    super();
    this._mappings = mappings;
  }
}

/** Returns a new branch expression node. */
function branch(mappings: [ExpressionNode, ExpressionNode][]) {
  return new Branch(mappings);
}

/** An object representing a ternary expression node. */
class Ternary extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.ternary(this);
  }
  toString(): string {
    throw new Error("Method not implemented.");
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.TERNARY;
  }
  _condition: ExpressionNode;
  _then: ExpressionNode;
  _else: ExpressionNode;
  constructor(
    condition: ExpressionNode,
    _then: ExpressionNode,
    _else: ExpressionNode
  ) {
    super();
    this._condition = condition;
    this._then = _then;
    this._else = _else;
  }
}

/** Returns a new ternary expression node. */
function ternary(
  condition: ExpressionNode,
  then: ExpressionNode,
  _else: ExpressionNode
) {
  return new Ternary(condition, then, _else);
}

/** An object representing a literal expression node. */
class Lit extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.lit(this);
  }
  toString(): string {
    return this._value.toString();
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.LIT;
  }
  _value: Expression;
  constructor(value: Expression) {
    super();
    this._value = value;
  }
}

/** Returns a new literal expression node. */
function lit(value: Expression) {
  return new Lit(value);
}

class Variable extends ExpressionNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.variable(this);
  }
  toString(): string {
    return this._symbol._lexeme;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.VARIABLE;
  }
  _symbol: TokenObj;
  constructor(symbol: TokenObj) {
    super();
    this._symbol = symbol;
  }
}

function variable(symbol: TokenObj) {
  return new Variable(symbol);
}

function isVariable(node: ExpressionNode): node is Variable {
  return node.kind() === AST_NODE_TYPE.VARIABLE;
}

// § Statements
// The following classes relate to AST nodes corresponding
// to statements.

/**
 * An object representing a Block
 * statement node.
 */
class BLOCK extends StatementNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.BLOCK(this);
  }
  toString(): string {
    const statements = this._statements.map((s) => s.toString());
    return `BLOCK: ${statements.join("\n")}\n`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.BLOCK;
  }
  _statements: StatementNode[];
  constructor(statements: StatementNode[]) {
    super();
    this._statements = statements;
  }
}

/** Returns a new Block statement node. */
export function block(statements: StatementNode[]) {
  return new BLOCK(statements);
}

/** An object representing an expression statement. */
class EXPRESSION extends StatementNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.EXPRESSION(this);
  }
  toString(): string {
    const expr = this._expression.toString();
    return `EXPRESSION: ${expr}\n`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.EXPRESSION;
  }
  _expression: ExpressionNode;
  constructor(expression: ExpressionNode) {
    super();
    this._expression = expression;
  }
}

/** Returns a new Expression statement node. */
function expression(expr: ExpressionNode) {
  return new EXPRESSION(expr);
}

/** An object representing a conditional statement node. */
class CONDITIONAL extends StatementNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.CONDITIONAL(this);
  }
  toString(): string {
    const condition = this._condition.toString();
    const thenBranch = this._then.toString();
    const elseBranch = this._else.toString();
    const a = `CONDITIONAL:\n`;
    const b = a + `\tCONDITION: ${condition}\n`;
    const c = b + `\tTHEN: ${thenBranch}\n`;
    const d = c + `\tELSE: ${elseBranch}\n`;
    return d;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.CONDITIONAL;
  }
  _condition: ExpressionNode;
  _then: StatementNode;
  _else: StatementNode;
  constructor(
    condition: ExpressionNode,
    thenBranch: StatementNode,
    elseBranch: StatementNode
  ) {
    super();
    this._condition = condition;
    this._then = thenBranch;
    this._else = elseBranch;
  }
}

/**
 * Returns a new conditional statement node.
 */
function conditional(
  condition: ExpressionNode,
  thenBranch: StatementNode,
  elseBranch: StatementNode
) {
  return new CONDITIONAL(condition, thenBranch, elseBranch);
}

/** An object representing a print statement. */
class PRINT extends StatementNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.PRINT(this);
  }
  toString(): string {
    return `PRINT: ${this._expression.toString()}\n`;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.PRINT;
  }

  _expression: ExpressionNode;
  constructor(expression: ExpressionNode) {
    super();
    this._expression = expression;
  }
}

/** Returns a new print statement node. */
export function print(expression: ExpressionNode) {
  return new PRINT(expression);
}

/**
 * An object representing a function declaration
 * statement node.
 */
class FUNCTION_DECLARATION extends StatementNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.FUNCTION_DECLARATION(this);
  }
  toString(): string {
    const a = `FUNCTION_DECLARATION:\n`;
    const b = a + `\tNAME: ${this._name._lexeme}\n`;
    const c =
      b + `\tPARAMS: (${this._params.map((t) => t._lexeme).join(",")})\n`;
    const d = c + `\tBODY: ${this._body.map((s) => s.toString()).join("\n")}`;
    return d;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.FUNCTION_DECLARATION;
  }
  _name: TokenObj;
  _params: TokenObj[];
  _body: StatementNode[];
  constructor(name: TokenObj, params: TokenObj[], body: StatementNode[]) {
    super();
    this._name = name;
    this._params = params;
    this._body = body;
  }
}

/**
 * Returns a new function declaration statement node.
 */
function functionDeclaration(
  name: TokenObj,
  params: TokenObj[],
  body: StatementNode[]
) {
  return new FUNCTION_DECLARATION(name, params, body);
}

/** An object representing a variable declaration statement node. */
class VARIABLE_DECLARATION extends StatementNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.VARIABLE_DECLARATION(this);
  }
  toString(): string {
    const name = this._name._lexeme;
    const init = this._init.toString();
    const out = `VARIABLE_DECLARATION: ${name} = ${init}\n`;
    return out;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.VARIABLE_DECLARATION;
  }
  _name: TokenObj;
  _init: ExpressionNode;
  constructor(name: TokenObj, init: ExpressionNode) {
    super();
    this._name = name;
    this._init = init;
  }
}

/**
 * Returns a new variable declaration statement node.
 */
function varDeclaration(name: TokenObj, init: ExpressionNode) {
  return new VARIABLE_DECLARATION(name, init);
}

/** An object representing a loop statement node. */
class LOOP extends StatementNode {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.LOOP(this);
  }
  toString(): string {
    const a = `LOOP:\n`;
    const b = a + `\tCONDITION:${this._condition.toString()}\n`;
    const c = b + `\tBODY:${this._body.toString()}\n`;
    return c;
  }
  kind(): AST_NODE_TYPE {
    return AST_NODE_TYPE.LOOP;
  }
  _condition: ExpressionNode;
  _body: StatementNode;
  constructor(condition: ExpressionNode, body: StatementNode) {
    super();
    this._condition = condition;
    this._body = body;
  }
}

/** Returns a new loop statement node. */
function loop(condition: ExpressionNode, body: StatementNode) {
  return new LOOP(condition, body);
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

class ParserState<STMT extends ASTNode, EXPR extends ASTNode> {
  _error: ERROR | null = null;
  _prev: TokenObj = TokenObj.empty;
  _cursor: number = -1;
  _peek: TokenObj = TokenObj.empty;
  _current: TokenObj = TokenObj.empty;
  _lastExpression: EXPR;
  _currentExpression: EXPR;
  _lastStatement: AST_NODE_TYPE;
  _currentStatement: AST_NODE_TYPE;
  constructor(nilExpression: EXPR, emptyStatement: STMT) {
    this._lastExpression = nilExpression;
    this._currentExpression = nilExpression;
    this._lastStatement = emptyStatement.kind();
    this._currentStatement = emptyStatement.kind();
  }
  private _lexer!: ReturnType<typeof lexical>;
  init(source: string) {
    this._lexer = lexical(source);
    this.next();
    return this;
  }
  /** Returns true if an implicit semicolon is permissible. */
  implicitSemicolonAllowed() {
    return this._peek.isToken(TOKEN.EOF) || this.atEnd();
  }

  /** Returns a new expression (in a RIGHT monad). */
  newExpression<E extends EXPR>(expression: E) {
    const prev = this._currentExpression;
    this._currentExpression = expression;
    this._lastExpression = prev;
    return right(expression);
  }

  /** Returns a new statement (in a RIGHT monad). */
  newStatement<S extends STMT>(statement: S) {
    const prev = this._currentStatement;
    this._currentStatement = statement.kind();
    this._lastStatement = prev;
    return right(statement);
  }

  /** Moves the parser state forward. */
  next(): TokenObj {
    this._cursor++;
    this._current = this._peek;
    const nextToken = this._lexer.scan();
    if (nextToken.isToken(TOKEN.ERROR)) {
      this._error = nextToken._literal as unknown as ERROR;
      return token(TOKEN.EOF, "EOF", -1);
    }
    this._peek = nextToken;
    return this._current;
  }

  /** Returns true if there is nothing left to parse. */
  atEnd() {
    return this._peek.isToken(TOKEN.EOF) || this._error !== null;
  }

  /** Returns a new error in a LEFT monad. */
  error(message: string, line: number) {
    const e = syntaxError(message, line);
    this._error = e;
    return left(e);
  }

  /** Returns true if the current token is of the given TOKEN. */
  check(token: TOKEN) {
    if (this.atEnd()) {
      return false;
    }
    return this._peek.isToken(token);
  }

  /**
   * Returns true and moves the parser forward if the next
   * token matches the provided TOKEN.
   */
  nextIs(token: TOKEN) {
    if (this._peek.isToken(token)) {
      this.next();
      return true;
    } else {
      return false;
    }
  }
}

/**
 * A function that returns a new parser state.
 * @param nilExpr A nil expression to serve as a placeholder expression.
 * @param emptyStmt An empty statement to serve as a placeholder statement.
 * @returns A new Parser State.
 */
function enstate<EXPR extends ASTNode, STMT extends ASTNode>(
  nilExpression: EXPR,
  emptyStatement: STMT
) {
  return new ParserState(nilExpression, emptyStatement);
}

/** @internal A Pratt parsing function. */
type Parslet<T> = (current: TokenObj, lastNode: T) => Either<ERROR, T>;

/** @internal An entry within parser’s BP table. The first element is a prefix parslet, the second element is an infix parslet, and the last element is the binding power of the operator. */
type ParsletEntry<T> = [Parslet<T>, Parslet<T>, BP];

/** @internal A record of parslet entries, where each key is a TOKEN. */
type BPTable<T> = Record<TOKEN, ParsletEntry<T>>;

export function syntaxAnalysis(source: string) {
  /** Begin by initializing the state. */
  const state = enstate<ExpressionNode, StatementNode>(
    lit(nil()),
    expression(lit(nil()))
  ).init(source);

  /** The "blank" binding power. */
  const ___o = BP.NIL;

  /** The "blank" parslet. */
  const ____: Parslet<ExpressionNode> = (token) => {
    if (state._error !== null) {
      return left(state._error);
    } else {
      return state.error(`Unexpected token: ${token._lexeme}`, token._line);
    }
  };

  /** Parses an infix expression. */
  const infix = (op: TokenObj, lhs: ExpressionNode) => {
    // Detour to handling complex assignments
    // E.g., x += 1
    if (state.nextIs(TOKEN.EQUAL)) {
      if (isVariable(lhs)) {
        const name = lhs;
        const r = expr();
        if (r.isLeft()) return r;
        const rhs = r.unwrap();
        const value = binex(lhs, op, rhs);
        return state.newExpression(assignment(name._symbol, value));
      } else {
        return state.error(
          `Invalid lefthand side of assignment. Expected a variable to the left of "${op._lexeme}" but got "${lhs.toString()}".`,
          op._line
        );
      }
    }
    // now handle infix expressions
    const p = precof(op._token);
    const r = expr(p);
    if (r.isLeft()) return r;
    const rhs = r.unwrap();
    return state.newExpression(binex(lhs, op, rhs));
  };

  /** Parses a literal expression. */
  const literal = (t: TokenObj) => {
    switch (t._token) {
      case TOKEN.UNDEFINED:
      case TOKEN.NIL:
      case TOKEN.INF:
      case TOKEN.NAN:
      case TOKEN.BIG_INTEGER:
      case TOKEN.FLOAT:
      case TOKEN.INTEGER:
      case TOKEN.FRACTION:
      // case TOKEN.BIG_FLOAT:
      case TOKEN.BIG_FRACTION:
      case TOKEN.STRING:
      case TOKEN.BOOLEAN:
        return state.newExpression(lit(t._literal));
      default: {
        return state.error(
          `Expected a literal, but got: ${t._lexeme}`,
          t._line
        );
      }
    }
  };

  /** Parses a parenthesized expression. */
  const primary = () => {
    const innerExpression = expr();
    if (innerExpression.isLeft()) {
      return innerExpression;
    }
    if (!state.nextIs(TOKEN.RIGHT_PAREN)) {
      return state.error(`Expected a closing ")"`, state._current._line);
    }
    return innerExpression.map((e) => parend(e));
  };

  /**
   * The rules table comprises mappings from every
   * token type to a triple `(Prefix, Infix, B)`,
   * where `Prefix` and `Infix` are parslets (small
   * parsers that handle a single grammar rule), and `B` is a
   * binding power.
   */
  const rules: BPTable<ExpressionNode> = {
    [TOKEN.EOF]: [____, ____, ___o],
    [TOKEN.ERROR]: [____, ____, ___o],
    [TOKEN.EMPTY]: [____, ____, ___o],
    [TOKEN.LEFT_PAREN]: [primary, ____, BP.CALL],
    [TOKEN.RIGHT_PAREN]: [____, ____, ___o],
    [TOKEN.LEFT_BRACKET]: [____, ____, ___o],
    [TOKEN.RIGHT_BRACKET]: [____, ____, ___o],
    [TOKEN.LEFT_BRACE]: [____, ____, ___o],
    [TOKEN.RIGHT_BRACE]: [____, ____, ___o],
    [TOKEN.SEMICOLON]: [____, ____, ___o],
    [TOKEN.COLON]: [____, ____, ___o],
    [TOKEN.DOT]: [____, ____, ___o],
    [TOKEN.COMMA]: [____, ____, ___o],

    // infix operations - arithmetic
    [TOKEN.PLUS]: [____, infix, BP.SUM],
    [TOKEN.MINUS]: [____, infix, BP.DIFFERENCE],
    [TOKEN.STAR]: [____, infix, BP.PRODUCT],
    [TOKEN.SLASH]: [____, infix, BP.QUOTIENT],
    [TOKEN.CARET]: [____, infix, BP.POWER],
    [TOKEN.PERCENT]: [____, infix, BP.QUOTIENT],
    [TOKEN.DIV]: [____, infix, BP.QUOTIENT],
    [TOKEN.REM]: [____, infix, BP.QUOTIENT],

    // infix operations - relations
    [TOKEN.EQUAL_EQUAL]: [____, infix, BP.EQ],
    [TOKEN.LESS]: [____, infix, BP.REL],
    [TOKEN.GREATER]: [____, infix, BP.REL],
    [TOKEN.LESS_EQUAL]: [____, infix, BP.REL],
    [TOKEN.GREATER_EQUAL]: [____, infix, BP.REL],
    [TOKEN.BANG_EQUAL]: [____, infix, BP.REL],
    [TOKEN.TILDE]: [____, infix, BP.REL],

    // infix operation - string concatenation
    [TOKEN.AMPERSAND]: [____, infix, BP.STRINGOP],

    // equation operation
    [TOKEN.EQUAL]: [____, infix, BP.REL],

    // assignment operation
    [TOKEN.COLON_EQUAL]: [____, ____, ___o],

    // literals
    [TOKEN.INTEGER]: [literal, ____, BP.ATOM],
    [TOKEN.BIG_INTEGER]: [literal, ____, BP.ATOM],
    [TOKEN.FLOAT]: [literal, ____, BP.ATOM],
    [TOKEN.FRACTION]: [literal, ____, BP.ATOM],
    // [TOKEN.BIG_FLOAT]: [literal, ____, BP.ATOM],
    [TOKEN.BIG_FRACTION]: [literal, ____, BP.ATOM],
    [TOKEN.STRING]: [literal, ____, BP.ATOM],
    [TOKEN.SYM_STRING]: [____, ____, ___o],
    [TOKEN.BOOLEAN]: [literal, ____, BP.ATOM],
    [TOKEN.NAN]: [literal, ____, BP.ATOM],
    [TOKEN.INF]: [literal, ____, BP.ATOM],
    [TOKEN.NIL]: [literal, ____, BP.ATOM],
    [TOKEN.UNDEFINED]: [literal, ____, BP.ATOM],

    [TOKEN.BANG]: [____, ____, ___o],

    [TOKEN.PLUS_PLUS]: [____, ____, ___o],
    [TOKEN.MINUS_MINUS]: [____, ____, ___o],
    [TOKEN.DOT_PLUS]: [____, ____, ___o],
    [TOKEN.DOT_STAR]: [____, ____, ___o],
    [TOKEN.DOT_MINUS]: [____, ____, ___o],
    [TOKEN.DOT_CARET]: [____, ____, ___o],
    [TOKEN.AT]: [____, ____, ___o],
    [TOKEN.POUND]: [____, ____, ___o],
    [TOKEN.STAR_PLUS]: [____, ____, ___o],
    [TOKEN.STAR_MINUS]: [____, ____, ___o],
    [TOKEN.STAR_STAR]: [____, ____, ___o],

    [TOKEN.SYMBOL]: [____, ____, ___o],

    [TOKEN.AND]: [____, ____, ___o],
    [TOKEN.OR]: [____, ____, ___o],
    [TOKEN.NOT]: [____, ____, ___o],
    [TOKEN.NAND]: [____, ____, ___o],
    [TOKEN.XOR]: [____, ____, ___o],
    [TOKEN.XNOR]: [____, ____, ___o],
    [TOKEN.NOR]: [____, ____, ___o],
    [TOKEN.IF]: [____, ____, ___o],
    [TOKEN.ELSE]: [____, ____, ___o],
    [TOKEN.FN]: [____, ____, ___o],
    [TOKEN.LET]: [____, ____, ___o],
    [TOKEN.VAR]: [____, ____, ___o],
    [TOKEN.RETURN]: [____, ____, ___o],
    [TOKEN.LOOP]: [____, ____, ___o],
    [TOKEN.FOR]: [____, ____, ___o],
    [TOKEN.CLASS]: [____, ____, ___o],
    [TOKEN.PRINT]: [____, ____, ___o],
    [TOKEN.SUPER]: [____, ____, ___o],
    [TOKEN.THIS]: [____, ____, ___o],
    [TOKEN.BEGIN]: [____, ____, ___o],
    [TOKEN.END]: [____, ____, ___o],
    [TOKEN.NUMERIC_CONSTANT]: [____, ____, ___o],
    [TOKEN.NATIVE_FUNCTION]: [____, ____, ___o],
  };

  /**
   * Returns the prefix parsing rule mapped to by the given
   * token type.
   */
  const prefixRule = (t: TOKEN): Parslet<ExpressionNode> => rules[t][0];

  /**
   * Returns the infix parsing rule mapped to by the given
   * token type.
   */
  const infixRule = (t: TOKEN): Parslet<ExpressionNode> => rules[t][1];

  /**
   * Returns the {@link bp|precedence} of the given token type.
   */
  const precof = (t: TOKEN): BP => rules[t][2];

  const expr = (minBP: number = BP.LOWEST): Either<ERROR, ExpressionNode> => {
    let token = state.next();
    const pre = prefixRule(token._token);
    let lhs = pre(token, lit(nil()));
    if (lhs.isLeft()) {
      return lhs;
    }
    while (minBP < precof(state._peek._token)) {
      token = state.next();
      const r = infixRule(token._token);
      const rhs = r(token, lhs.unwrap());
      if (rhs.isLeft()) {
        return rhs;
      }
      lhs = rhs;
    }
    return lhs;
  };

  return {
    /** Returns a syntax analysis of a single expression. */
    parsex(): Either<ERROR, ExpressionNode> {
      if (state._error !== null) {
        return left(state._error);
      } else {
        const out = expr();
        return out;
      }
    },
  };
}

class RuntimeError extends ERROR {
  constructor(message: string) {
    super(message);
  }
}

function runtimeError(message: string) {
  return new RuntimeError(message);
}

class Interpreter implements Visitor<Expression> {
  evaluate(expr: ExpressionNode) {
    return expr.accept(this);
  }
  interpret(expr: ExpressionNode) {
    try {
      const value = this.evaluate(expr);
      return right(value);
    } catch (error: unknown) {
      const e = error as ERROR;
      return left(e);
    }
  }
  assignment(node: Assignment): Expression {
    throw new Error("Method not implemented.");
  }
  binex(node: Binex): Expression {
    const op = node._op._token;
    const left = this.evaluate(node._left);
    const right = this.evaluate(node._right);
    switch (op) {
      case TOKEN.PLUS:
        return sum(left, right);
      case TOKEN.MINUS:
        return diff(left, right);
      case TOKEN.STAR:
        return product(left, right);
      case TOKEN.SLASH:
        return quotient(left, right);
      case TOKEN.CARET:
        return power(left, right);
      case TOKEN.PERCENT:
        return corefn("%", [left, right]);
      case TOKEN.DIV:
        return corefn("div", [left, right]);
      case TOKEN.REM:
        return corefn("rem", [left, right]);
      case TOKEN.EQUAL_EQUAL:
        return bool(left.equals(right));
      case TOKEN.LESS:
        return relate("<", [left, right]);
      case TOKEN.GREATER:
        return relate(">", [left, right]);
      case TOKEN.LESS_EQUAL:
        return relate("<=", [left, right]);
      case TOKEN.GREATER_EQUAL:
        return relate(">=", [left, right]);
      case TOKEN.BANG_EQUAL:
        return relate("!=", [left, right]);
      case TOKEN.TILDE:
        return relate("~", [left, right]);
      case TOKEN.EQUAL:
        return equate(left, right);
      case TOKEN.AMPERSAND: {
        const l = left.toString();
        const r = right.toString();
        return str(l + r);
      }
    }
    throw runtimeError(
      `While interpreting a binex, encountered an unknown operator: "${node._op._lexeme}".`
    );
  }
  call(node: Call): Expression {
    throw new Error("Method not implemented.");
  }
  propRead(node: PropRead): Expression {
    throw new Error("Method not implemented.");
  }
  propWrite(node: PropWrite): Expression {
    throw new Error("Method not implemented.");
  }
  parend(node: Parend): Expression {
    throw new Error("Method not implemented.");
  }
  unex(node: Unex): Expression {
    throw new Error("Method not implemented.");
  }
  branch(node: Branch): Expression {
    throw new Error("Method not implemented.");
  }
  ternary(node: Ternary): Expression {
    throw new Error("Method not implemented.");
  }
  variable(node: Variable): Expression {
    throw new Error("Method not implemented.");
  }
  lit(node: Lit): Expression {
    return node._value;
  }
  BLOCK(node: BLOCK): Expression {
    throw new Error("Method not implemented.");
  }
  EXPRESSION(node: EXPRESSION): Expression {
    throw new Error("Method not implemented.");
  }
  CONDITIONAL(node: CONDITIONAL): Expression {
    throw new Error("Method not implemented.");
  }
  PRINT(node: PRINT): Expression {
    throw new Error("Method not implemented.");
  }
  FUNCTION_DECLARATION(node: FUNCTION_DECLARATION): Expression {
    throw new Error("Method not implemented.");
  }
  VARIABLE_DECLARATION(node: VARIABLE_DECLARATION): Expression {
    throw new Error("Method not implemented.");
  }
  LOOP(node: LOOP): Expression {
    throw new Error("Method not implemented.");
  }
}

export function isERROR(x: any): x is ERROR {
  return x instanceof ERROR;
}

export function isExpression(x: any): x is Expression {
  return x instanceof Expression;
}

export function interpret(code: string) {
  const interpreter = new Interpreter();
  const s = syntaxAnalysis(code).parsex();
  if (s.isLeft()) {
    return s.unwrap();
  } else {
    const result = interpreter.interpret(s.unwrap());
    return result.unwrap();
  }
}
