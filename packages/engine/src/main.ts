/**
 * An object corresponding to an expression.
 */
abstract class Expression {}

/**
 * An object corresponding to a floating
 * point value.
 */
class Float extends Expression {
	value: number;
	constructor(value: number) {
		super();
		this.value = value;
	}
}

/** Returns a new float expression. */
export function float(value: number) {
	return new Float(value);
}

/**
 * An object corresponding to an integer.
 */
class Integer extends Expression {
  value: number;
  constructor(n: number) {
    super();
    this.value = Math.floor(n);
  }
}

type AlgebraicOperator = '+' | '-' | '*' | '/';

/**
 * An object representing an expression 
 * corresponding to an algebraic
 * operation.
 */
export class AlgebraicOp extends Expression {
	op: AlgebraicOperator
	args: Expression[];
	constructor(op: AlgebraicOperator, args: Expression[]) {
		super();
		this.op = op;
		this.args = args;
	}
}

/** An object corresponding to a sum. */
export class Sum extends AlgebraicOp {
	constructor(args: Expression[]) {
		super('+', args);
	}
}

/** Returns a new sum. */
export function sum(args: Expression[]) {
	return new Sum(args);
}

class RelationOp {}

class LogicOp {}

class SetOp {}

/**
 * Returns a new Integer object.
 */
export function int(value: number) {
  return new Integer(value);
}
