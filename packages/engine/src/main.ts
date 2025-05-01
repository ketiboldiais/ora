enum EXPR {
	INTEGER,
	BIG_INTEGER,
	REAL,
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

/** An object corresponding to a floating point number. */
export class Real extends Expression {
	kind(): EXPR {
		return EXPR.REAL;
	}
	/** The value of this real number. */
	_value: number;
	constructor(value: number) {
		super();
		this._value = value;
	}
}

/** Returns a new Real. */
export function real(value: number) {
	return new Real(value);
}


