export type Either<A, B> = Left<A> | Right<B>;

/** An object corresponding to failure. */
export class Left<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map(): Either<T, never> {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return this as any;
  }
  isLeft(): this is Left<T> {
    return true;
  }
  isRight(): this is never {
    return false;
  }
  unwrap() {
    return this.value;
  }
  chain(): Left<T> {
    return this;
  }
}

/** Returns a new Left with value `x` of type `T`. */
export function left<T>(x: T) {
  return new Left(x);
}

/** An object corresponding to success. */
export class Right<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<X>(f: (x: T) => X): Either<never, X> {
    return new Right(f(this.value));
  }
  isLeft(): this is never {
    return false;
  }
  isRight(): this is Right<T> {
    return true;
  }
  unwrap() {
    return this.value;
  }
  chain<N, X>(f: (x: T) => Either<N, X>): Either<never, X> {
    return f(this.value) as Either<never, X>;
  }
}

/** Returns a new Right with value `x` of type `T`. */
export function right<T>(x: T) {
  return new Right(x);
}

export class None {
  constructor() {}
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  map(f: (x: never) => unknown): None {
    return this;
  }
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  chain(f: (x: never) => unknown) {
    return this;
  }
}

export class Some<T> {
  readonly _value: T;
  constructor(x: T) {
    this._value = x;
  }
  map<S>(f: (x: T) => S): Some<S> {
    return new Some(f(this._value));
  }
  chain<S>(f: (x: T) => Option<S>) {
    return f(this._value);
  }
}

export type Option<T> = None | Some<T>;

export function option<T>(x: T): Option<NonNullable<T>> {
  return x === null || x === undefined
    ? new Some(x as NonNullable<T>)
    : new None();
}
