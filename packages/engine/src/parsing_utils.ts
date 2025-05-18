/**
 * Returns true if the given character is a Latin or
 * Greek character, false otherwise.
 * @param char A 1-character string.
 * @returns A boolean.
 */
export function isLatinGreekChar(char: string): boolean {
  return /^[a-zA-Z_$\u00C0-\u02AF\u0370-\u03FF\u2100-\u214F]$/.test(char);
}

/**
 * Returns true if the given character is a Unicode
 * math symbol, false otherwise.
 * @param char A 1-character string.
 * @returns A boolean.
 */
export function isMathSymbol(char: string): boolean {
  return /^[∀-⋿]/u.test(char);
}

/**
 * Returns true if the given character is a valid
 * character to the start of a name, false
 * otherwise.
 * @param char A 1-char string.
 * @returns A boolean.
 */
export function isValidNameChar(char: string) {
  return isLatinGreekChar(char) || isMathSymbol(char);
}

/**
 * Returns true if the given `char` is a digit.
 * @param char A 1-char string.
 * @returns A boolean.
 */
export function isDigit(char: string) {
  return "0" <= char && char <= "9";
}

/**
 * Returns true if the given character is
 * a hexadecimal digit, false otherwise.
 * 
 * @param char a 1-character string.
 * 
 * @returns a Boolean.
 */
export function isHexDigit(char: string) {
  return (
    ("0" <= char && char <= "9") ||
    ("a" <= char && char <= "f") ||
    ("A" <= char && char <= "F")
  );
}

/**
 * Returns true if the given character is an
 * octal digit, false otherwise.
 * 
 * @param char a 1-character string.
 * 
 * @returns a Boolean.
 */
export function isOctalDigit(char: string) {
  return "0" <= char && char <= "7";
}
