/* eslint-disable @typescript-eslint/no-explicit-any */

/** Returns a pretty-print tree of the given Object `Obj`. */
export function treestring<T extends object>(
  Obj: T,
  cbfn?: (node: any) => void
) {
  const prefix = (key: keyof T, last: boolean) => {
    let str = last ? "└" : "├";
    if (key) str += "─ ";
    else str += "──┐";
    return str;
  };
  const getKeys = (obj: T) => {
    const keys: (keyof T)[] = [];
    for (const branch in obj) {
      if (
        !Object.prototype.hasOwnProperty.call(obj, branch) ||
        typeof obj[branch] === "function"
      ) {
        continue;
      }
      keys.push(branch);
    }
    return keys;
  };
  const grow = (
    key: keyof T,
    root: any,
    last: boolean,
    prevstack: [T, boolean][],
    cb: (str: string) => any
  ) => {
    if (cbfn) {
      cbfn(root);
    }
    let line = "";
    let index = 0;
    let lastKey = false;
    let circ = false;
    const stack = prevstack.slice(0);
    if (stack.push([root, last]) && stack.length > 0) {
      prevstack.forEach(function (lastState, idx) {
        if (idx > 0) line += (lastState[1] ? " " : "│") + "  ";
        if (!circ && lastState[0] === root) circ = true;
      });
      line += prefix(key, last) + key.toString();
      if (typeof root !== "object") line += ": " + root;
      if (circ) {
        line += " (circular ref.)";
      }
      cb(line);
    }
    if (!circ && typeof root === "object") {
      const keys = getKeys(root);
      keys.forEach((branch) => {
        lastKey = ++index === keys.length;
        grow(branch, root[branch], lastKey, stack, cb);
      });
    }
  };
  let output = "";
  const obj = Object.assign({}, Obj);
  grow(
    "." as keyof T,
    obj,
    false,
    [],
    (line: string) => (output += line + "\n")
  );
  return output;
}
